// src/main.rs
// This is a simplified implementation of the 'compiler' binary for vib-lang in Rust.
// It uses the Inkwell crate for LLVM integration to compile vib code to native binaries.
// For VM compilation, it generates a custom bytecode (.object files).
// The compiler parses vib syntax, which is inspired by Python/Ruby/JS with custom elements.
// Features:
// - Lexer and Parser for vib syntax.
// - Codegen to LLVM IR for native binaries.
// - Codegen to bytecode for VM (.object).
// - Handles imports (:biblioteka:), memory management (<zarzadznie pamiecia>), write statements.
// - Embedded code blocks #=lang={ code } translated to calls or inlined.
// - Blocks use [] instead of {} or indentation (for simplicity, we'll parse with [] for blocks).
// Note: This is highly simplified; a real compiler would be much more complex.

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process;

// Tokens for lexer
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Colon,          // :
    LBracket,       // [
    RBracket,       // ]
    LAngle,         // <
    RAngle,         // >
    Hash,           // #
    Equal,          // =
    LBrace,         // {
    RBrace,         // }
    Write,          // write keyword
    Import,         // :biblioteka: parsed as import
    MemoryMode(String), // <zarzadznie pamiecia> automatic or manual
    EmbeddedStart(String), // #=lang=
    EmbeddedEnd,
    Plus, Minus, Star, Slash,
    EOF,
}

// Lexer
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    chars: Vec<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            chars: input.chars().collect(),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.pos >= self.chars.len() {
            return Token::EOF;
        }
        let ch = self.chars[self.pos];
        match ch {
            ':' => {
                self.pos += 1;
                if self.match_str("biblioteka:") {
                    Token::Import
                } else {
                    Token::Colon
                }
            }
            '<' => {
                self.pos += 1;
                if self.match_str("zarzadznie pamiecia>") {
                    // Parse mode
                    self.skip_whitespace();
                    let mode = self.read_identifier();
                    Token::MemoryMode(mode)
                } else {
                    Token::LAngle
                }
            }
            '>' => { self.pos += 1; Token::RAngle }
            '[' => { self.pos += 1; Token::LBracket }
            ']' => { self.pos += 1; Token::RBracket }
            '#' => {
                self.pos += 1;
                if self.current_char() == Some('=') {
                    self.pos += 1;
                    let lang = self.read_identifier();
                    if self.current_char() == Some('=') {
                        self.pos += 1;
                        return Token::EmbeddedStart(lang);
                    }
                }
                Token::Hash
            }
            '=' => { self.pos += 1; Token::Equal }
            '{' => { self.pos += 1; Token::LBrace }
            '}' => { self.pos += 1; Token::EmbeddedEnd } // For embedded
            '+' => { self.pos += 1; Token::Plus }
            '-' => { self.pos += 1; Token::Minus }
            '*' => { self.pos += 1; Token::Star }
            '/' => { self.pos += 1; Token::Slash }
            '"' => self.read_string(),
            '0'..='9' => self.read_number(),
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_identifier();
                if ident == "write" {
                    Token::Write
                } else {
                    Token::Identifier(ident)
                }
            }
            _ => {
                self.pos += 1;
                Token::EOF // Unknown, skip
            }
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.pos < self.chars.len() {
            Some(self.chars[self.pos])
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() && self.chars[self.pos].is_whitespace() {
            self.pos += 1;
        }
    }

    fn match_str(&mut self, s: &str) -> bool {
        let start = self.pos;
        for c in s.chars() {
            if self.current_char() != Some(c) {
                self.pos = start;
                return false;
            }
            self.pos += 1;
        }
        true
    }

    fn read_identifier(&mut self) -> String {
        let start = self.pos;
        while self.pos < self.chars.len() && (self.chars[self.pos].is_alphanumeric() || self.chars[self.pos] == '_') {
            self.pos += 1;
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.chars.len() && self.chars[self.pos].is_digit(10) {
            self.pos += 1;
        }
        if self.pos < self.chars.len() && self.chars[self.pos] == '.' {
            self.pos += 1;
            while self.pos < self.chars.len() && self.chars[self.pos].is_digit(10) {
                self.pos += 1;
            }
            let num: f64 = self.input[start..self.pos].parse().unwrap();
            Token::Float(num)
        } else {
            let num: i64 = self.input[start..self.pos].parse().unwrap();
            Token::Integer(num)
        }
    }

    fn read_string(&mut self) -> Token {
        self.pos += 1; // Skip "
        let start = self.pos;
        while self.pos < self.chars.len() && self.chars[self.pos] != '"' {
            self.pos += 1;
        }
        let s = self.chars[start..self.pos].iter().collect();
        self.pos += 1; // Skip closing "
        Token::String(s)
    }
}

// AST Nodes
#[derive(Debug)]
enum AstNode {
    Import(String),
    MemoryMode(String),
    Write(Box<AstNode>),
    BinaryOp(char, Box<AstNode>, Box<AstNode>),
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Block(Vec<AstNode>),
    Embedded(String, String), // lang, code
}

// Parser
struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token();
        Parser { lexer, current }
    }

    fn parse(&mut self) -> AstNode {
        let mut statements = Vec::new();
        while self.current != Token::EOF {
            let stmt = self.parse_statement();
            statements.push(stmt);
            self.advance();
        }
        AstNode::Block(statements)
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> AstNode {
        match &self.current {
            Token::Import => {
                self.advance();
                if let Token::Identifier(lib) = &self.current {
                    AstNode::Import(lib.clone())
                } else {
                    panic!("Expected identifier after import");
                }
            }
            Token::MemoryMode(mode) => AstNode::MemoryMode(mode.clone()),
            Token::Write => {
                self.advance();
                let expr = self.parse_expr();
                AstNode::Write(Box::new(expr))
            }
            Token::EmbeddedStart(lang) => {
                let lang = lang.clone();
                self.advance();
                if self.current != Token::LBrace {
                    panic!("Expected { after #=lang=");
                }
                self.advance();
                let start = self.lexer.pos;
                while self.current != Token::EmbeddedEnd {
                    self.advance();
                }
                let code = &self.lexer.input[start..self.lexer.pos - 1]; // Exclude }
                AstNode::Embedded(lang, code.to_string())
            }
            Token::LBracket => {
                self.advance();
                let mut block = Vec::new();
                while self.current != Token::RBracket && self.current != Token::EOF {
                    block.push(self.parse_statement());
                }
                if self.current == Token::RBracket {
                    self.advance();
                }
                AstNode::Block(block)
            }
            _ => self.parse_expr(),
        }
    }

    fn parse_expr(&mut self) -> AstNode {
        let mut left = self.parse_primary();
        while matches!(&self.current, Token::Plus | Token::Minus | Token::Star | Token::Slash) {
            let op = match self.current {
                Token::Plus => '+',
                Token::Minus => '-',
                Token::Star => '*',
                Token::Slash => '/',
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_primary();
            left = AstNode::BinaryOp(op, Box::new(left), Box::new(right));
        }
        left
    }

    fn parse_primary(&mut self) -> AstNode {
        match &self.current {
            Token::Integer(i) => AstNode::Integer(*i),
            Token::Float(f) => AstNode::Float(*f),
            Token::String(s) => AstNode::String(s.clone()),
            Token::Identifier(id) => AstNode::Identifier(id.clone()),
            _ => panic!("Unexpected token: {:?}", self.current),
        }
    }
}

// Codegen for LLVM
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    memory_mode: String, // "manual" or "automatic"
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            memory_mode: "manual".to_string(),
        }
    }

    fn gen_code(&mut self, node: &AstNode) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Block(stmts) => {
                let mut last = None;
                for stmt in stmts {
                    last = self.gen_code(stmt);
                }
                last
            }
            AstNode::Import(lib) => {
                // For simplicity, ignore or link libraries
                None
            }
            AstNode::MemoryMode(mode) => {
                self.memory_mode = mode.clone();
                None
            }
            AstNode::Write(expr) => {
                let value = self.gen_code(expr).unwrap();
                let printf = self.get_printf();
                let mut args = vec![value];
                self.builder.build_call(printf, &args, "printf_call");
                None
            }
            AstNode::BinaryOp(op, left, right) => {
                let lhs = self.gen_code(left).unwrap();
                let rhs = self.gen_code(right).unwrap();
                match op {
                    '+' => Some(self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add").into()),
                    '-' => Some(self.builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub").into()),
                    '*' => Some(self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul").into()),
                    '/' => Some(self.builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "div").into()),
                    _ => None,
                }
            }
            AstNode::Integer(i) => Some(self.context.i64_type().const_int(*i as u64, true).into()),
            AstNode::Float(f) => Some(self.context.f64_type().const_float(*f).into()),
            AstNode::String(s) => {
                let global = self.builder.build_global_string_ptr(s, "str");
                Some(global.as_basic_value_enum())
            }
            AstNode::Identifier(id) => {
                if let Some(var) = self.variables.get(id) {
                    Some(self.builder.build_load(*var, id).into())
                } else {
                    None
                }
            }
            AstNode::Embedded(lang, code) => {
                // For simplicity, if lang == "rust", compile inline or something; here, placeholder
                // In real, use rustc or bindgen for C, etc.
                println!("Embedded {} code: {}", lang, code);
                None
            }
        }
    }

    fn get_printf(&mut self) -> FunctionValue<'ctx> {
        let i32_type = self.context.i32_type();
        let printf_type = i32_type.fn_type(&[self.context.i8_type().ptr_type(inkwell::AddressSpace::Generic).into()], true);
        self.module.add_function("printf", printf_type, None)
    }

    fn compile_to_binary(&self, target_file: &str) {
        Target::initialize_all(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        let pass_manager = PassManager::create(&self.module);
        pass_manager.add_verifier_pass();
        pass_manager.run_on(&self.module);

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(target_file))
            .unwrap();
        // Then, link to executable using system linker, but for simplicity, output .o
    }
}

// For VM bytecode generation (simplified, similar to VM opcodes)
fn gen_bytecode(node: &AstNode) -> Vec<u8> {
    let mut bytecode = Vec::new();
    match node {
        AstNode::Block(stmts) => {
            for stmt in stmts {
                bytecode.extend(gen_bytecode(stmt));
            }
        }
        AstNode::Write(expr) => {
            bytecode.extend(gen_bytecode(expr));
            bytecode.push(0x09); // Opcode::Write
        }
        AstNode::BinaryOp(op, left, right) => {
            bytecode.extend(gen_bytecode(left));
            bytecode.extend(gen_bytecode(right));
            match op {
                '+' => bytecode.push(0x05), // Add
                '-' => bytecode.push(0x06),
                '*' => bytecode.push(0x07),
                '/' => bytecode.push(0x08),
                _ => {}
            }
        }
        AstNode::Integer(i) => {
            bytecode.push(0x01); // PushInt
            bytecode.extend(i.to_le_bytes());
        }
        AstNode::Float(f) => {
            bytecode.push(0x02); // PushFloat
            bytecode.extend(f.to_le_bytes());
        }
        AstNode::String(s) => {
            bytecode.push(0x03); // PushString, assume index 0 for simplicity
            bytecode.extend((0u32).to_le_bytes());
            // In real, add to constant pool
        }
        _ => {}
    }
    bytecode.push(0xFF); // Halt
    bytecode
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: compiler <input.vib> <output> <mode>");
        eprintln!("mode: binary or object");
        process::exit(1);
    }

    let input_file = &args[1];
    let output_file = &args[2];
    let mode = &args[3];

    let input = fs::read_to_string(input_file).expect("Failed to read input file");

    let mut parser = Parser::new(&input);
    let ast = parser.parse();

    if mode == "binary" {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "vib_module");
        codegen.gen_code(&ast);

        // Add main function wrapper
        let i32_type = context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_func = codegen.module.add_function("main", main_type, None);
        let entry = context.append_basic_block(main_func, "entry");
        codegen.builder.position_at_end(entry);
        codegen.gen_code(&ast); // Generate in main
        codegen.builder.build_return(Some(&i32_type.const_int(0, false)));

        codegen.compile_to_binary(output_file);
    } else if mode == "object" {
        let bytecode = gen_bytecode(&ast);
        let mut file = File::create(output_file).expect("Failed to create output");
        file.write_all(&bytecode).expect("Failed to write bytecode");
    } else {
        eprintln!("Unknown mode: {}", mode);
    }
}

// Note: This is a demo. In full:
// - Full parser for classes, functions (JS-like).
// - Handle manual/auto memory: For auto, add refcounting in codegen.
// - Embedded code: Compile separately and link (e.g., rustc for Rust, javac for Java).
// - Use [] for blocks properly.
// - Handle project structure with bytes.yaml.
// - More opcodes, types, etc.
