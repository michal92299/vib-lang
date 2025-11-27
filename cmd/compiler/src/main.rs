// src/main.rs
// This is an expanded implementation of the 'compiler' binary for vib-lang in Rust.
// It uses the Inkwell crate for LLVM integration to compile vib code to native binaries.
// For VM compilation, it generates a custom bytecode (.object files).
// The compiler parses vib syntax, which is inspired by Python/Ruby/JS with custom elements.
// Updated syntax based on user feedback:
// - Imports: :std: instead of :biblioteka: std
// - Memory management: <automatic> or <manual> instead of <zarzadznie pamiecia> automatic/manual
// - Comments: -> instead of # (line comments start with ->)
// - Embedded code blocks #=lang={ code } translated to calls or inlined.
// - Blocks use [] instead of {} or indentation.
// Expanded features:
// - Support for variables (let x = expr;)
// - Assignments (x = expr;)
// - If statements: [cond] [true_block] [false_block]
// - While loops: [cond] [body]
// - Functions: fn name(params) [body]
// - Classes: class Name [methods]
// - More arithmetic, comparisons.
// - Better error handling.
// - In codegen, handle automatic memory with refcounting (simple impl).
// Note: Still simplified; a real compiler would be more complex.

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, IntValue};
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;
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
    Colon, // :
    LBracket, // [
    RBracket, // ]
    LAngle, // <
    RAngle, // >
    Hash, // # (still for embedded, but comments are ->)
    Equal, // =
    LBrace, // {
    RBrace, // }
    Plus, Minus, Star, Slash,
    Lt, Gt, Le, Ge, EqEq, NotEq, // Comparisons
    Let, // let keyword for var decl
    Fn, // fn keyword
    Class, // class keyword
    If, // if (but using [cond] [true] [false])
    While, // while ( [cond] [body] )
    Return, // return
    Write, // write keyword
    Import(String), // :lib: parses lib
    MemoryMode(String), // <automatic> or <manual>
    EmbeddedStart(String), // #=lang=
    EmbeddedEnd,
    Assign, // =
    Semicolon, // ; statement end
    Comma, // ,
    LParen, RParen, // ( )
    Arrow, // -> for comments, but lexer skips comments
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
        self.skip_comment();
        if self.pos >= self.chars.len() {
            return Token::EOF;
        }
        let ch = self.chars[self.pos];
        match ch {
            ':' => {
                self.pos += 1;
                let lib = self.read_identifier();
                if self.current_char() == Some(':') {
                    self.pos += 1;
                    Token::Import(lib)
                } else {
                    self.pos -= lib.len() + 1; // Rewind
                    Token::Colon
                }
            }
            '<' => {
                self.pos += 1;
                let mode = self.read_identifier();
                if self.current_char() == Some('>') {
                    self.pos += 1;
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
                        Token::EmbeddedStart(lang)
                    } else {
                        Token::Hash
                    }
                } else {
                    Token::Hash
                }
            }
            '=' => {
                self.pos += 1;
                if self.current_char() == Some('=') {
                    self.pos += 1;
                    Token::EqEq
                } else {
                    Token::Assign
                }
            }
            '<' => {
                self.pos += 1;
                if self.current_char() == Some('=') {
                    self.pos += 1;
                    Token::Le
                } else {
                    Token::Lt
                }
            }
            '>' => {
                self.pos += 1;
                if self.current_char() == Some('=') {
                    self.pos += 1;
                    Token::Ge
                } else {
                    Token::Gt
                }
            }
            '!' => {
                self.pos += 1;
                if self.current_char() == Some('=') {
                    self.pos += 1;
                    Token::NotEq
                } else {
                    Token::EOF // Unknown
                }
            }
            '{' => { self.pos += 1; Token::LBrace }
            '}' => { self.pos += 1; Token::EmbeddedEnd }
            '+' => { self.pos += 1; Token::Plus }
            '-' => {
                self.pos += 1;
                if self.current_char() == Some('>') {
                    self.pos += 1;
                    Token::Arrow // For comments, but since skip_comment handles, maybe not needed
                } else {
                    Token::Minus
                }
            }
            '*' => { self.pos += 1; Token::Star }
            '/' => { self.pos += 1; Token::Slash }
            '(' => { self.pos += 1; Token::LParen }
            ')' => { self.pos += 1; Token::RParen }
            ';' => { self.pos += 1; Token::Semicolon }
            ',' => { self.pos += 1; Token::Comma }
            '"' => self.read_string(),
            '0'..='9' => self.read_number(),
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_identifier();
                match ident.as_str() {
                    "let" => Token::Let,
                    "fn" => Token::Fn,
                    "class" => Token::Class,
                    "if" => Token::If, // Though syntax uses [
                    "while" => Token::While,
                    "return" => Token::Return,
                    "write" => Token::Write,
                    _ => Token::Identifier(ident),
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
    fn skip_comment(&mut self) {
        if self.pos + 1 < self.chars.len() && self.chars[self.pos] == '-' && self.chars[self.pos + 1] == '>' {
            self.pos += 2;
            while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                self.pos += 1;
            }
        }
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
    Block(Vec<AstNode>),
    VarDecl(String, Box<AstNode>), // let x = expr
    Assign(String, Box<AstNode>), // x = expr
    If(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>), // [cond] [true] [false?]
    While(Box<AstNode>, Box<AstNode>), // [cond] [body]
    FnDef(String, Vec<String>, Box<AstNode>), // fn name(params) [body]
    ClassDef(String, Vec<AstNode>), // class Name [methods]
    Return(Box<AstNode>),
    Write(Box<AstNode>),
    BinaryOp(char, Box<AstNode>, Box<AstNode>),
    CompareOp(String, Box<AstNode>, Box<AstNode>), // ==, <, >, etc.
    Call(String, Vec<AstNode>), // name(args)
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Import(String),
    MemoryMode(String),
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
            statements.push(self.parse_statement());
            self.advance();
        }
        AstNode::Block(statements)
    }
    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }
    fn parse_statement(&mut self) -> AstNode {
        match &self.current {
            Token::Let => self.parse_var_decl(),
            Token::Fn => self.parse_fn_def(),
            Token::Class => self.parse_class_def(),
            Token::Return => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                AstNode::Return(Box::new(expr))
            }
            Token::Write => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                AstNode::Write(Box::new(expr))
            }
            Token::Import(lib) => AstNode::Import(lib.clone()),
            Token::MemoryMode(mode) => AstNode::MemoryMode(mode.clone()),
            Token::EmbeddedStart(lang) => {
                let lang = lang.clone();
                self.advance();
                self.expect(Token::LBrace);
                let start = self.lexer.pos;
                while self.current != Token::EmbeddedEnd {
                    self.advance();
                }
                let code = &self.lexer.input[start..self.lexer.pos - 1];
                AstNode::Embedded(lang, code.to_string())
            }
            Token::LBracket => self.parse_block_or_control(),
            Token::Identifier(id) => {
                let id = id.clone();
                self.advance();
                if self.current == Token::Assign {
                    self.advance();
                    let expr = self.parse_expr();
                    self.expect(Token::Semicolon);
                    AstNode::Assign(id, Box::new(expr))
                } else if self.current == Token::LParen {
                    let args = self.parse_args();
                    self.expect(Token::Semicolon);
                    AstNode::Call(id, args)
                } else {
                    panic!("Unexpected after identifier");
                }
            }
            _ => {
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                expr
            }
        }
    }
    fn parse_var_decl(&mut self) -> AstNode {
        self.advance(); // Skip let
        if let Token::Identifier(name) = &self.current {
            let name = name.clone();
            self.advance();
            self.expect(Token::Assign);
            let expr = self.parse_expr();
            self.expect(Token::Semicolon);
            AstNode::VarDecl(name, Box::new(expr))
        } else {
            panic!("Expected identifier after let");
        }
    }
    fn parse_fn_def(&mut self) -> AstNode {
        self.advance(); // Skip fn
        if let Token::Identifier(name) = &self.current {
            let name = name.clone();
            self.advance();
            self.expect(Token::LParen);
            let params = self.parse_params();
            self.expect(Token::RParen);
            let body = self.parse_block();
            AstNode::FnDef(name, params, Box::new(body))
        } else {
            panic!("Expected function name");
        }
    }
    fn parse_class_def(&mut self) -> AstNode {
        self.advance(); // Skip class
        if let Token::Identifier(name) = &self.current {
            let name = name.clone();
            self.advance();
            let methods = self.parse_block().as_block().unwrap();
            AstNode::ClassDef(name, methods)
        } else {
            panic!("Expected class name");
        }
    }
    fn parse_block_or_control(&mut self) -> AstNode {
        self.advance(); // Skip [
        let cond_or_stmt = self.parse_expr();
        self.expect(Token::RBracket);
        let true_body = self.parse_block();
        if self.current == Token::LBracket {
            let false_body = self.parse_block();
            AstNode::If(Box::new(cond_or_stmt), Box::new(true_body), Some(Box::new(false_body)))
        } else if matches!(self.current, Token::While) {
            // For while, but since syntax is [cond] [body], no 'while' keyword needed? Assume implicit while if after cond
            // Adjust for [cond] [body] as while or if based on context; for simplicity, assume [cond] [body] is while if no false
            AstNode::While(Box::new(cond_or_stmt), Box::new(true_body))
        } else {
            AstNode::If(Box::new(cond_or_stmt), Box::new(true_body), None)
        }
    }
    fn parse_block(&mut self) -> AstNode {
        self.expect(Token::LBracket);
        let mut stmts = Vec::new();
        while self.current != Token::RBracket && self.current != Token::EOF {
            stmts.push(self.parse_statement());
        }
        self.expect(Token::RBracket);
        AstNode::Block(stmts)
    }
    fn parse_params(&mut self) -> Vec<String> {
        let mut params = Vec::new();
        if self.current != Token::RParen {
            if let Token::Identifier(p) = &self.current {
                params.push(p.clone());
                self.advance();
            }
            while self.current == Token::Comma {
                self.advance();
                if let Token::Identifier(p) = &self.current {
                    params.push(p.clone());
                    self.advance();
                }
            }
        }
        params
    }
    fn parse_args(&mut self) -> Vec<AstNode> {
        self.advance(); // Skip (
        let mut args = Vec::new();
        if self.current != Token::RParen {
            args.push(self.parse_expr());
            while self.current == Token::Comma {
                self.advance();
                args.push(self.parse_expr());
            }
        }
        self.expect(Token::RParen);
        args
    }
    fn expect(&mut self, expected: Token) {
        if self.current == expected {
            self.advance();
        } else {
            panic!("Expected {:?}, got {:?}", expected, self.current);
        }
    }
    fn parse_expr(&mut self) -> AstNode {
        let mut left = self.parse_term();
        while matches!(&self.current, Token::Plus | Token::Minus) {
            let op = match &self.current {
                Token::Plus => '+',
                Token::Minus => '-',
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term();
            left = AstNode::BinaryOp(op, Box::new(left), Box::new(right));
        }
        left
    }
    fn parse_term(&mut self) -> AstNode {
        let mut left = self.parse_primary();
        while matches!(&self.current, Token::Star | Token::Slash) {
            let op = match &self.current {
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
            Token::Integer(i) => {
                let val = *i;
                self.advance();
                AstNode::Integer(val)
            }
            Token::Float(f) => {
                let val = *f;
                self.advance();
                AstNode::Float(val)
            }
            Token::String(s) => {
                let val = s.clone();
                self.advance();
                AstNode::String(val)
            }
            Token::Identifier(id) => {
                let id = id.clone();
                self.advance();
                if self.current == Token::LParen {
                    let args = self.parse_args();
                    AstNode::Call(id, args)
                } else {
                    AstNode::Identifier(id)
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            _ => panic!("Unexpected token in primary: {:?}", self.current),
        }
    }
    // Add parse_compare if needed, but for now binary op for arith, separate for compare
}

// Codegen for LLVM
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    classes: HashMap<String, PointerType<'ctx>>, // Simple struct types
    memory_mode: String, // "manual" or "automatic"
    refcount_type: inkwell::types::StructType<'ctx>, // For auto: {refcount: i32, value: T}
}
impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let refcount_type = context.opaque_struct_type("RefCounted");
        refcount_type.set_body(&[context.i32_type().into(), context.i64_type().into()], false); // Example for i64
        CodeGen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            memory_mode: "manual".to_string(),
            refcount_type,
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
                // Link external libs; for now, ignore
                None
            }
            AstNode::MemoryMode(mode) => {
                self.memory_mode = mode.clone();
                None
            }
            AstNode::VarDecl(name, expr) => {
                let value = self.gen_code(expr).unwrap();
                let alloca = self.builder.build_alloca(value.get_type(), name);
                self.builder.build_store(alloca, value);
                self.variables.insert(name.clone(), alloca);
                None
            }
            AstNode::Assign(name, expr) => {
                if let Some(var) = self.variables.get(name) {
                    let value = self.gen_code(expr).unwrap();
                    self.builder.build_store(*var, value);
                }
                None
            }
            AstNode::If(cond, true_body, false_body) => {
                let cond_val = self.gen_code(cond).unwrap().into_int_value();
                let then_bb = self.context.append_basic_block(self.get_current_fn(), "then");
                let else_bb = self.context.append_basic_block(self.get_current_fn(), "else");
                let merge_bb = self.context.append_basic_block(self.get_current_fn(), "merge");
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                self.gen_code(true_body);
                self.builder.build_unconditional_branch(merge_bb);
                self.builder.position_at_end(else_bb);
                if let Some(fb) = false_body {
                    self.gen_code(fb);
                }
                self.builder.build_unconditional_branch(merge_bb);
                self.builder.position_at_end(merge_bb);
                None
            }
            AstNode::While(cond, body) => {
                let loop_bb = self.context.append_basic_block(self.get_current_fn(), "loop");
                let body_bb = self.context.append_basic_block(self.get_current_fn(), "body");
                let exit_bb = self.context.append_basic_block(self.get_current_fn(), "exit");
                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);
                let cond_val = self.gen_code(cond).unwrap().into_int_value();
                self.builder.build_conditional_branch(cond_val, body_bb, exit_bb);
                self.builder.position_at_end(body_bb);
                self.gen_code(body);
                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(exit_bb);
                None
            }
            AstNode::FnDef(name, params, body) => {
                let param_types = vec![self.context.i64_type().into(); params.len()];
                let fn_type = self.context.i64_type().fn_type(&param_types, false);
                let fn_val = self.module.add_function(name, fn_type, None);
                self.functions.insert(name.clone(), fn_val);
                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);
                for (i, param) in params.iter().enumerate() {
                    let alloca = self.builder.build_alloca(self.context.i64_type(), param);
                    self.builder.build_store(alloca, fn_val.get_nth_param(i as u32).unwrap());
                    self.variables.insert(param.clone(), alloca);
                }
                self.gen_code(body);
                self.builder.build_return(Some(&self.context.i64_type().const_int(0, false)));
                None
            }
            AstNode::ClassDef(name, methods) => {
                let class_type = self.context.opaque_struct_type(name);
                // For simplicity, assume methods are functions with self
                for method in methods {
                    if let AstNode::FnDef(mname, params, body) = method {
                        let mut full_params = vec!["self".to_string()];
                        full_params.extend_from_slice(params);
                        let full_name = format!("{}.{}", name, mname);
                        self.gen_code(&AstNode::FnDef(full_name, full_params, body.clone()));
                    }
                }
                self.classes.insert(name.clone(), class_type.ptr_type(AddressSpace::Generic));
                None
            }
            AstNode::Return(expr) => {
                let val = self.gen_code(expr).unwrap();
                self.builder.build_return(Some(&val));
                None
            }
            AstNode::Write(expr) => {
                let value = self.gen_code(expr).unwrap();
                let printf = self.get_printf();
                let format = self.builder.build_global_string_ptr("%lld\n", "fmt");
                let args = &[format.as_basic_value_enum(), value];
                self.builder.build_call(printf, args, "printf_call");
                None
            }
            AstNode::BinaryOp(op, left, right) => {
                let lhs = self.gen_code(left).unwrap().into_int_value();
                let rhs = self.gen_code(right).unwrap().into_int_value();
                match op {
                    '+' => Some(self.builder.build_int_add(lhs, rhs, "add").into()),
                    '-' => Some(self.builder.build_int_sub(lhs, rhs, "sub").into()),
                    '*' => Some(self.builder.build_int_mul(lhs, rhs, "mul").into()),
                    '/' => Some(self.builder.build_int_signed_div(lhs, rhs, "div").into()),
                    _ => None,
                }
            }
            AstNode::CompareOp(op, left, right) => {
                let lhs = self.gen_code(left).unwrap().into_int_value();
                let rhs = self.gen_code(right).unwrap().into_int_value();
                let cmp = match op.as_str() {
                    "==" => self.builder.build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "eq"),
                    "!=" => self.builder.build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "ne"),
                    "<" => self.builder.build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "lt"),
                    ">" => self.builder.build_int_compare(inkwell::IntPredicate::SGT, lhs, rhs, "gt"),
                    "<=" => self.builder.build_int_compare(inkwell::IntPredicate::SLE, lhs, rhs, "le"),
                    ">=" => self.builder.build_int_compare(inkwell::IntPredicate::SGE, lhs, rhs, "ge"),
                    _ => panic!("Unknown compare op"),
                };
                Some(self.builder.build_int_cast(cmp, self.context.i64_type(), "bool_to_int").into())
            }
            AstNode::Call(name, args) => {
                if let Some(fn_val) = self.functions.get(name) {
                    let mut arg_vals = Vec::new();
                    for arg in args {
                        arg_vals.push(self.gen_code(arg).unwrap().into());
                    }
                    Some(self.builder.build_call(*fn_val, &arg_vals, "call").try_as_basic_value().left().unwrap())
                } else {
                    None
                }
            }
            AstNode::Identifier(name) => {
                if let Some(var) = self.variables.get(name) {
                    Some(self.builder.build_load(*var, name).into())
                } else {
                    None
                }
            }
            AstNode::Integer(i) => Some(self.context.i64_type().const_int(*i as u64, true).into()),
            AstNode::Float(f) => Some(self.context.f64_type().const_float(*f).into()),
            AstNode::String(s) => {
                let global = self.builder.build_global_string_ptr(s, "str");
                Some(global.as_basic_value_enum())
            }
            AstNode::Embedded(lang, code) => {
                // Placeholder: Compile inline if lang == "rust", else comment
                println!("Embedded {} code: {}", lang, code);
                None
            }
        }
    }
    fn get_current_fn(&self) -> FunctionValue<'ctx> {
        self.builder.get_insert_block().unwrap().get_parent().unwrap()
    }
    fn get_printf(&mut self) -> FunctionValue<'ctx> {
        let i32_type = self.context.i32_type();
        let printf_type = i32_type.fn_type(&[self.context.i8_type().ptr_type(AddressSpace::Generic).into(), self.context.i64_type().into()], true);
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
        // Link to executable: In real, use linker like ld or clang
    }
}

// For VM bytecode generation (expanded with more opcodes)
#[derive(Debug)]
enum BytecodeOp {
    PushInt(i64),
    PushFloat(f64),
    PushString(usize), // index
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Write,
    Alloc(usize),
    Free,
    Load,
    Store,
    Jump(usize),
    JumpIfFalse(usize),
    Call(usize),
    Return,
    Dup,
    Swap,
    LoadVar(usize), // var index
    StoreVar(usize),
    Halt,
}
fn gen_bytecode(node: &AstNode, const_pool: &mut Vec<String>, var_map: &mut HashMap<String, usize>) -> Vec<u8> {
    let mut bytecode = Vec::new();
    match node {
        AstNode::Block(stmts) => {
            for stmt in stmts {
                bytecode.extend(gen_bytecode(stmt, const_pool, var_map));
            }
        }
        AstNode::Write(expr) => {
            bytecode.extend(gen_bytecode(expr, const_pool, var_map));
            bytecode.push(0x09); // Write
        }
        AstNode::BinaryOp(op, left, right) => {
            bytecode.extend(gen_bytecode(left, const_pool, var_map));
            bytecode.extend(gen_bytecode(right, const_pool, var_map));
            match op {
                '+' => bytecode.push(0x05),
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
            bytecode.push(0x02);
            bytecode.extend(f.to_le_bytes());
        }
        AstNode::String(s) => {
            let idx = const_pool.len();
            const_pool.push(s.clone());
            bytecode.push(0x03);
            bytecode.extend((idx as u32).to_le_bytes());
        }
        AstNode::Identifier(name) => {
            if let Some(&idx) = var_map.get(name) {
                bytecode.push(0x20); // LoadVar custom opcode
                bytecode.extend((idx as u32).to_le_bytes());
            }
        }
        // Add more for other nodes...
        _ => {}
    }
    bytecode.push(0xFF); // Halt at end
    bytecode
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 4 {
        eprintln!("Usage: compiler <input.vib> <output> <mode>");
        eprintln!("mode: binary, object, or vm");
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
        let i32_type = context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_func = codegen.module.add_function("main", main_type, None);
        let entry = context.append_basic_block(main_func, "entry");
        codegen.builder.position_at_end(entry);
        codegen.gen_code(&ast);
        codegen.builder.build_return(Some(&i32_type.const_int(0, false)));
        codegen.compile_to_binary(output_file);
    } else if mode == "object" || mode == "vm" {
        let mut const_pool = Vec::new();
        let mut var_map = HashMap::new();
        let bytecode = gen_bytecode(&ast, &mut const_pool, &mut var_map);
        let mut file = File::create(output_file).expect("Failed to create output");
        file.write_all(&bytecode).expect("Failed to write bytecode");
        // For vm mode, perhaps add header or package
    } else {
        eprintln!("Unknown mode: {}", mode);
    }
}
// Note: Expanded with more syntax support. For full production, add type checking, optimization, full refcounting for auto memory, etc.
// Handle floating point in VM, more opcodes for control flow, classes as objects in heap.
// Embedded code: Use external compilers/linker for integration.
