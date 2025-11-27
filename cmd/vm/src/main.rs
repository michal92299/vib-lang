// src/main.rs
// This is a simple implementation of a virtual machine in Rust for the fictional 'vib' language.
// It's designed to be like a JVM but simpler, more efficient, and easier to use.
// The VM interprets a custom bytecode format from .vib-vm files (which are like .jar archives containing .object files like .class).
// For simplicity, this VM assumes .vib-vm is a single binary file with bytecode, not a full archive.
// In a real implementation, you'd use a zip library to handle archives.

// We define a stack-based VM with basic operations: arithmetic, I/O, memory management.
// Memory management: Supports manual (with alloc/free) and automatic (simple refcounting for demo).

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process;

// Opcodes for the bytecode
#[derive(Debug, Clone, Copy, PartialEq)]
enum Opcode {
    Nop = 0x00,
    PushInt = 0x01,    // Push i64
    PushFloat = 0x02,  // Push f64
    PushString = 0x03, // Push string index
    Pop = 0x04,
    Add = 0x05,
    Sub = 0x06,
    Mul = 0x07,
    Div = 0x08,
    Write = 0x09,      // Like print/write
    Alloc = 0x0A,      // Manual alloc (size in bytes)
    Free = 0x0B,       // Manual free (ptr)
    Load = 0x0C,       // Load from memory
    Store = 0x0D,      // Store to memory
    Jump = 0x0E,       // Unconditional jump
    JumpIfFalse = 0x0F,// Conditional jump
    Call = 0x10,       // Call function (index)
    Return = 0x11,
    Halt = 0xFF,
}

// Value types for the stack
#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Pointer(usize),  // For manual memory
    Null,
}

// Memory management: Simple heap simulation with refcounting for automatic mode
struct Heap {
    memory: Vec<u8>,
    allocations: HashMap<usize, (usize, usize)>, // addr -> (size, refcount)
}

impl Heap {
    fn new() -> Self {
        Heap {
            memory: Vec::new(),
            allocations: HashMap::new(),
        }
    }

    fn alloc(&mut self, size: usize) -> usize {
        let addr = self.memory.len();
        self.memory.extend(vec![0u8; size]);
        self.allocations.insert(addr, (size, 1)); // Initial refcount 1
        addr
    }

    fn free(&mut self, addr: usize) {
        if let Some((size, refcount)) = self.allocations.get_mut(&addr) {
            *refcount -= 1;
            if *refcount == 0 {
                // In real VM, reclaim memory; here, just mark or ignore for simplicity
                self.allocations.remove(&addr);
            }
        }
    }

    fn read_i64(&self, addr: usize) -> i64 {
        let bytes = &self.memory[addr..addr + 8];
        i64::from_le_bytes(bytes.try_into().unwrap())
    }

    fn write_i64(&mut self, addr: usize, val: i64) {
        let bytes = val.to_le_bytes();
        self.memory[addr..addr + 8].copy_from_slice(&bytes);
    }

    // Similar for other types...
}

// VM structure
struct VM {
    stack: Vec<Value>,
    heap: Heap,
    pc: usize,                // Program counter
    bytecode: Vec<u8>,        // Loaded bytecode
    strings: Vec<String>,     // Constant pool for strings
    functions: HashMap<usize, usize>, // func_index -> start_pc
    auto_memory: bool,        // True if automatic memory management
}

impl VM {
    fn new(bytecode: Vec<u8>, auto_memory: bool) -> Self {
        VM {
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            bytecode,
            strings: Vec::new(),
            functions: HashMap::new(),
            auto_memory,
        }
    }

    fn run(&mut self) -> Result<(), String> {
        while self.pc < self.bytecode.len() {
            let opcode = self.bytecode[self.pc] as Opcode;
            self.pc += 1;
            match opcode {
                Opcode::Nop => {},
                Opcode::PushInt => {
                    let val = self.read_i64();
                    self.stack.push(Value::Int(val));
                }
                Opcode::PushFloat => {
                    let val = self.read_f64();
                    self.stack.push(Value::Float(val));
                }
                Opcode::PushString => {
                    let idx = self.read_u32() as usize;
                    self.stack.push(Value::String(self.strings[idx].clone()));
                }
                Opcode::Pop => { let _ = self.stack.pop(); }
                Opcode::Add => self.binary_op(|a, b| a + b, |a, b| a + b),
                Opcode::Sub => self.binary_op(|a, b| a - b, |a, b| a - b),
                Opcode::Mul => self.binary_op(|a, b| a * b, |a, b| a * b),
                Opcode::Div => self.binary_op(|a, b| a / b, |a, b| a / b),
                Opcode::Write => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::Int(i) => println!("{}", i),
                            Value::Float(f) => println!("{}", f),
                            Value::String(s) => println!("{}", s),
                            _ => return Err("Invalid write value".to_string()),
                        }
                    }
                }
                Opcode::Alloc => {
                    if let Some(Value::Int(size)) = self.stack.pop() {
                        let addr = self.heap.alloc(size as usize);
                        self.stack.push(Value::Pointer(addr));
                    }
                }
                Opcode::Free => {
                    if let Some(Value::Pointer(addr)) = self.stack.pop() {
                        self.heap.free(addr);
                    }
                }
                Opcode::Load => {
                    if let Some(Value::Pointer(addr)) = self.stack.pop() {
                        let val = self.heap.read_i64(addr); // Assume loading i64 for simplicity
                        self.stack.push(Value::Int(val));
                    }
                }
                Opcode::Store => {
                    if let (Some(Value::Int(val)), Some(Value::Pointer(addr))) = (self.stack.pop(), self.stack.pop()) {
                        self.heap.write_i64(addr, val);
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_i32() as usize;
                    self.pc = offset;
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_i32() as usize;
                    if let Some(Value::Int(cond)) = self.stack.pop() {
                        if cond == 0 {
                            self.pc = offset;
                        }
                    }
                }
                Opcode::Call => {
                    let func_idx = self.read_u32() as usize;
                    if let Some(&start_pc) = self.functions.get(&func_idx) {
                        // Push return address
                        self.stack.push(Value::Int(self.pc as i64));
                        self.pc = start_pc;
                    }
                }
                Opcode::Return => {
                    if let Some(Value::Int(ret_pc)) = self.stack.pop() {
                        self.pc = ret_pc as usize;
                    }
                }
                Opcode::Halt => break,
                _ => return Err(format!("Unknown opcode: {:?}", opcode)),
            }
        }
        Ok(())
    }

    fn binary_op<F, G>(&mut self, int_op: F, float_op: G)
    where
        F: Fn(i64, i64) -> i64,
        G: Fn(f64, f64) -> f64,
    {
        if let (Some(b), Some(a)) = (self.stack.pop(), self.stack.pop()) {
            match (a, b) {
                (Value::Int(ai), Value::Int(bi)) => self.stack.push(Value::Int(int_op(ai, bi))),
                (Value::Float(af), Value::Float(bf)) => self.stack.push(Value::Float(float_op(af, bf))),
                _ => {} // Error handling omitted for brevity
            }
        }
    }

    fn read_i64(&mut self) -> i64 {
        let bytes = &self.bytecode[self.pc..self.pc + 8];
        self.pc += 8;
        i64::from_le_bytes(bytes.try_into().unwrap())
    }

    fn read_f64(&mut self) -> f64 {
        let bytes = &self.bytecode[self.pc..self.pc + 8];
        self.pc += 8;
        f64::from_le_bytes(bytes.try_into().unwrap())
    }

    fn read_u32(&mut self) -> u32 {
        let bytes = &self.bytecode[self.pc..self.pc + 4];
        self.pc += 4;
        u32::from_le_bytes(bytes.try_into().unwrap())
    }

    fn read_i32(&mut self) -> i32 {
        let bytes = &self.bytecode[self.pc..self.pc + 4];
        self.pc += 4;
        i32::from_le_bytes(bytes.try_into().unwrap())
    }

    // In real VM, load constants and functions from header
    fn load_constants(&mut self) {
        // Placeholder: Assume bytecode starts with header
        // For demo, hardcode some strings and functions
        self.strings.push("Hello, Vib!".to_string());
        self.functions.insert(0, 100); // Example func at pc 100
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: virtual-machine <file.vib-vm> <memory_mode>");
        eprintln!("memory_mode: manual or automatic");
        process::exit(1);
    }

    let filename = &args[1];
    let mode = &args[2];
    let auto_memory = mode == "automatic";

    let file = File::open(filename).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut bytecode = Vec::new();
    reader.read_to_end(&mut bytecode).expect("Failed to read file");

    let mut vm = VM::new(bytecode, auto_memory);
    vm.load_constants();

    if let Err(e) = vm.run() {
        eprintln!("VM Error: {}", e);
        process::exit(1);
    }
}

// Note: This is a simplified demo. In a full implementation, you'd add:
// - Proper bytecode loading with headers for constants, functions.
// - More opcodes for control flow, objects, classes (inspired by JS/Ruby/Python).
// - Better memory management (GC for automatic mode).
// - Error handling, debugging.
// - Integration with vib lang syntax (e.g., parse :biblioteka:, <zarzadznie pamiecia>, etc. in compiler).
// - Support for embedded code blocks like #=rust={ code } translated to native calls.
// For brevity, this is a starting point for a "better JVM" – efficient Rust base, simple API.
