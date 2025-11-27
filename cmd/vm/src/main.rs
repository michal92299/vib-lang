// src/main.rs
// This is an expanded implementation of a virtual machine in Rust for the fictional 'vib' language.
// It's designed to be like a JVM but simpler, more efficient, and easier to use.
// The VM interprets a custom bytecode format from .vib-vm files (which are like .jar archives containing .object files like .class).
// For simplicity, this VM assumes .vib-vm is a single binary file with bytecode, constants, and headers.
// In a real implementation, you'd use a zip library to handle archives.
// Expanded features:
// - More opcodes: LoadVar, StoreVar, Dup, Swap, Compare ops (Eq, Lt, etc.), JumpIfTrue, NewObject for classes.
// - Support for functions: Call pushes args, return addr; Return pops value.
// - Classes: Simple objects as heap-allocated structs with methods (dispatch via vtable index).
// - Variables: Local variables in stack frames.
// - Constants pool for strings, integers, etc.
// - Better heap: Vec-based with refcounting for automatic mode (increment/decrement on assign, free when 0).
// - Manual mode: Explicit alloc/free.
// - Error handling improved.
// - Load header: Constants, functions entry points.

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process;

// Opcodes
#[derive(Debug, Clone, Copy, PartialEq)]
enum Opcode {
    Nop = 0x00,
    PushInt = 0x01,       // + i64
    PushFloat = 0x02,     // + f64
    PushString = 0x03,    // + const_index (u32)
    PushNull = 0x04,
    Pop = 0x05,
    Dup = 0x06,
    Swap = 0x07,
    Add = 0x08,
    Sub = 0x09,
    Mul = 0x0A,
    Div = 0x0B,
    Write = 0x0C,
    Alloc = 0x0D,         // size (u32) -> ptr
    Free = 0x0E,          // ptr
    Load = 0x0F,          // ptr -> value (assume i64 for simplicity)
    Store = 0x10,         // value, ptr
    LoadVar = 0x11,       // local_index (u16) -> value
    StoreVar = 0x12,      // value, local_index (u16)
    Jump = 0x13,          // offset (i32)
    JumpIfFalse = 0x14,   // offset (i32)
    JumpIfTrue = 0x15,    // offset (i32)
    Eq = 0x16,
    Lt = 0x17,
    Gt = 0x18,
    Le = 0x19,
    Ge = 0x1A,
    Ne = 0x1B,
    Call = 0x1C,          // func_index (u32), argc (u8)
    Return = 0x1D,
    NewObject = 0x1E,     // class_index (u32) -> obj_ptr
    GetField = 0x1F,      // obj_ptr, field_index (u16) -> value
    SetField = 0x20,      // value, obj_ptr, field_index (u16)
    CallMethod = 0x21,    // obj_ptr, method_index (u32), argc (u8)
    IncRef = 0x22,        // ptr (for auto mode)
    DecRef = 0x23,        // ptr
    Halt = 0xFF,
}

// Value types
#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Float(f64),
    String(usize),  // index in const pool
    Pointer(usize), // heap addr
    Null,
}

// Heap allocation
struct Allocation {
    size: usize,
    refcount: usize,
    data: Vec<u8>, // raw bytes
}

struct Heap {
    allocations: HashMap<usize, Allocation>,
    next_addr: usize,
}

impl Heap {
    fn new() -> Self {
        Heap {
            allocations: HashMap::new(),
            next_addr: 1, // Start from 1, 0 is null
        }
    }

    fn alloc(&mut self, size: usize) -> usize {
        let addr = self.next_addr;
        self.next_addr += size + 8; // + refcount space? But for simplicity, separate
        self.allocations.insert(addr, Allocation {
            size,
            refcount: 1,
            data: vec![0u8; size],
        });
        addr
    }

    fn free(&mut self, addr: usize) {
        if let Some(alloc) = self.allocations.get_mut(&addr) {
            alloc.refcount -= 1;
            if alloc.refcount == 0 {
                self.allocations.remove(&addr);
            }
        }
    }

    fn inc_ref(&mut self, addr: usize) {
        if let Some(alloc) = self.allocations.get_mut(&addr) {
            alloc.refcount += 1;
        }
    }

    fn read_i64(&self, addr: usize, offset: usize) -> i64 {
        if let Some(alloc) = self.allocations.get(&addr) {
            let bytes = &alloc.data[offset..offset + 8];
            i64::from_le_bytes(bytes.try_into().unwrap())
        } else {
            0
        }
    }

    fn write_i64(&mut self, addr: usize, offset: usize, val: i64) {
        if let Some(alloc) = self.allocations.get_mut(&addr) {
            let bytes = val.to_le_bytes();
            alloc.data[offset..offset + 8].copy_from_slice(&bytes);
        }
    }

    // Similar for other types...
}

// Stack frame for functions
struct Frame {
    return_pc: usize,
    locals: Vec<Value>,
}

// VM
struct VM {
    stack: Vec<Value>,
    heap: Heap,
    pc: usize,
    bytecode: Vec<u8>,
    const_strings: Vec<String>, // constant strings
    functions: HashMap<usize, usize>, // func_index -> pc
    classes: HashMap<usize, usize>, // class_index -> field_count or vtable
    frames: Vec<Frame>,
    auto_memory: bool,
}

impl VM {
    fn new(bytecode: Vec<u8>, auto_memory: bool) -> Self {
        VM {
            stack: Vec::new(),
            heap: Heap::new(),
            pc: 0,
            bytecode,
            const_strings: Vec::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            frames: Vec::new(),
            auto_memory,
        }
    }

    fn run(&mut self) -> Result<(), String> {
        self.load_header();
        self.frames.push(Frame { return_pc: 0, locals: vec![] }); // Global frame
        while self.pc < self.bytecode.len() {
            let opcode = Opcode::try_from(self.bytecode[self.pc]).map_err(|_| "Invalid opcode".to_string())?;
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
                    self.stack.push(Value::String(idx));
                }
                Opcode::PushNull => self.stack.push(Value::Null),
                Opcode::Pop => { self.pop_with_ref()?; }
                Opcode::Dup => {
                    let val = self.stack.last().cloned().unwrap_or(Value::Null);
                    self.stack.push(val);
                }
                Opcode::Swap => {
                    let len = self.stack.len();
                    if len >= 2 {
                        self.stack.swap(len - 1, len - 2);
                    }
                }
                Opcode::Add => self.binary_op_int(|a, b| a + b)?,
                Opcode::Sub => self.binary_op_int(|a, b| a - b)?,
                Opcode::Mul => self.binary_op_int(|a, b| a * b)?,
                Opcode::Div => self.binary_op_int(|a, b| if b != 0 { a / b } else { return Err("Div by zero".to_string()) })?,
                Opcode::Write => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::Int(i) => println!("{}", i),
                            Value::Float(f) => println!("{}", f),
                            Value::String(idx) => println!("{}", self.const_strings[idx]),
                            _ => return Err("Invalid write value".to_string()),
                        }
                    }
                }
                Opcode::Alloc => {
                    if let Value::Int(size) = self.pop_with_ref()? {
                        let addr = self.heap.alloc(size as usize);
                        self.stack.push(Value::Pointer(addr));
                    }
                }
                Opcode::Free => {
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        self.heap.free(addr);
                    }
                }
                Opcode::Load => {
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        let val = self.heap.read_i64(addr, 0); // offset 0
                        self.stack.push(Value::Int(val));
                    }
                }
                Opcode::Store => {
                    if let (Value::Int(val), Value::Pointer(addr)) = (self.pop_with_ref()?, self.pop_with_ref()?) {
                        self.heap.write_i64(addr, 0, val);
                    }
                }
                Opcode::LoadVar => {
                    let idx = self.read_u16() as usize;
                    let frame = self.frames.last().unwrap();
                    if idx < frame.locals.len() {
                        self.stack.push(frame.locals[idx].clone());
                    } else {
                        return Err("Invalid local index".to_string());
                    }
                }
                Opcode::StoreVar => {
                    let idx = self.read_u16() as usize;
                    let val = self.pop_with_ref()?;
                    let frame = self.frames.last_mut().unwrap();
                    if idx >= frame.locals.len() {
                        frame.locals.resize(idx + 1, Value::Null);
                    }
                    frame.locals[idx] = val;
                }
                Opcode::Jump => {
                    let offset = self.read_i32();
                    self.pc = (self.pc as i32 + offset) as usize;
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_i32();
                    if self.is_false(self.pop_with_ref()?) {
                        self.pc = (self.pc as i32 + offset) as usize;
                    }
                }
                Opcode::JumpIfTrue => {
                    let offset = self.read_i32();
                    if !self.is_false(self.pop_with_ref()?) {
                        self.pc = (self.pc as i32 + offset) as usize;
                    }
                }
                Opcode::Eq => self.compare_op(|a, b| a == b)?,
                Opcode::Lt => self.compare_op(|a, b| a < b)?,
                Opcode::Gt => self.compare_op(|a, b| a > b)?,
                Opcode::Le => self.compare_op(|a, b| a <= b)?,
                Opcode::Ge => self.compare_op(|a, b| a >= b)?,
                Opcode::Ne => self.compare_op(|a, b| a != b)?,
                Opcode::Call => {
                    let func_idx = self.read_u32() as usize;
                    let argc = self.bytecode[self.pc] as usize;
                    self.pc += 1;
                    if let Some(&entry_pc) = self.functions.get(&func_idx) {
                        let mut locals = vec![Value::Null; 10]; // Assume max locals, or from metadata
                        for i in (0..argc).rev() {
                            locals[i] = self.stack.pop().unwrap();
                        }
                        self.frames.push(Frame {
                            return_pc: self.pc,
                            locals,
                        });
                        self.pc = entry_pc;
                    } else {
                        return Err("Unknown function".to_string());
                    }
                }
                Opcode::Return => {
                    let ret_val = self.pop_with_ref()?;
                    let frame = self.frames.pop().unwrap();
                    self.pc = frame.return_pc;
                    self.stack.push(ret_val);
                }
                Opcode::NewObject => {
                    let class_idx = self.read_u32() as usize;
                    if let Some(&field_count) = self.classes.get(&class_idx) {
                        let size = field_count * 8; // Assume i64 fields
                        let addr = self.heap.alloc(size);
                        self.stack.push(Value::Pointer(addr));
                    }
                }
                Opcode::GetField => {
                    let field_idx = self.read_u16() as usize;
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        let val = self.heap.read_i64(addr, field_idx * 8);
                        self.stack.push(Value::Int(val));
                    }
                }
                Opcode::SetField => {
                    let field_idx = self.read_u16() as usize;
                    let val = self.pop_with_ref()?;
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        if let Value::Int(i) = val {
                            self.heap.write_i64(addr, field_idx * 8, i);
                        }
                    }
                }
                Opcode::CallMethod => {
                    let method_idx = self.read_u32() as usize;
                    let argc = self.bytecode[self.pc] as usize;
                    self.pc += 1;
                    // Assume method_idx is func_idx, obj is first arg
                    // Pop args, then obj
                    let mut args = vec![];
                    for _ in 0..argc {
                        args.push(self.pop_with_ref()?);
                    }
                    let obj = self.pop_with_ref()?;
                    args.insert(0, obj); // self
                    // Then like call, but resolve func from vtable; for simplicity, assume method_idx = func_idx
                    self.call_func(method_idx, args)?;
                }
                Opcode::IncRef => {
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        self.heap.inc_ref(addr);
                        self.stack.push(Value::Pointer(addr));
                    }
                }
                Opcode::DecRef => {
                    if let Value::Pointer(addr) = self.pop_with_ref()? {
                        self.heap.free(addr); // Dec and free if 0
                    }
                }
                Opcode::Halt => break,
            }
        }
        Ok(())
    }

    fn pop_with_ref(&mut self) -> Result<Value, String> {
        let val = self.stack.pop().ok_or("Stack underflow".to_string())?;
        if self.auto_memory {
            if let Value::Pointer(addr) = &val {
                self.heap.free(*addr); // Dec ref
            }
        }
        Ok(val)
    }

    fn is_false(&self, val: Value) -> bool {
        match val {
            Value::Int(0) => true,
            Value::Float(0.0) => true,
            Value::Null => true,
            _ => false,
        }
    }

    fn binary_op_int<F>(&mut self, op: F) -> Result<(), String>
    where F: Fn(i64, i64) -> i64,
    {
        let b = if let Value::Int(b) = self.pop_with_ref()? { b } else { return Err("Type mismatch".to_string()) };
        let a = if let Value::Int(a) = self.pop_with_ref()? { a } else { return Err("Type mismatch".to_string()) };
        self.stack.push(Value::Int(op(a, b)));
        Ok(())
    }

    fn compare_op<F>(&mut self, op: F) -> Result<(), String>
    where F: Fn(i64, i64) -> bool,
    {
        let b = if let Value::Int(b) = self.pop_with_ref()? { b } else { return Err("Type mismatch".to_string()) };
        let a = if let Value::Int(a) = self.pop_with_ref()? { a } else { return Err("Type mismatch".to_string()) };
        self.stack.push(Value::Int(if op(a, b) { 1 } else { 0 }));
        Ok(())
    }

    fn call_func(&mut self, func_idx: usize, args: Vec<Value>) -> Result<(), String> {
        if let Some(&entry_pc) = self.functions.get(&func_idx) {
            let mut locals = vec![Value::Null; 10]; // Arbitrary
            for (i, arg) in args.into_iter().enumerate() {
                locals[i] = arg;
            }
            self.frames.push(Frame {
                return_pc: self.pc,
                locals,
            });
            self.pc = entry_pc;
            Ok(())
        } else {
            Err("Unknown function".to_string())
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

    fn read_u16(&mut self) -> u16 {
        let bytes = &self.bytecode[self.pc..self.pc + 2];
        self.pc += 2;
        u16::from_le_bytes(bytes.try_into().unwrap())
    }

    fn load_header(&mut self) {
        // Assume first 4 bytes: num_strings u32
        self.pc = 0;
        let num_strings = self.read_u32() as usize;
        for _ in 0..num_strings {
            let len = self.read_u32() as usize;
            let bytes = &self.bytecode[self.pc..self.pc + len];
            self.pc += len;
            self.const_strings.push(String::from_utf8(bytes.to_vec()).unwrap());
        }
        // Num functions
        let num_funcs = self.read_u32() as usize;
        for _ in 0..num_funcs {
            let idx = self.read_u32() as usize;
            let pc = self.read_u32() as usize;
            self.functions.insert(idx, pc);
        }
        // Num classes
        let num_classes = self.read_u32() as usize;
        for _ in 0..num_classes {
            let idx = self.read_u32() as usize;
            let fields = self.read_u32() as usize;
            self.classes.insert(idx, fields);
        }
        // Bytecode starts here
    }
}

impl TryFrom<u8> for Opcode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Opcode::Nop),
            0x01 => Ok(Opcode::PushInt),
            0x02 => Ok(Opcode::PushFloat),
            0x03 => Ok(Opcode::PushString),
            0x04 => Ok(Opcode::PushNull),
            0x05 => Ok(Opcode::Pop),
            0x06 => Ok(Opcode::Dup),
            0x07 => Ok(Opcode::Swap),
            0x08 => Ok(Opcode::Add),
            0x09 => Ok(Opcode::Sub),
            0x0A => Ok(Opcode::Mul),
            0x0B => Ok(Opcode::Div),
            0x0C => Ok(Opcode::Write),
            0x0D => Ok(Opcode::Alloc),
            0x0E => Ok(Opcode::Free),
            0x0F => Ok(Opcode::Load),
            0x10 => Ok(Opcode::Store),
            0x11 => Ok(Opcode::LoadVar),
            0x12 => Ok(Opcode::StoreVar),
            0x13 => Ok(Opcode::Jump),
            0x14 => Ok(Opcode::JumpIfFalse),
            0x15 => Ok(Opcode::JumpIfTrue),
            0x16 => Ok(Opcode::Eq),
            0x17 => Ok(Opcode::Lt),
            0x18 => Ok(Opcode::Gt),
            0x19 => Ok(Opcode::Le),
            0x1A => Ok(Opcode::Ge),
            0x1B => Ok(Opcode::Ne),
            0x1C => Ok(Opcode::Call),
            0x1D => Ok(Opcode::Return),
            0x1E => Ok(Opcode::NewObject),
            0x1F => Ok(Opcode::GetField),
            0x20 => Ok(Opcode::SetField),
            0x21 => Ok(Opcode::CallMethod),
            0x22 => Ok(Opcode::IncRef),
            0x23 => Ok(Opcode::DecRef),
            0xFF => Ok(Opcode::Halt),
            _ => Err(()),
        }
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
    let mut file = File::open(filename).expect("Failed to open file");
    let mut bytecode = Vec::new();
    file.read_to_end(&mut bytecode).expect("Failed to read file");
    let mut vm = VM::new(bytecode, auto_memory);
    if let Err(e) = vm.run() {
        eprintln!("VM Error: {}", e);
        process::exit(1);
    }
}

// Note: Expanded with more features. In full:
// - Proper header parsing for constants, functions, classes (fields, methods).
// - Support floats in comparisons, strings eq/ne.
// - Classes with vtables for methods.
// - Global variables.
// - Error stack traces.
// - Integration with embedded code: Perhaps native calls.
// For demo, assumes i64-heavy, simple classes.
