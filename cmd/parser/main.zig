// parser.zig
// This is an expanded implementation of the 'parser' binary for vib-lang in Zig.
// It parses vib code into an AST and prints it (for demo).
// Updated syntax:
// - Imports: :std: instead of :biblioteka: std
// - Memory management: <automatic> or <manual>
// - Comments: -> line comments start with ->
// - Embedded code blocks #=lang={ code }.
// - Blocks use [].
// - Outputs AST as JSON-like string for simplicity.
// - Can be called by vib for parsing tasks.
// Expanded features:
// - Variables: let x = expr;
// - Assignments: x = expr;
// - If: [cond] [true_block] [false_block?]
// - While: [cond] [body]
// - Functions: fn name(params) [body]
// - Classes: class Name [methods]
// - More operators, calls, etc.

const std = @import("std");

const TokenType = enum {
    Identifier,
    Integer,
    Float,
    String,
    Colon,
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    Hash,
    Equal,
    LBrace,
    RBrace,
    Plus,
    Minus,
    Star,
    Slash,
    Lt,
    Gt,
    Le,
    Ge,
    EqEq,
    NotEq,
    Let,
    Fn,
    Class,
    If,
    While,
    Return,
    Write,
    Import,
    MemoryMode,
    EmbeddedStart,
    EmbeddedEnd,
    Assign,
    Semicolon,
    Comma,
    LParen,
    RParen,
    Arrow, // -> but skipped in comments
    EOF,
};

const Token = struct {
    typ: TokenType,
    literal: []const u8,
};

const Lexer = struct {
    input: []const u8,
    pos: usize = 0,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, input: []const u8) Lexer {
        return .{ .allocator = allocator, .input = input };
    }

    fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();
        try self.skipComment();
        if (self.pos >= self.input.len) return .{ .typ = .EOF, .literal = "" };
        const ch = self.input[self.pos];
        switch (ch) {
            ':' => {
                self.pos += 1;
                const lib = try self.readIdentifier();
                if (self.currentChar() == ':') {
                    self.pos += 1;
                    return .{ .typ = .Import, .literal = lib };
                } else {
                    self.pos -= lib.len + 1; // Rewind
                    return .{ .typ = .Colon, .literal = "" };
                }
            },
            '<' => {
                self.pos += 1;
                const mode = try self.readIdentifier();
                if (self.currentChar() == '>') {
                    self.pos += 1;
                    return .{ .typ = .MemoryMode, .literal = mode };
                } else {
                    return .{ .typ = .LAngle, .literal = "" };
                }
            },
            '>' => {
                self.pos += 1;
                return .{ .typ = .RAngle, .literal = "" };
            },
            '[' => {
                self.pos += 1;
                return .{ .typ = .LBracket, .literal = "" };
            },
            ']' => {
                self.pos += 1;
                return .{ .typ = .RBracket, .literal = "" };
            },
            '#' => {
                self.pos += 1;
                if (self.currentChar() == '=') {
                    self.pos += 1;
                    const lang = try self.readIdentifier();
                    if (self.currentChar() == '=') {
                        self.pos += 1;
                        return .{ .typ = .EmbeddedStart, .literal = lang };
                    } else {
                        return .{ .typ = .Hash, .literal = "" };
                    }
                } else {
                    return .{ .typ = .Hash, .literal = "" };
                }
            },
            '=' => {
                self.pos += 1;
                if (self.currentChar() == '=') {
                    self.pos += 1;
                    return .{ .typ = .EqEq, .literal = "" };
                } else {
                    return .{ .typ = .Assign, .literal = "" };
                }
            },
            '<' => {
                self.pos += 1;
                if (self.currentChar() == '=') {
                    self.pos += 1;
                    return .{ .typ = .Le, .literal = "" };
                } else {
                    return .{ .typ = .Lt, .literal = "" };
                }
            },
            '>' => {
                self.pos += 1;
                if (self.currentChar() == '=') {
                    self.pos += 1;
                    return .{ .typ = .Ge, .literal = "" };
                } else {
                    return .{ .typ = .Gt, .literal = "" };
                }
            },
            '!' => {
                self.pos += 1;
                if (self.currentChar() == '=') {
                    self.pos += 1;
                    return .{ .typ = .NotEq, .literal = "" };
                } else {
                    return .{ .typ = .EOF, .literal = "" };
                }
            },
            '{' => {
                self.pos += 1;
                return .{ .typ = .LBrace, .literal = "" };
            },
            '}' => {
                self.pos += 1;
                return .{ .typ = .EmbeddedEnd, .literal = "" };
            },
            '+' => {
                self.pos += 1;
                return .{ .typ = .Plus, .literal = "" };
            },
            '-' => {
                self.pos += 1;
                if (self.currentChar() == '>') {
                    self.pos += 1;
                    return .{ .typ = .Arrow, .literal = "" }; // But since skipComment handles
                } else {
                    return .{ .typ = .Minus, .literal = "" };
                }
            },
            '*' => {
                self.pos += 1;
                return .{ .typ = .Star, .literal = "" };
            },
            '/' => {
                self.pos += 1;
                return .{ .typ = .Slash, .literal = "" };
            },
            '(' => {
                self.pos += 1;
                return .{ .typ = .LParen, .literal = "" };
            },
            ')' => {
                self.pos += 1;
                return .{ .typ = .RParen, .literal = "" };
            },
            ';' => {
                self.pos += 1;
                return .{ .typ = .Semicolon, .literal = "" };
            },
            ',' => {
                self.pos += 1;
                return .{ .typ = .Comma, .literal = "" };
            },
            '"' => {
                return .{ .typ = .String, .literal = try self.readString() };
            },
            '0'...'9' => {
                return try self.readNumber();
            },
            'a'...'z', 'A'...'Z', '_' => {
                const ident = try self.readIdentifier();
                return switch (ident) {
                    "let" => .{ .typ = .Let, .literal = "" },
                    "fn" => .{ .typ = .Fn, .literal = "" },
                    "class" => .{ .typ = .Class, .literal = "" },
                    "if" => .{ .typ = .If, .literal = "" },
                    "while" => .{ .typ = .While, .literal = "" },
                    "return" => .{ .typ = .Return, .literal = "" },
                    "write" => .{ .typ = .Write, .literal = "" },
                    else => .{ .typ = .Identifier, .literal = ident },
                };
            },
            else => {
                self.pos += 1;
                return .{ .typ = .EOF, .literal = "" };
            },
        }
    }

    fn currentChar(self: *Lexer) u8 {
        if (self.pos < self.input.len) return self.input[self.pos] else return 0;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.pos < self.input.len and std.ascii.isWhitespace(self.input[self.pos])) {
            self.pos += 1;
        }
    }

    fn skipComment(self: *Lexer) !void {
        if (self.pos + 1 < self.input.len and self.input[self.pos] == '-' and self.input[self.pos + 1] == '>') {
            self.pos += 2;
            while (self.pos < self.input.len and self.input[self.pos] != '\n') {
                self.pos += 1;
            }
        }
    }

    fn readIdentifier(self: *Lexer) ![]const u8 {
        const start = self.pos;
        while (self.pos < self.input.len and (std.ascii.isAlphanumeric(self.input[self.pos]) or self.input[self.pos] == '_')) {
            self.pos += 1;
        }
        return try self.allocator.dupe(u8, self.input[start..self.pos]);
    }

    fn readNumber(self: *Lexer) !Token {
        const start = self.pos;
        while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
            self.pos += 1;
        }
        if (self.pos < self.input.len and self.input[self.pos] == '.') {
            self.pos += 1;
            while (self.pos < self.input.len and std.ascii.isDigit(self.input[self.pos])) {
                self.pos += 1;
            }
            return .{ .typ = .Float, .literal = try self.allocator.dupe(u8, self.input[start..self.pos]) };
        } else {
            return .{ .typ = .Integer, .literal = try self.allocator.dupe(u8, self.input[start..self.pos]) };
        }
    }

    fn readString(self: *Lexer) ![]const u8 {
        self.pos += 1; // Skip "
        const start = self.pos;
        while (self.pos < self.input.len and self.input[self.pos] != '"') {
            self.pos += 1;
        }
        const str = try self.allocator.dupe(u8, self.input[start..self.pos]);
        self.pos += 1; // Skip closing "
        return str;
    }
};

// AST Nodes
const AstNode = union(enum) {
    Block: std.ArrayList(AstNode),
    VarDecl: struct { name: []const u8, expr: *AstNode },
    Assign: struct { name: []const u8, expr: *AstNode },
    If: struct { cond: *AstNode, true_body: *AstNode, false_body: ?*AstNode },
    While: struct { cond: *AstNode, body: *AstNode },
    FnDef: struct { name: []const u8, params: std.ArrayList([]const u8), body: *AstNode },
    ClassDef: struct { name: []const u8, methods: std.ArrayList(AstNode) },
    Return: *AstNode,
    Write: *AstNode,
    BinaryOp: struct { op: u8, left: *AstNode, right: *AstNode },
    CompareOp: struct { op: []const u8, left: *AstNode, right: *AstNode },
    Call: struct { name: []const u8, args: std.ArrayList(AstNode) },
    Identifier: []const u8,
    Integer: i64,
    Float: f64,
    String: []const u8,
    Import: []const u8,
    MemoryMode: []const u8,
    Embedded: struct { lang: []const u8, code: []const u8 },
};

const Parser = struct {
    lexer: Lexer,
    current: Token,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, input: []const u8) !Parser {
        var lexer = Lexer.init(allocator, input);
        const current = try lexer.nextToken();
        return .{ .allocator = allocator, .lexer = lexer, .current = current };
    }

    fn parse(self: *Parser) !AstNode {
        var statements = std.ArrayList(AstNode).init(self.allocator);
        defer statements.deinit(); // But since returning Block, manage properly
        while (self.current.typ != .EOF) {
            const stmt = try self.parseStatement();
            try statements.append(stmt);
            try self.advance();
        }
        return .{ .Block = statements };
    }

    fn advance(self: *Parser) !void {
        self.current = try self.lexer.nextToken();
    }

    fn parseStatement(self: *Parser) !AstNode {
        return switch (self.current.typ) {
            .Let => try self.parseVarDecl(),
            .Fn => try self.parseFnDef(),
            .Class => try self.parseClassDef(),
            .Return => blk: {
                try self.advance();
                const expr = try self.parseExpr();
                try self.expect(.Semicolon);
                break :blk .{ .Return = try self.allocNode(expr) };
            },
            .Write => blk: {
                try self.advance();
                const expr = try self.parseExpr();
                try self.expect(.Semicolon);
                break :blk .{ .Write = try self.allocNode(expr) };
            },
            .Import => .{ .Import = self.current.literal },
            .MemoryMode => .{ .MemoryMode = self.current.literal },
            .EmbeddedStart => blk: {
                const lang = self.current.literal;
                try self.advance();
                try self.expect(.LBrace);
                const start = self.lexer.pos;
                while (self.current.typ != .EmbeddedEnd) {
                    try self.advance();
                }
                const code = try self.allocator.dupe(u8, self.lexer.input[start..self.lexer.pos - 1]);
                break :blk .{ .Embedded = .{ .lang = lang, .code = code } };
            },
            .LBracket => try self.parseBlockOrControl(),
            .Identifier => blk: {
                const id = self.current.literal;
                try self.advance();
                if (self.current.typ == .Assign) {
                    try self.advance();
                    const expr = try self.parseExpr();
                    try self.expect(.Semicolon);
                    break :blk .{ .Assign = .{ .name = id, .expr = try self.allocNode(expr) } };
                } else if (self.current.typ == .LParen) {
                    const args = try self.parseArgs();
                    try self.expect(.Semicolon);
                    break :blk .{ .Call = .{ .name = id, .args = args } };
                } else {
                    return error.ParseError;
                }
            },
            else => blk: {
                const expr = try self.parseExpr();
                try self.expect(.Semicolon);
                break :blk expr;
            },
        };
    }

    fn parseVarDecl(self: *Parser) !AstNode {
        try self.advance(); // Skip let
        if (self.current.typ == .Identifier) {
            const name = self.current.literal;
            try self.advance();
            try self.expect(.Assign);
            const expr = try self.parseExpr();
            try self.expect(.Semicolon);
            return .{ .VarDecl = .{ .name = name, .expr = try self.allocNode(expr) } };
        } else {
            return error.ParseError;
        }
    }

    fn parseFnDef(self: *Parser) !AstNode {
        try self.advance(); // Skip fn
        if (self.current.typ == .Identifier) {
            const name = self.current.literal;
            try self.advance();
            try self.expect(.LParen);
            const params = try self.parseParams();
            try self.expect(.RParen);
            const body = try self.parseBlock();
            return .{ .FnDef = .{ .name = name, .params = params, .body = try self.allocNode(body) } };
        } else {
            return error.ParseError;
        }
    }

    fn parseClassDef(self: *Parser) !AstNode {
        try self.advance(); // Skip class
        if (self.current.typ == .Identifier) {
            const name = self.current.literal;
            try self.advance();
            const block = try self.parseBlock();
            if (std.meta.activeTag(block) == .Block) {
                return .{ .ClassDef = .{ .name = name, .methods = block.Block } };
            } else {
                return error.ParseError;
            }
        } else {
            return error.ParseError;
        }
    }

    fn parseBlockOrControl(self: *Parser) !AstNode {
        try self.advance(); // Skip [
        const cond_or_stmt = try self.parseExpr();
        try self.expect(.RBracket);
        const true_body = try self.parseBlock();
        if (self.current.typ == .LBracket) {
            const false_body = try self.parseBlock();
            return .{ .If = .{ .cond = try self.allocNode(cond_or_stmt), .true_body = try self.allocNode(true_body), .false_body = try self.allocNode(false_body) } };
        } else {
            // Assume while if no false body
            return .{ .While = .{ .cond = try self.allocNode(cond_or_stmt), .body = try self.allocNode(true_body) } };
        }
    }

    fn parseBlock(self: *Parser) !AstNode {
        try self.expect(.LBracket);
        var stmts = std.ArrayList(AstNode).init(self.allocator);
        while (self.current.typ != .RBracket and self.current.typ != .EOF) {
            const stmt = try self.parseStatement();
            try stmts.append(stmt);
        }
        try self.expect(.RBracket);
        return .{ .Block = stmts };
    }

    fn parseParams(self: *Parser) !std.ArrayList([]const u8) {
        var params = std.ArrayList([]const u8).init(self.allocator);
        if (self.current.typ != .RParen) {
            if (self.current.typ == .Identifier) {
                try params.append(self.current.literal);
                try self.advance();
            }
            while (self.current.typ == .Comma) {
                try self.advance();
                if (self.current.typ == .Identifier) {
                    try params.append(self.current.literal);
                    try self.advance();
                }
            }
        }
        return params;
    }

    fn parseArgs(self: *Parser) !std.ArrayList(AstNode) {
        try self.advance(); // Skip (
        var args = std.ArrayList(AstNode).init(self.allocator);
        if (self.current.typ != .RParen) {
            const arg = try self.parseExpr();
            try args.append(arg);
            while (self.current.typ == .Comma) {
                try self.advance();
                const next_arg = try self.parseExpr();
                try args.append(next_arg);
            }
        }
        try self.expect(.RParen);
        return args;
    }

    fn expect(self: *Parser, expected: TokenType) !void {
        if (self.current.typ == expected) {
            try self.advance();
        } else {
            return error.ParseError;
        }
    }

    fn parseExpr(self: *Parser) !AstNode {
        var left = try self.parseTerm();
        while (self.current.typ == .Plus or self.current.typ == .Minus) {
            const op = switch (self.current.typ) {
                .Plus => '+',
                .Minus => '-',
                else => unreachable,
            };
            try self.advance();
            const right = try self.parseTerm();
            left = .{ .BinaryOp = .{ .op = op, .left = try self.allocNode(left), .right = try self.allocNode(right) } };
        }
        return left;
    }

    fn parseTerm(self: *Parser) !AstNode {
        var left = try self.parsePrimary();
        while (self.current.typ == .Star or self.current.typ == .Slash) {
            const op = switch (self.current.typ) {
                .Star => '*',
                .Slash => '/',
                else => unreachable,
            };
            try self.advance();
            const right = try self.parsePrimary();
            left = .{ .BinaryOp = .{ .op = op, .left = try self.allocNode(left), .right = try self.allocNode(right) } };
        }
        return left;
    }

    fn parsePrimary(self: *Parser) !AstNode {
        return switch (self.current.typ) {
            .Integer => blk: {
                const val = try std.fmt.parseInt(i64, self.current.literal, 10);
                try self.advance();
                break :blk .{ .Integer = val };
            },
            .Float => blk: {
                const val = try std.fmt.parseFloat(f64, self.current.literal);
                try self.advance();
                break :blk .{ .Float = val };
            },
            .String => blk: {
                const val = self.current.literal;
                try self.advance();
                break :blk .{ .String = val };
            },
            .Identifier => blk: {
                const id = self.current.literal;
                try self.advance();
                if (self.current.typ == .LParen) {
                    const args = try self.parseArgs();
                    break :blk .{ .Call = .{ .name = id, .args = args } };
                } else {
                    break :blk .{ .Identifier = id };
                }
            },
            .LParen => blk: {
                try self.advance();
                const expr = try self.parseExpr();
                try self.expect(.RParen);
                break :blk expr;
            },
            else => error.ParseError,
        };
    }

    fn allocNode(self: *Parser, node: AstNode) !*AstNode {
        const ptr = try self.allocator.create(AstNode);
        ptr.* = node;
        return ptr;
    }
};

// Print AST (recursive)
fn printAst(node: AstNode, writer: anytype, depth: usize) !void {
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        try writer.print("  ", .{});
    }
    switch (node) {
        .Block => |block| {
            try writer.print("Block: [\n", .{});
            for (block.items) |stmt| {
                try printAst(stmt, writer, depth + 1);
            }
            i = 0;
            while (i < depth) : (i += 1) {
                try writer.print("  ", .{});
            }
            try writer.print("]\n", .{});
        },
        .VarDecl => |decl| {
            try writer.print("VarDecl: {s} = ", .{decl.name});
            try printAst(decl.expr.*, writer, 0);
        },
        .Assign => |assign| {
            try writer.print("Assign: {s} = ", .{assign.name});
            try printAst(assign.expr.*, writer, 0);
        },
        .If => |if_stmt| {
            try writer.print("If: cond=", .{});
            try printAst(if_stmt.cond.*, writer, 0);
            try writer.print("\ntrue_body=\n", .{});
            try printAst(if_stmt.true_body.*, writer, depth + 1);
            if (if_stmt.false_body) |fb| {
                try writer.print("false_body=\n", .{});
                try printAst(fb.*, writer, depth + 1);
            }
        },
        .While => |while_stmt| {
            try writer.print("While: cond=", .{});
            try printAst(while_stmt.cond.*, writer, 0);
            try writer.print("\nbody=\n", .{});
            try printAst(while_stmt.body.*, writer, depth + 1);
        },
        .FnDef => |fn_def| {
            try writer.print("FnDef: {s}(", .{fn_def.name});
            for (fn_def.params.items, 0..) |param, idx| {
                try writer.print("{s}", .{param});
                if (idx < fn_def.params.items.len - 1) try writer.print(", ", .{});
            }
            try writer.print(")\n", .{});
            try printAst(fn_def.body.*, writer, depth + 1);
        },
        .ClassDef => |class_def| {
            try writer.print("ClassDef: {s} [\n", .{class_def.name});
            for (class_def.methods.items) |method| {
                try printAst(method, writer, depth + 1);
            }
            i = 0;
            while (i < depth) : (i += 1) {
                try writer.print("  ", .{});
            }
            try writer.print("]\n", .{});
        },
        .Return => |ret| {
            try writer.print("Return: ", .{});
            try printAst(ret.*, writer, 0);
            try writer.print("\n", .{});
        },
        .Write => |write| {
            try writer.print("Write: ", .{});
            try printAst(write.*, writer, 0);
            try writer.print("\n", .{});
        },
        .BinaryOp => |op| {
            try writer.print("BinaryOp '{c}': \n", .{op.op});
            try printAst(op.left.*, writer, depth + 1);
            try printAst(op.right.*, writer, depth + 1);
        },
        .CompareOp => |op| {
            try writer.print("CompareOp \"{s}\": \n", .{op.op});
            try printAst(op.left.*, writer, depth + 1);
            try printAst(op.right.*, writer, depth + 1);
        },
        .Call => |call| {
            try writer.print("Call: {s}(", .{call.name});
            for (call.args.items, 0..) |arg, idx| {
                try printAst(arg, writer, 0);
                if (idx < call.args.items.len - 1) try writer.print(", ", .{});
            }
            try writer.print(")\n", .{});
        },
        .Identifier => |name| try writer.print("Identifier: {s}\n", .{name}),
        .Integer => |val| try writer.print("Integer: {}\n", .{val}),
        .Float => |val| try writer.print("Float: {d}\n", .{val}),
        .String => |val| try writer.print("String: \"{s}\"\n", .{val}),
        .Import => |lib| try writer.print("Import: {s}\n", .{lib}),
        .MemoryMode => |mode| try writer.print("MemoryMode: {s}\n", .{mode}),
        .Embedded => |emb| try writer.print("Embedded {s}: {s}\n", .{emb.lang, emb.code}),
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: parser <input.vib>\n", .{});
        return;
    }
    const input_file = args[1];
    const input = try std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024);
    defer allocator.free(input);
    var parser = try Parser.init(allocator, input);
    const ast = try parser.parse();
    const stdout = std.io.getStdOut().writer();
    try printAst(ast, stdout, 0);
    // TODO: Deallocate AST recursively
}

// Note: Expanded with more syntax. In full, implement deallocation for all nodes recursively.
// Better error handling, support comparisons in parseExpr (add parseCompare level).
// For comparisons, add to parseExpr similar to binary ops, using CompareOp.
