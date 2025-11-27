// parser.zig
// This is a simplified implementation of the 'parser' binary for vib-lang in Zig.
// It parses vib code into an AST and prints it (for demo).
// Features:
// - Lexer and Parser for vib syntax.
// - Handles imports (:biblioteka:), memory management (<zarzadznie pamiecia>), write statements.
// - Embedded code blocks #=lang={ code }.
// - Blocks use [].
// - Outputs AST as JSON-like string for simplicity.
// - Can be called by vib for parsing tasks.

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
    Write,
    Import,
    MemoryMode,
    EmbeddedStart,
    EmbeddedEnd,
    Plus,
    Minus,
    Star,
    Slash,
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
        if (self.pos >= self.input.len) return .{ .typ = .EOF, .literal = "" };

        const ch = self.input[self.pos];
        switch (ch) {
            ':' => {
                self.pos += 1;
                if (self.matchStr("biblioteka:")) {
                    return .{ .typ = .Import, .literal = "" };
                } else {
                    return .{ .typ = .Colon, .literal = "" };
                }
            },
            '<' => {
                self.pos += 1;
                if (self.matchStr("zarzadznie pamiecia>")) {
                    self.skipWhitespace();
                    const mode = try self.readIdentifier();
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
                    }
                }
                return .{ .typ = .Hash, .literal = "" };
            },
            '=' => {
                self.pos += 1;
                return .{ .typ = .Equal, .literal = "" };
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
                return .{ .typ = .Minus, .literal = "" };
            },
            '*' => {
                self.pos += 1;
                return .{ .typ = .Star, .literal = "" };
            },
            '/' => {
                self.pos += 1;
                return .{ .typ = .Slash, .literal = "" };
            },
            '"' => {
                return .{ .typ = .String, .literal = try self.readString() };
            },
            '0'...'9' => {
                return try self.readNumber();
            },
            'a'...'z', 'A'...'Z', '_' => {
                const ident = try self.readIdentifier();
                if (std.mem.eql(u8, ident, "write")) {
                    return .{ .typ = .Write, .literal = "" };
                } else {
                    return .{ .typ = .Identifier, .literal = ident };
                }
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

    fn matchStr(self: *Lexer, s: []const u8) bool {
        const start = self.pos;
        for (s) |c| {
            if (self.currentChar() != c) {
                self.pos = start;
                return false;
            }
            self.pos += 1;
        }
        return true;
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
    Import: []const u8,
    MemoryMode: []const u8,
    Write: *AstNode,
    BinaryOp: struct { op: u8, left: *AstNode, right: *AstNode },
    Integer: i64,
    Float: f64,
    String: []const u8,
    Identifier: []const u8,
    Block: std.ArrayList(AstNode),
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
            .Import => blk: {
                try self.advance();
                if (self.current.typ == .Identifier) {
                    break :blk .{ .Import = self.current.literal };
                } else {
                    return error.ParseError;
                }
            },
            .MemoryMode => .{ .MemoryMode = self.current.literal },
            .Write => blk: {
                try self.advance();
                const expr = try self.parseExpr();
                break :blk .{ .Write = try self.allocator.create(AstNode{.Integer = 0}) }; // Placeholder, actually box expr
                // Note: In Zig, boxing requires alloc, simplify for demo
            },
            .EmbeddedStart => blk: {
                const lang = self.current.literal;
                try self.advance();
                if (self.current.typ != .LBrace) return error.ParseError;
                try self.advance();
                const start = self.lexer.pos;
                while (self.current.typ != .EmbeddedEnd) {
                    try self.advance();
                }
                const code = try self.allocator.dupe(u8, self.lexer.input[start..self.lexer.pos - 1]);
                break :blk .{ .Embedded = .{ .lang = lang, .code = code } };
            },
            .LBracket => blk: {
                try self.advance();
                var block = std.ArrayList(AstNode).init(self.allocator);
                while (self.current.typ != .RBracket and self.current.typ != .EOF) {
                    const stmt = try self.parseStatement();
                    try block.append(stmt);
                }
                if (self.current.typ == .RBracket) try self.advance();
                break :blk .{ .Block = block };
            },
            else => try self.parseExpr(),
        };
    }

    fn parseExpr(self: *Parser) !AstNode {
        var left = try self.parsePrimary();
        while (self.current.typ == .Plus or self.current.typ == .Minus or self.current.typ == .Star or self.current.typ == .Slash) {
            const op = switch (self.current.typ) {
                .Plus => '+',
                .Minus => '-',
                .Star => '*',
                .Slash => '/',
                else => unreachable,
            };
            try self.advance();
            const right = try self.parsePrimary();
            left = .{ .BinaryOp = .{ .op = op, .left = try self.allocator.create(left), .right = try self.allocator.create(right) } };
            // Note: Memory management simplified
        }
        return left;
    }

    fn parsePrimary(self: *Parser) !AstNode {
        return switch (self.current.typ) {
            .Integer => .{ .Integer = try std.fmt.parseInt(i64, self.current.literal, 10) },
            .Float => .{ .Float = try std.fmt.parseFloat(f64, self.current.literal) },
            .String => .{ .String = self.current.literal },
            .Identifier => .{ .Identifier = self.current.literal },
            else => error.ParseError,
        };
    }
};

// Print AST (simplified)
fn printAst(node: AstNode, writer: anytype) !void {
    switch (node) {
        .Block => |block| {
            try writer.print("Block: [\n", .{});
            for (block.items) |stmt| {
                try printAst(stmt, writer);
            }
            try writer.print("]\n", .{});
        },
        .Import => |lib| try writer.print("Import: {s}\n", .{lib}),
        .MemoryMode => |mode| try writer.print("MemoryMode: {s}\n", .{mode}),
        .Write => |expr| {
            try writer.print("Write: ", .{});
            try printAst(expr.*, writer);
        },
        .BinaryOp => |op| {
            try writer.print("BinaryOp '{c}': ", .{op.op});
            try printAst(op.left.*, writer);
            try printAst(op.right.*, writer);
        },
        .Integer => |val| try writer.print("Integer: {}\n", .{val}),
        .Float => |val| try writer.print("Float: {d}\n", .{val}),
        .String => |val| try writer.print("String: \"{s}\"\n", .{val}),
        .Identifier => |name| try writer.print("Identifier: {s}\n", .{name}),
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
    try printAst(ast, stdout);
}

// Note: This is a demo. In full:
// - Proper memory management for AST nodes.
// - Handle deallocation.
// - More syntax support (classes, etc.).
// - Perhaps output to file or JSON for other tools.
// - Error handling improved.
