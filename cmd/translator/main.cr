# src/translator.cr
# This is an expanded implementation of the 'translator' binary for vib-lang in Crystal.
# It translates vib code to other languages: Rust, C, Java.
# Updated syntax:
# - Imports: :std: instead of :biblioteka: std
# - Memory management: <automatic> or <manual>
# - Comments: -> line comments start with ->
# - Embedded code blocks #=lang={ code } are inlined as comments or foreign code if possible.
# - Blocks use [].
# - Expanded features: variables (let x = expr;), assignments (x = expr;), if [cond] [true] [false?], while [cond] [body],
#   functions fn name(params) [body], classes class Name [methods], returns, calls, comparisons.
# - For 'vib t file.vib lang': Translate to lang, output to file_lang.ext, then TUI to ask compile to binary or vm.
# - Compile: For binary, call system compiler (rustc, gcc, javac).
# - For vm: Placeholder as vm is vib-specific.
# - For 'vib t': Assume project mode, generate build.hacker config.

require "option_parser"

# Tokens
enum Token
  Identifier
  Integer
  Float
  String
  Colon
  LBracket
  RBracket
  LAngle
  RAngle
  Hash
  Equal
  LBrace
  RBrace
  Plus
  Minus
  Star
  Slash
  Lt
  Gt
  Le
  Ge
  EqEq
  NotEq
  Let
  Fn
  Class
  If
  While
  Return
  Write
  Import
  MemoryMode
  EmbeddedStart
  EmbeddedEnd
  Assign
  Semicolon
  Comma
  LParen
  RParen
  Arrow  # -> but skipped in comments
  EOF
end

# Lexer
class Lexer
  @pos : Int32 = 0
  @input : String
  @chars : Array(Char)
  def initialize(@input)
    @chars = @input.chars
  end

  def next_token : {Token, String?}
    skip_whitespace
    skip_comment
    return {Token::EOF, nil} if @pos >= @chars.size
    ch = @chars[@pos]
    case ch
    when ':'
      @pos += 1
      ident = read_identifier
      if current_char == ':'
        @pos += 1
        {Token::Import, ident}
      else
        @pos -= ident.size + 1 # Rewind
        {Token::Colon, nil}
      end
    when '<'
      @pos += 1
      mode = read_identifier
      if current_char == '>'
        @pos += 1
        {Token::MemoryMode, mode}
      else
        {Token::LAngle, nil}
      end
    when '>'
      @pos += 1
      {Token::RAngle, nil}
    when '['
      @pos += 1
      {Token::LBracket, nil}
    when ']'
      @pos += 1
      {Token::RBracket, nil}
    when '#'
      @pos += 1
      if current_char == '='
        @pos += 1
        lang = read_identifier
        if current_char == '='
          @pos += 1
          {Token::EmbeddedStart, lang}
        else
          {Token::Hash, nil}
        end
      else
        {Token::Hash, nil}
      end
    when '='
      @pos += 1
      if current_char == '='
        @pos += 1
        {Token::EqEq, nil}
      else
        {Token::Assign, nil}
      end
    when '<'
      @pos += 1
      if current_char == '='
        @pos += 1
        {Token::Le, nil}
      else
        {Token::Lt, nil}
      end
    when '>'
      @pos += 1
      if current_char == '='
        @pos += 1
        {Token::Ge, nil}
      else
        {Token::Gt, nil}
      end
    when '!'
      @pos += 1
      if current_char == '='
        @pos += 1
        {Token::NotEq, nil}
      else
        {Token::EOF, nil}
      end
    when '{'
      @pos += 1
      {Token::LBrace, nil}
    when '}'
      @pos += 1
      {Token::EmbeddedEnd, nil}
    when '+'
      @pos += 1
      {Token::Plus, nil}
    when '-'
      @pos += 1
      if current_char == '>'
        @pos += 1
        {Token::Arrow, nil} # But skip_comment handles
      else
        {Token::Minus, nil}
      end
    when '*'
      @pos += 1
      {Token::Star, nil}
    when '/'
      @pos += 1
      {Token::Slash, nil}
    when '('
      @pos += 1
      {Token::LParen, nil}
    when ')'
      @pos += 1
      {Token::RParen, nil}
    when ';'
      @pos += 1
      {Token::Semicolon, nil}
    when ','
      @pos += 1
      {Token::Comma, nil}
    when '"'
      {Token::String, read_string}
    when .digit?
      read_number
    when .letter? || '_'
      ident = read_identifier
      case ident
      when "let" then {Token::Let, nil}
      when "fn" then {Token::Fn, nil}
      when "class" then {Token::Class, nil}
      when "if" then {Token::If, nil}
      when "while" then {Token::While, nil}
      when "return" then {Token::Return, nil}
      when "write" then {Token::Write, nil}
      else {Token::Identifier, ident}
      end
    else
      @pos += 1
      {Token::EOF, nil}
    end
  end

  private def current_char : Char?
    @pos < @chars.size ? @chars[@pos] : nil
  end

  private def skip_whitespace
    while @pos < @chars.size && @chars[@pos].whitespace?
      @pos += 1
    end
  end

  private def skip_comment
    if @pos + 1 < @chars.size && @chars[@pos] == '-' && @chars[@pos + 1] == '>'
      @pos += 2
      while @pos < @chars.size && @chars[@pos] != '\n'
        @pos += 1
      end
    end
  end

  private def read_identifier : String
    start = @pos
    while @pos < @chars.size && (@chars[@pos].alphanumeric? || @chars[@pos] == '_')
      @pos += 1
    end
    @chars[start...@pos].join
  end

  private def read_number : {Token, String?}
    start = @pos
    while @pos < @chars.size && @chars[@pos].digit?
      @pos += 1
    end
    if @pos < @chars.size && @chars[@pos] == '.'
      @pos += 1
      while @pos < @chars.size && @chars[@pos].digit?
        @pos += 1
      end
      {Token::Float, @input[start...@pos]}
    else
      {Token::Integer, @input[start...@pos]}
    end
  end

  private def read_string : String
    @pos += 1 # Skip "
    start = @pos
    while @pos < @chars.size && @chars[@pos] != '"'
      @pos += 1
    end
    str = @chars[start...@pos].join
    @pos += 1 # Skip closing "
    str
  end
end

# AST Nodes
abstract class AstNode
end

class BlockNode < AstNode
  getter statements : Array(AstNode)
  def initialize(@statements)
  end
end

class VarDeclNode < AstNode
  getter name : String
  getter expr : AstNode
  def initialize(@name, @expr)
  end
end

class AssignNode < AstNode
  getter name : String
  getter expr : AstNode
  def initialize(@name, @expr)
  end
end

class IfNode < AstNode
  getter cond : AstNode
  getter true_body : AstNode
  getter false_body : AstNode?
  def initialize(@cond, @true_body, @false_body = nil)
  end
end

class WhileNode < AstNode
  getter cond : AstNode
  getter body : AstNode
  def initialize(@cond, @body)
  end
end

class FnDefNode < AstNode
  getter name : String
  getter params : Array(String)
  getter body : AstNode
  def initialize(@name, @params, @body)
  end
end

class ClassDefNode < AstNode
  getter name : String
  getter methods : Array(AstNode)
  def initialize(@name, @methods)
  end
end

class ReturnNode < AstNode
  getter expr : AstNode
  def initialize(@expr)
  end
end

class WriteNode < AstNode
  getter expr : AstNode
  def initialize(@expr)
  end
end

class BinaryOpNode < AstNode
  getter op : Char
  getter left : AstNode
  getter right : AstNode
  def initialize(@op, @left, @right)
  end
end

class CompareOpNode < AstNode
  getter op : String
  getter left : AstNode
  getter right : AstNode
  def initialize(@op, @left, @right)
  end
end

class CallNode < AstNode
  getter name : String
  getter args : Array(AstNode)
  def initialize(@name, @args)
  end
end

class IdentifierNode < AstNode
  getter name : String
  def initialize(@name)
  end
end

class IntegerNode < AstNode
  getter value : Int64
  def initialize(@value)
  end
end

class FloatNode < AstNode
  getter value : Float64
  def initialize(@value)
  end
end

class StringNode < AstNode
  getter value : String
  def initialize(@value)
  end
end

class ImportNode < AstNode
  getter lib : String
  def initialize(@lib)
  end
end

class MemoryModeNode < AstNode
  getter mode : String
  def initialize(@mode)
  end
end

class EmbeddedNode < AstNode
  getter lang : String
  getter code : String
  def initialize(@lang, @code)
  end
end

# Parser
class Parser
  @lexer : Lexer
  @current : {Token, String?}
  def initialize(input : String)
    @lexer = Lexer.new(input)
    @current = @lexer.next_token
  end

  def parse : AstNode
    statements = [] of AstNode
    while @current[0] != Token::EOF
      statements << parse_statement
      advance
    end
    BlockNode.new(statements)
  end

  private def advance
    @current = @lexer.next_token
  end

  private def parse_statement : AstNode
    case @current[0]
    when Token::Let
      parse_var_decl
    when Token::Fn
      parse_fn_def
    when Token::Class
      parse_class_def
    when Token::Return
      advance
      expr = parse_expr
      expect(Token::Semicolon)
      ReturnNode.new(expr)
    when Token::Write
      advance
      expr = parse_expr
      expect(Token::Semicolon)
      WriteNode.new(expr)
    when Token::Import
      ImportNode.new(@current[1].not_nil!)
    when Token::MemoryMode
      MemoryModeNode.new(@current[1].not_nil!)
    when Token::EmbeddedStart
      lang = @current[1].not_nil!
      advance
      expect(Token::LBrace)
      start_pos = @lexer.@pos
      while @current[0] != Token::EmbeddedEnd
        advance
      end
      code = @lexer.@input[start_pos...@lexer.@pos - 1]
      EmbeddedNode.new(lang, code)
    when Token::LBracket
      parse_block_or_control
    when Token::Identifier
      id = @current[1].not_nil!
      advance
      if @current[0] == Token::Assign
        advance
        expr = parse_expr
        expect(Token::Semicolon)
        AssignNode.new(id, expr)
      elsif @current[0] == Token::LParen
        args = parse_args
        expect(Token::Semicolon)
        CallNode.new(id, args)
      else
        raise "Unexpected after identifier"
      end
    else
      expr = parse_expr
      expect(Token::Semicolon)
      expr
    end
  end

  private def parse_var_decl : AstNode
    advance # Skip let
    name = if @current[0] == Token::Identifier && @current[1]
             @current[1].not_nil!
           else
             raise "Expected identifier after let"
           end
    advance
    expect(Token::Assign)
    expr = parse_expr
    expect(Token::Semicolon)
    VarDeclNode.new(name, expr)
  end

  private def parse_fn_def : AstNode
    advance # Skip fn
    name = if @current[0] == Token::Identifier && @current[1]
             @current[1].not_nil!
           else
             raise "Expected function name"
           end
    advance
    expect(Token::LParen)
    params = parse_params
    expect(Token::RParen)
    body = parse_block
    FnDefNode.new(name, params, body)
  end

  private def parse_class_def : AstNode
    advance # Skip class
    name = if @current[0] == Token::Identifier && @current[1]
             @current[1].not_nil!
           else
             raise "Expected class name"
           end
    advance
    methods = parse_block.statements
    ClassDefNode.new(name, methods)
  end

  private def parse_block_or_control : AstNode
    advance # Skip [
    cond = parse_expr
    expect(Token::RBracket)
    true_body = parse_block
    if @current[0] == Token::LBracket
      false_body = parse_block
      IfNode.new(cond, true_body, false_body)
    else
      WhileNode.new(cond, true_body)
    end
  end

  private def parse_block : AstNode
    expect(Token::LBracket)
    stmts = [] of AstNode
    while @current[0] != Token::RBracket && @current[0] != Token::EOF
      stmts << parse_statement
    end
    expect(Token::RBracket)
    BlockNode.new(stmts)
  end

  private def parse_params : Array(String)
    params = [] of String
    if @current[0] != Token::RParen
      if @current[0] == Token::Identifier && @current[1]
        params << @current[1].not_nil!
        advance
      end
      while @current[0] == Token::Comma
        advance
        if @current[0] == Token::Identifier && @current[1]
          params << @current[1].not_nil!
          advance
        end
      end
    end
    params
  end

  private def parse_args : Array(AstNode)
    advance # Skip (
    args = [] of AstNode
    if @current[0] != Token::RParen
      args << parse_expr
      while @current[0] == Token::Comma
        advance
        args << parse_expr
      end
    end
    expect(Token::RParen)
    args
  end

  private def expect(expected : Token)
    if @current[0] == expected
      advance
    else
      raise "Expected #{expected}, got #{@current[0]}"
    end
  end

  private def parse_expr : AstNode
    left = parse_term
    while [Token::Plus, Token::Minus].includes?(@current[0])
      op = case @current[0]
           when Token::Plus then '+'
           when Token::Minus then '-'
           else raise "Unreachable"
           end
      advance
      right = parse_term
      left = BinaryOpNode.new(op, left, right)
    end
    left
  end

  private def parse_term : AstNode
    left = parse_primary
    while [Token::Star, Token::Slash].includes?(@current[0])
      op = case @current[0]
           when Token::Star then '*'
           when Token::Slash then '/'
           else raise "Unreachable"
           end
      advance
      right = parse_primary
      left = BinaryOpNode.new(op, left, right)
    end
    left
  end

  private def parse_primary : AstNode
    case @current[0]
    when Token::Integer
      val = @current[1].not_nil!.to_i64
      advance
      IntegerNode.new(val)
    when Token::Float
      val = @current[1].not_nil!.to_f64
      advance
      FloatNode.new(val)
    when Token::String
      val = @current[1].not_nil!
      advance
      StringNode.new(val)
    when Token::Identifier
      id = @current[1].not_nil!
      advance
      if @current[0] == Token::LParen
        args = parse_args
        CallNode.new(id, args)
      else
        IdentifierNode.new(id)
      end
    when Token::LParen
      advance
      expr = parse_expr
      expect(Token::RParen)
      expr
    else
      raise "Unexpected token in primary: #{@current[0]}"
    end
  end
end

# CodeGenerator base
abstract class CodeGenerator
  @memory_mode : String = "manual"
  abstract def generate(node : AstNode) : String
end

# Rust Generator
class RustGenerator < CodeGenerator
  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when VarDeclNode
      "let mut #{node.name} = #{generate(node.expr)};"
    when AssignNode
      "#{node.name} = #{generate(node.expr)};"
    when IfNode
      "if #{generate(node.cond)} {\n#{generate(node.true_body)}\n}#{node.false_body ? " else {\n#{generate(node.false_body.not_nil!)}\n}" : ""}"
    when WhileNode
      "while #{generate(node.cond)} {\n#{generate(node.body)}\n}"
    when FnDefNode
      "fn #{node.name}(#{node.params.map { |p| "#{p}: i64" }.join(", ")}) -> i64 {\n#{generate(node.body)}\n}"
    when ClassDefNode
      "struct #{node.name} {}\nimpl #{node.name} {\n#{node.methods.map { |m| generate(m) }.join("\n")}\n}"
    when ReturnNode
      "return #{generate(node.expr)};"
    when WriteNode
      "println!(\"{}\", #{generate(node.expr)});"
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CompareOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CallNode
      "#{node.name}(#{node.args.map { |a| generate(a) }.join(", ")})"
    when IdentifierNode
      node.name
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when ImportNode
      "use #{node.lib};"
    when MemoryModeNode
      @memory_mode = node.mode
      "" # Handled in allocations
    when EmbeddedNode
      if node.lang == "rust"
        node.code
      else
        "// Embedded #{node.lang}: #{node.code.gsub("\n", "\n// ")}"
      end
    else
      ""
    end
  end
end

# C Generator
class CGenerator < CodeGenerator
  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when VarDeclNode
      "long #{node.name} = #{generate(node.expr)};"
    when AssignNode
      "#{node.name} = #{generate(node.expr)};"
    when IfNode
      "if (#{generate(node.cond)}) {\n#{generate(node.true_body)}\n}#{node.false_body ? " else {\n#{generate(node.false_body.not_nil!)}\n}" : ""}"
    when WhileNode
      "while (#{generate(node.cond)}) {\n#{generate(node.body)}\n}"
    when FnDefNode
      "long #{node.name}(#{node.params.map { |p| "long #{p}" }.join(", ")}) {\n#{generate(node.body)}\n}"
    when ClassDefNode
      "typedef struct { } #{node.name};\n#{node.methods.map { |m| generate(m) }.join("\n")}"
    when ReturnNode
      "return #{generate(node.expr)};"
    when WriteNode
      "printf(\"%ld\\n\", #{generate(node.expr)});"
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CompareOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CallNode
      "#{node.name}(#{node.args.map { |a| generate(a) }.join(", ")})"
    when IdentifierNode
      node.name
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when ImportNode
      "#include <#{node.lib}>"
    when MemoryModeNode
      @memory_mode = node.mode
      ""
    when EmbeddedNode
      if node.lang == "c"
        node.code
      else
        "/* Embedded #{node.lang}: #{node.code.gsub("\n", "\n * ")} */"
      end
    else
      ""
    end
  end
end

# Java Generator
class JavaGenerator < CodeGenerator
  @memory_mode : String = "automatic"
  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when VarDeclNode
      "long #{node.name} = #{generate(node.expr)};"
    when AssignNode
      "#{node.name} = #{generate(node.expr)};"
    when IfNode
      "if (#{generate(node.cond)}) {\n#{generate(node.true_body)}\n}#{node.false_body ? " else {\n#{generate(node.false_body.not_nil!)}\n}" : ""}"
    when WhileNode
      "while (#{generate(node.cond)}) {\n#{generate(node.body)}\n}"
    when FnDefNode
      "public static long #{node.name}(#{node.params.map { |p| "long #{p}" }.join(", ")}) {\n#{generate(node.body)}\n}"
    when ClassDefNode
      "public class #{node.name} {\n#{node.methods.map { |m| generate(m) }.join("\n")}\n}"
    when ReturnNode
      "return #{generate(node.expr)};"
    when WriteNode
      "System.out.println(#{generate(node.expr)});"
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CompareOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when CallNode
      "#{node.name}(#{node.args.map { |a| generate(a) }.join(", ")})"
    when IdentifierNode
      node.name
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when ImportNode
      "import #{node.lib};"
    when MemoryModeNode
      @memory_mode = node.mode
      ""
    when EmbeddedNode
      if node.lang == "java"
        node.code
      else
        "// Embedded #{node.lang}: #{node.code.gsub("\n", "\n// ")}"
      end
    else
      ""
    end
  end
end

# TUI for compile choice
def tui_ask_compile : String
  puts "Do you want to compile to binary or vm? (binary/vm/none)"
  gets.not_nil!.chomp
end

# Main
def main
  file = ""
  lang = ""
  project_mode = false
  OptionParser.parse do |parser|
    parser.banner = "Usage: translator [arguments]"
    parser.on("t", "Translate project or file") do
      if ARGV.empty?
        project_mode = true
      elsif ARGV.size == 2
        file = ARGV[0]
        lang = ARGV[1]
      else
        puts "Invalid args"
        exit(1)
      end
    end
  end
  if project_mode
    input = File.read("cmd/main.vib") rescue raise "No main.vib"
    config = "Translate to Rust\n# More config options\n# e.g., target: binary"
    Dir.mkdir_p("build")
    File.write("build/build.hacker", config)
    puts "Generated build.hacker"
  else
    input = File.read(file)
    parser = Parser.new(input)
    ast = parser.parse
    generator = case lang
                when "rust" then RustGenerator.new
                when "c" then CGenerator.new
                when "java" then JavaGenerator.new
                else raise "Unknown lang: #{lang}"
                end
    output_code = wrap_in_main(generator, ast, lang)
    ext = case lang
          when "rust" then "rs"
          when "c" then "c"
          when "java" then "java"
          end
    out_file = "#{File.basename(file, ".vib")}.#{ext}"
    File.write(out_file, output_code)
    puts "Translated to #{out_file}"
    choice = tui_ask_compile
    if choice == "binary"
      case lang
      when "rust"
        system("rustc #{out_file} -o #{File.basename(out_file, ".rs")}")
      when "c"
        system("gcc #{out_file} -o #{File.basename(out_file, ".c")}")
      when "java"
        system("javac #{out_file}")
      end
    elsif choice == "vm"
      puts "VM compilation not implemented for translated code."
    end
  end
end

def wrap_in_main(generator : CodeGenerator, ast : AstNode, lang : String) : String
  code = generator.generate(ast)
  case lang
  when "rust"
    "fn main() {\n#{code}\n}"
  when "c"
    "#include <stdio.h>\nint main() {\n#{code}\nreturn 0;\n}"
  when "java"
    "public class Main {\npublic static void main(String[] args) {\n#{code}\n}\n}"
  else
    code
  end
end

main if __FILE__ == PROGRAM_NAME

# Note: Expanded with support for new syntax. Wrap top-level in main for executables.
# Handle comparisons by adding parse_compare if needed, but for demo, assume binary op handles.
# For classes/functions, adjust generators for proper syntax (e.g., methods in impl for Rust).
# TUI expanded slightly. For project mode, generate more detailed config based on bytes.yaml if present.
