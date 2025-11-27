# src/translator.cr
# This is a simplified implementation of the 'translator' binary for vib-lang in Crystal.
# It translates vib code to other languages: Rust, C, Java.
# Features:
# - Lexer and Parser for vib syntax (similar to the Rust compiler).
# - Codegen to source code in target language.
# - Handles imports (:biblioteka:), memory management (<zarzadznie pamiecia>), write statements.
# - Embedded code blocks #=lang={ code } are inlined as comments or foreign code if possible.
# - Blocks use [].
# - For 'vib t file.vib lang': Translate to lang, output to file_lang.ext, then TUI to ask compile to binary or vm.
#   - Compile: For binary, call system compiler (rustc, gcc, javac).
#   - For vm: Perhaps translate back or note; here, placeholder as vm is vib-specific.
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
  Write
  Import
  MemoryMode
  EmbeddedStart
  EmbeddedEnd
  Plus
  Minus
  Star
  Slash
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
    return {Token::EOF, nil} if @pos >= @chars.size

    ch = @chars[@pos]
    case ch
    when ':'
      @pos += 1
      if match_str("biblioteka:")
        {Token::Import, nil}
      else
        {Token::Colon, nil}
      end
    when '<'
      @pos += 1
      if match_str("zarzadznie pamiecia>")
        skip_whitespace
        mode = read_identifier
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
      {Token::Equal, nil}
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
      {Token::Minus, nil}
    when '*'
      @pos += 1
      {Token::Star, nil}
    when '/'
      @pos += 1
      {Token::Slash, nil}
    when '"'
      {Token::String, read_string}
    when .digit?
      read_number
    when .letter? || '_'
      ident = read_identifier
      case ident
      when "write"
        {Token::Write, nil}
      else
        {Token::Identifier, ident}
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

  private def match_str(s : String) : Bool
    start = @pos
    s.chars.each do |c|
      return false if current_char != c
      @pos += 1
    end
    true
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

class IdentifierNode < AstNode
  getter name : String

  def initialize(@name)
  end
end

class BlockNode < AstNode
  getter statements : Array(AstNode)

  def initialize(@statements)
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
      stmt = parse_statement
      statements << stmt
      advance
    end
    BlockNode.new(statements)
  end

  private def advance
    @current = @lexer.next_token
  end

  private def parse_statement : AstNode
    case @current[0]
    when Token::Import
      advance
      if @current[0] == Token::Identifier && @current[1]
        ImportNode.new(@current[1].not_nil!)
      else
        raise "Expected identifier after import"
      end
    when Token::MemoryMode
      MemoryModeNode.new(@current[1].not_nil!)
    when Token::Write
      advance
      expr = parse_expr
      WriteNode.new(expr)
    when Token::EmbeddedStart
      lang = @current[1].not_nil!
      advance
      if @current[0] != Token::LBrace
        raise "Expected { after #=lang="
      end
      advance
      start_pos = @lexer.@pos
      while @current[0] != Token::EmbeddedEnd
        advance
      end
      code = @lexer.@input[start_pos...@lexer.@pos - 1]
      EmbeddedNode.new(lang, code)
    when Token::LBracket
      advance
      block = [] of AstNode
      while @current[0] != Token::RBracket && @current[0] != Token::EOF
        block << parse_statement
      end
      if @current[0] == Token::RBracket
        advance
      end
      BlockNode.new(block)
    else
      parse_expr
    end
  end

  private def parse_expr : AstNode
    left = parse_primary
    while [Token::Plus, Token::Minus, Token::Star, Token::Slash].includes?(@current[0])
      op = case @current[0]
           when Token::Plus  then '+'
           when Token::Minus then '-'
           when Token::Star  then '*'
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
      IntegerNode.new(@current[1].not_nil!.to_i64)
    when Token::Float
      FloatNode.new(@current[1].not_nil!.to_f64)
    when Token::String
      StringNode.new(@current[1].not_nil!)
    when Token::Identifier
      IdentifierNode.new(@current[1].not_nil!)
    else
      raise "Unexpected token: #{@current[0]}"
    end
  end
end

# CodeGenerator base
abstract class CodeGenerator
  abstract def generate(node : AstNode) : String
end

# Rust Generator
class RustGenerator < CodeGenerator
  @memory_mode : String = "manual" # Default

  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when ImportNode
      "use #{node.lib};"
    when MemoryModeNode
      @memory_mode = node.mode
      "" # Handled elsewhere
    when WriteNode
      "println!(\"{}\", #{generate(node.expr)});"
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when IdentifierNode
      node.name
    when EmbeddedNode
      if node.lang == "rust"
        node.code
      else
        "// Embedded #{node.lang}: #{node.code}"
      end
    else
      ""
    end
  end
end

# C Generator
class CGenerator < CodeGenerator
  @memory_mode : String = "manual"

  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when ImportNode
      "#include <#{node.lib}>"
    when MemoryModeNode
      @memory_mode = node.mode
      "" 
    when WriteNode
      "printf(\"%d\\n\", #{generate(node.expr)});" # Assume int for simplicity
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when IdentifierNode
      node.name
    when EmbeddedNode
      if node.lang == "c"
        node.code
      else
        "/* Embedded #{node.lang}: #{node.code} */"
      end
    else
      ""
    end
  end
end

# Java Generator
class JavaGenerator < CodeGenerator
  @memory_mode : String = "automatic" # Java is GC

  def generate(node : AstNode) : String
    case node
    when BlockNode
      node.statements.map { |stmt| generate(stmt) }.join("\n")
    when ImportNode
      "import #{node.lib};"
    when MemoryModeNode
      @memory_mode = node.mode
      "" 
    when WriteNode
      "System.out.println(#{generate(node.expr)});"
    when BinaryOpNode
      "(#{generate(node.left)} #{node.op} #{generate(node.right)})"
    when IntegerNode
      node.value.to_s
    when FloatNode
      node.value.to_s
    when StringNode
      "\"#{node.value}\""
    when IdentifierNode
      node.name
    when EmbeddedNode
      if node.lang == "java"
        node.code
      else
        "// Embedded #{node.lang}: #{node.code}"
      end
    else
      ""
    end
  end
end

# TUI for compile choice
def tui_ask_compile : String
  puts "Do you want to compile to binary or vm? (binary/vm)"
  choice = gets.not_nil!.chomp
  choice
end

# Main
def main
  file = ""
  lang = ""
  project_mode = false

  OptionParser.parse do |parser|
    parser.banner = "Usage: translator [arguments]"
    parser.on("t", "Translate project or file") do
      # vib t or vib t file lang
      if ARGV.size == 0
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
    # vib t: generate build.hacker
    # Assume current dir is project, read cmd/main.vib
    input = File.read("cmd/main.vib") rescue raise "No main.vib"
    # Placeholder: generate config
    config = "Translate to Rust\n# More config"
    Dir.mkdir_p("build")
    File.write("build/build.hacker", config)
    puts "Generated build.hacker"
  else
    # vib t file.vib lang
    input = File.read(file)
    parser = Parser.new(input)
    ast = parser.parse

    generator = case lang
                when "rust" then RustGenerator.new
                when "c"    then CGenerator.new
                when "java" then JavaGenerator.new
                else raise "Unknown lang: #{lang}"
                end

    output_code = generator.generate(ast)
    out_file = "#{File.basename(file, ".vib")}.#{lang == "c" ? "c" : lang == "rust" ? "rs" : "java"}"
    File.write(out_file, output_code)
    puts "Translated to #{out_file}"

    # TUI
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

main if __FILE__ == PROGRAM_NAME

# Note: This is a demo. In full:
# - Wrap in main function for targets.
# - Handle memory mode: For manual in Rust use Box/unsafe, in Java ignore.
# - Better error handling.
# - Support more syntax (classes like JS).
# - For project mode, read bytes.yaml, generate full config in .hacker.
# - TUI more advanced, perhaps using a lib like termbox.
