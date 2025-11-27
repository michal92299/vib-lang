# src/vib.cr
# This is the main 'vib' binary implemented in Crystal.
# It serves as the entry point for the vib-lang CLI, dispatching commands to other binaries.
# It handles project structure, calls compiler, translator, virtual-machine, etc.
# Makes the CLI "pretty" with colored output where possible.
# Assumes other binaries are in /usr/lib/vib-lang/bin/
# For install/remove, assumes 'bytes' is a separate binary (package manager).
# Expanded features:
# - Better error handling with detailed messages.
# - Support for more commands: init (create project), test (run tests if any), clean (remove build).
# - Verbose mode (-v) for more output.
# - Configurable bin_path via env var.
# - Handle more file types, check dependencies from bytes.yaml.
# - Pretty print help with categories.
# - Version command.

require "colorize"
require "file_utils"
require "process"
require "yaml"
require "option_parser"

VERSION = "0.1.0"

def print_help
  puts "Vib Lang CLI - Version #{VERSION}".colorize(:green).bold
  puts "Usage: vib <command> [options]"
  puts "\nCore Commands:"
  puts "  init                 - Initialize a new project (creates cmd/main.vib, bytes.yaml)"
  puts "  c                    - Compile project to binary"
  puts "  c <file.vib>         - Compile single file to binary"
  puts "  t                    - Translate project and generate build/build.hacker"
  puts "  t <file.vib> <lang>  - Translate file to language (rust, c, java) + TUI for compile"
  puts "  vm build             - Build project to VM format (.object)"
  puts "  vm compile <file.object> - Compile .object to .vib-vm"
  puts "  run <file.vib-vm>    - Run .vib-vm file"
  puts "\nPackage Management:"
  puts "  install              - Run bytes install"
  puts "  remove               - Run bytes remove"
  puts "\nBuild & Clean:"
  puts "  build                - Build from build/build.hacker configuration"
  puts "  clean                - Clean build directory"
  puts "\nHelp & Info:"
  puts "  help                 - Show this help"
  puts "  version              - Show version"
  puts "  docs                 - Show documentation"
  puts "  tutorials            - Show tutorials"
  puts "\nOptions:"
  puts "  -v, --verbose        - Enable verbose output"
end

def error(msg : String)
  puts "Error: #{msg}".colorize(:red)
  exit(1)
end

def info(msg : String, verbose : Bool)
  if verbose
    puts "Info: #{msg}".colorize(:yellow)
  end
end

def check_project_files : Bool
  File.exists?("cmd/main.vib") && File.exists?("bytes.yaml")
end

def load_bytes_yaml : YAML::Any?
  if File.exists?("bytes.yaml")
    YAML.parse(File.read("bytes.yaml"))
  else
    nil
  end
end

def main
  verbose = false
  bin_path = ENV["VIB_BIN_PATH"]? || "/usr/lib/vib-lang/bin/"
  OptionParser.parse do |parser|
    parser.on("-v", "--verbose", "Verbose output") { verbose = true }
    parser.on("help", "Show help") { print_help; exit(0) }
    parser.on("version", "Show version") { puts VERSION; exit(0) }
    parser.unknown_args do |args|
      if args.empty?
        print_help
        exit(0)
      end
      cmd = args.shift
      case cmd
      when "init"
        if check_project_files
          error("Project already initialized")
        end
        Dir.mkdir_p("cmd")
        File.write("cmd/main.vib", "<automatic>\n\n[ write \"Hello, Vib!\" ]")
        File.write("bytes.yaml", "dependencies: []\n")
        puts "Initialized new Vib project".colorize(:green)
      when "c"
        if args.empty?
          # Project mode
          unless check_project_files
            error("Missing cmd/main.vib or bytes.yaml in project directory")
          end
          info("Checking dependencies...", verbose)
          if let yaml = load_bytes_yaml
            deps = yaml["dependencies"].as_a? || [] of YAML::Any
            if !deps.empty?
              puts "Dependencies found: #{deps.size}".colorize(:blue)
              # Assume bytes install if needed
              Process.run("bytes", args: ["install"])
            end
          end
          FileUtils.mkdir_p("build/release")
          status = Process.run("#{bin_path}compiler", args: ["cmd/main.vib", "build/release/main", "binary"])
          unless status.success?
            error("Compilation failed: #{status.exit_status}")
          end
          puts "Compiled project to build/release/main".colorize(:green)
        elsif args.size == 1 && args[0].ends_with?(".vib")
          file = args[0]
          out = file.rchop(".vib")
          status = Process.run("#{bin_path}compiler", args: [file, out, "binary"])
          unless status.success?
            error("Compilation failed: #{status.exit_status}")
          end
          puts "Compiled #{file} to #{out}".colorize(:green)
        else
          error("Invalid arguments for 'c'")
        end
      when "t"
        if args.empty?
          # Project mode
          unless check_project_files
            error("Not a project directory")
          end
          status = Process.run("#{bin_path}translator", args: ["t"])
          unless status.success?
            error("Translation failed: #{status.exit_status}")
          end
          puts "Generated build/build.hacker".colorize(:green)
        elsif args.size == 2 && args[0].ends_with?(".vib")
          file = args[0]
          lang = args[1]
          status = Process.run("#{bin_path}translator", args: ["t", file, lang])
          unless status.success?
            error("Translation failed: #{status.exit_status}")
          end
          puts "Translated #{file} to #{lang}".colorize(:green)
        else
          error("Invalid arguments for 't'")
        end
      when "vm"
        if args.empty?
          error("Missing subcommand for 'vm' (build or compile)")
        end
        sub = args.shift
        case sub
        when "build"
          unless check_project_files
            error("Missing files in project directory")
          end
          FileUtils.mkdir_p("build/release")
          status = Process.run("#{bin_path}compiler", args: ["cmd/main.vib", "build/release/main.object", "object"])
          unless status.success?
            error("VM build failed: #{status.exit_status}")
          end
          puts "Built project to build/release/main.object for VM".colorize(:green)
        when "compile"
          if args.empty?
            error("Missing .object file")
          end
          file = args[0]
          if file.ends_with?(".object")
            out = file.rchop(".object") + ".vib-vm"
            status = Process.run("#{bin_path}compiler", args: [file, out, "vm"])
            unless status.success?
              error("VM compile failed: #{status.exit_status}")
            end
            puts "Compiled #{file} to #{out}".colorize(:green)
          else
            error("File must end with .object")
          end
        else
          error("Unknown 'vm' subcommand: #{sub}")
        end
      when "run"
        if args.size == 1 && args[0].ends_with?(".vib-vm")
          file = args[0]
          memory_mode = "automatic" # Default or from config
          if let yaml = load_bytes_yaml
            if yaml["memory"]?
              memory_mode = yaml["memory"].as_s
            end
          end
          info("Running with memory mode: #{memory_mode}", verbose)
          status = Process.run("#{bin_path}virtual-machine", args: [file, memory_mode])
          unless status.success?
            error("Run failed: #{status.exit_status}")
          end
        else
          error("Invalid arguments for 'run': provide .vib-vm file")
        end
      when "install"
        status = Process.run("bytes", args: ["install"])
        unless status.success?
          error("Install failed: #{status.exit_status}")
        end
        puts "Installed dependencies".colorize(:green)
      when "remove"
        status = Process.run("bytes", args: ["remove"])
        unless status.success?
          error("Remove failed: #{status.exit_status}")
        end
        puts "Removed dependencies".colorize(:green)
      when "build"
        if File.exists?("build/build.hacker")
          config = File.read("build/build.hacker")
          info("Building from config: #{config}", verbose)
          # Parse simple config
          lines = config.lines
          lines.each do |line|
            if line.starts_with?("Translate to")
              lang = line.split("Translate to")[1].strip
              status = Process.run("#{bin_path}translator", args: ["t", "cmd/main.vib", lang])
              unless status.success?
                error("Build failed: #{status.exit_status}")
              end
              puts "Built to #{lang}".colorize(:green)
            end
            # Add more config parsers
          end
        else
          error("No build/build.hacker found")
        end
      when "clean"
        if Dir.exists?("build")
          FileUtils.rm_rf("build")
          puts "Cleaned build directory".colorize(:green)
        else
          puts "No build directory to clean".colorize(:yellow)
        end
      when "docs"
        puts "Vib Lang Documentation:".colorize(:blue)
        puts "- Syntax inspired by Python/Ruby/JS"
        puts "- Imports: :std:"
        puts "- Memory: <automatic> or <manual>"
        puts "- Write instead of print"
        puts "- Blocks with []"
        puts "- Embedded code: #=lang={ code }"
        puts "- Variables: let x = 1;"
        puts "- Functions: fn add(a, b) [ return a + b; ]"
        puts "- Classes: class Point [ fn init(x, y) [ ... ] ]"
        puts "- If: [cond] [true] [false]"
        puts "- While: [cond] [body]"
        # More detailed docs...
      when "tutorials"
        puts "Vib Lang Tutorials:".colorize(:blue)
        puts "1. Hello World:"
        puts "   <automatic>"
        puts "   [ write \"Hello\" ]"
        puts "2. Function:"
        puts "   fn greet(name) [ write \"Hello, \" + name; ]"
        puts "   greet(\"Vib\");"
        # More tutorials...
      else
        puts "Unknown command: #{cmd}".colorize(:red)
        print_help
      end
    end
  end
end

main if __FILE__ == PROGRAM_NAME

# Note: Expanded with init, clean, version, verbose, env config, dependency check.
# Assumes bytes.yaml has 'dependencies' array and optional 'memory'.
# For real, parse bytes.yaml fully for deps resolution before compile.
# Add more subcommands like 'test' if test framework exists.
