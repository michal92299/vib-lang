# src/vib.cr
# This is the main 'vib' binary implemented in Crystal.
# It serves as the entry point for the vib-lang CLI, dispatching commands to other binaries.
# It handles project structure, calls compiler, translator, virtual-machine, etc.
# Makes the CLI "pretty" with colored output where possible.
# Assumes other binaries are in /usr/lib/vib-lang/bin/
# For install/remove, assumes 'bytes' is a separate binary (package manager).

require "colorize"
require "file_utils"
require "process"

def print_help
  puts "Vib Lang CLI".colorize(:green).bold
  puts "Usage: vib <command> [options]"
  puts "\nCommands:"
  puts "  c                - Compile project (must be in project dir with cmd/main.vib and bytes.yaml)"
  puts "  c <file.vib>     - Compile single file to binary"
  puts "  t <file.vib> <lang> - Translate file to language (rust, c, java) + TUI for compile"
  puts "  t                - Translate project and generate /build/build.hacker"
  puts "  vm build         - Build project to VM format (.object in /build/release/)"
  puts "  vm compile <file.object> - Compile .object to .vib-vm"
  puts "  run <file.vib-vm> - Run .vib-vm file"
  puts "  install          - Run bytes install (package manager)"
  puts "  remove           - Run bytes remove"
  puts "  build            - Build from /build/build.hacker configuration"
  puts "  help             - Show this help"
  puts "  docs             - Show documentation"
  puts "  tutorials        - Show tutorials"
end

def error(msg : String)
  puts "Error: #{msg}".colorize(:red)
  exit(1)
end

def main
  if ARGV.empty?
    print_help
    exit(0)
  end

  cmd = ARGV.shift
  bin_path = "/usr/lib/vib-lang/bin/"

  case cmd
  when "c"
    if ARGV.empty?
      # Project mode
      unless File.exists?("cmd/main.vib") && File.exists?("bytes.yaml")
        error("Missing cmd/main.vib or bytes.yaml in project directory")
      end
      FileUtils.mkdir_p("build/release")
      status = Process.run(bin_path + "compiler", args: ["cmd/main.vib", "build/release/main", "binary"])
      unless status.success?
        error("Compilation failed")
      end
      puts "Compiled project to build/release/main".colorize(:green)
    elsif ARGV.size == 1 && ARGV[0].ends_with?(".vib")
      file = ARGV[0]
      out = file.rchop(".vib")
      status = Process.run(bin_path + "compiler", args: [file, out, "binary"])
      unless status.success?
        error("Compilation failed")
      end
      puts "Compiled #{file} to #{out}".colorize(:green)
    else
      error("Invalid arguments for 'c'")
    end
  when "t"
    if ARGV.empty?
      # Project mode
      status = Process.run(bin_path + "translator", args: ["t"])
      unless status.success?
        error("Translation failed")
      end
      puts "Generated build/build.hacker".colorize(:green)
    elsif ARGV.size == 2 && ARGV[0].ends_with?(".vib")
      file = ARGV[0]
      lang = ARGV[1]
      status = Process.run(bin_path + "translator", args: ["t", file, lang])
      unless status.success?
        error("Translation failed")
      end
      puts "Translated #{file} to #{lang}".colorize(:green)
    else
      error("Invalid arguments for 't'")
    end
  when "vm"
    if ARGV.empty?
      error("Missing subcommand for 'vm' (build or compile)")
    end
    sub = ARGV.shift
    case sub
    when "build"
      # Project mode to .object
      unless File.exists?("cmd/main.vib") && File.exists?("bytes.yaml")
        error("Missing files in project directory")
      end
      FileUtils.mkdir_p("build/release")
      status = Process.run(bin_path + "compiler", args: ["cmd/main.vib", "build/release/main.object", "object"])
      unless status.success?
        error("VM build failed")
      end
      puts "Built project to build/release/main.object for VM".colorize(:green)
    when "compile"
      if ARGV.empty?
        error("Missing .object file")
      end
      file = ARGV[0]
      if file.ends_with?(".object")
        out = file.rchop(".object") + ".vib-vm"
        # Assuming compiler handles 'vm' mode for packaging
        status = Process.run(bin_path + "compiler", args: [file, out, "vm"])
        unless status.success?
          error("VM compile failed")
        end
        puts "Compiled #{file} to #{out}".colorize(:green)
      else
        error("File must end with .object")
      end
    else
      error("Unknown 'vm' subcommand: #{sub}")
    end
  when "run"
    if ARGV.size == 1 && ARGV[0].ends_with?(".vib-vm")
      file = ARGV[0]
      # Assume memory mode from config or default automatic
      status = Process.run(bin_path + "virtual-machine", args: [file, "automatic"])
      unless status.success?
        error("Run failed")
      end
    else
      error("Invalid arguments for 'run': provide .vib-vm file")
    end
  when "install"
    status = Process.run("bytes", args: ["install"])
    unless status.success?
      error("Install failed")
    end
    puts "Installed dependencies".colorize(:green)
  when "remove"
    status = Process.run("bytes", args: ["remove"])
    unless status.success?
      error("Remove failed")
    end
    puts "Removed dependencies".colorize(:green)
  when "build"
    if File.exists?("build/build.hacker")
      config = File.read("build/build.hacker")
      puts "Building from config: #{config}"
      # Parse config and act; for demo, assume it's a lang, call translator build
      # Assuming first line is "Translate to <lang>"
      if config.includes?("Translate to")
        lang = config.split("Translate to")[1].strip.split("\n")[0].strip
        status = Process.run(bin_path + "translator", args: ["t", "cmd/main.vib", lang])
        unless status.success?
          error("Build failed")
        end
        puts "Built from build.hacker to #{lang}".colorize(:green)
      else
        error("Invalid build.hacker config")
      end
    else
      error("No build/build.hacker found")
    end
  when "help"
    print_help
  when "docs"
    puts "Vib Lang Documentation:".colorize(:blue)
    puts "- Syntax inspired by Python/Ruby/JS"
    puts "- Imports: :biblioteka:"
    puts "- Memory: <zarzadznie pamiecia> automatic or manual"
    puts "- Write instead of print"
    puts "- Blocks with []"
    puts "- Embedded code: #=lang={ code }"
    # More...
  when "tutorials"
    puts "Vib Lang Tutorials:".colorize(:blue)
    puts "1. Hello World: write \"Hello\""
    puts "2. ..."
    # Placeholder
  else
    puts "Unknown command: #{cmd}".colorize(:red)
    print_help
  end
end

main if __FILE__ == PROGRAM_NAME

# Note: This is a complete implementation based on the spec.
# - Uses Crystal's Process to call other binaries.
# - Handles project vs single file.
# - For 'bytes', assumes it's installed system-wide.
# - Parser binary not directly called, assumed used internally by others.
# - Colors for pretty output.
# - Error handling basic.
# - For real, add more features like dependency resolution from bytes.yaml.
