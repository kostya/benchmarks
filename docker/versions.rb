#!/usr/bin/env ruby

require 'fileutils'
require 'json'

BUILD_DIR = "build"

def dotnet_base_path
  info = `dotnet --info`
  info.match(/Base Path:\s+(.*)\s*/)[1]
end

def cat(filename, content = nil)
  name = File.join(BUILD_DIR, filename)
  unless content.nil?
    open(name, 'w') do |f|
      f.puts content
    end
  end
  name
end

def versions
  FileUtils.mkdir_p BUILD_DIR

  pad = ->(n, str, padstr) { str.strip.ljust(n, padstr) }
  lpad = ->(str, padstr = " ") { pad.(12, str, padstr) }
  rpad = ->(str, padstr = " ") { pad.(31, str, padstr) }
  table = [
    "| #{lpad.('Language')} | #{rpad.('Version')} |",
    "| #{  lpad.('-', '-')} | #{rpad.('-', '-')} |"
  ]
  langs = {
    "Rust" => -> { `rustc --version`.split()[1] },
    "LDC" => -> {
      line = `ldc2 -v -X -Xf=#{cat('ldc.json')} -Xi=compilerInfo`.split("\n")[1]
      line.match(/version\s+(.*)\s+\(/)[1]
    },
    "Swift" => -> { `swift --version`.split("\n")[0].match(/\((.*)\)/)[1] },
    "MLton" => -> { `mlton`.split()[1] },
    "F# .NET Core" => -> {
      fsharpc = File.join(dotnet_base_path, "FSharp", "fsc.exe")
      `dotnet #{fsharpc} --help | sed -n 1p`.match(/version\s(.*)/)[1]
    },

    "GCC" => -> { `gcc -dumpfullversion` },
    "GCC Go" => -> { `gccgo -dumpfullversion` },
    "GDC" => -> { `gdc -dumpfullversion` },
    "Nim" => -> { `nim c --verbosity:0 --hint[Processing]:off -r #{cat('nim.nim', 'echo NimVersion')}` },
    "Crystal" => -> { `crystal eval "puts Crystal::VERSION"` },
    "Go" => -> {
      prog = <<-END
package main
import (
  "fmt"
  "runtime"
)
func main() {
  fmt.Printf(runtime.Version())
}
END
      `go run #{cat('go.go', prog)}`
    },
    "DMD" => -> {
      json = `dmd -X -Xf=#{cat('dmd.json')} -Xi=compilerInfo && cat #{cat('dmd.json')}`
      JSON.parse(json)["compilerInfo"]["version"]
    },
    "Clang" => -> {
      prog = <<-END
#include <stdio.h>
int main(void) {
  printf(__clang_version__);
  return 0;
}
END
      `clang -o #{cat('clang')} #{cat('clang.c', prog)} && ./#{cat('clang')}`
    },
    "Scala" => -> { `scala -e "print(util.Properties.versionNumberString)"` },
    "Node.js" => -> { `node -e "console.log(process.version)"` },
    "Python" => -> { `python3 -c "import platform;print(platform.python_version())"` },
    "PyPy" => -> {
      prog = <<-END
import platform, sys
pypy = "%d.%d.%d-%s%d" % sys.pypy_version_info
print("%s for Python %s" % (pypy, platform.python_version()))
END
      `pypy3 #{cat('pypy.py', prog)}`
    },
    "Ruby" => -> { `ruby -e 'puts "#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}"'` },
    "JRuby" => -> { `jruby -e 'puts JRUBY_VERSION'` },
    "TruffleRuby" => -> { `truffleruby -e 'puts RUBY_ENGINE_VERSION'` },
    "Java" => -> {
      prog = <<-END
class Test {
  public static void main(String[] argv) {
    System.out.print(System.getProperty("java.version"));
  }
}
END
      `java #{cat('java.java', prog)}`
    },
    "Julia" => -> { `julia -E 'VERSION'` },
    "C# Mono" => -> { `mono --version=number` },
    ".NET Core" => -> { `dotnet --version` },
    "C# .NET Core" => -> {
      csc = File.join(dotnet_base_path, "Roslyn", "bincore", "csc.dll")
      `dotnet #{csc} -version`
    },
    "Perl" => -> { `perl -e 'print $^V;'` },
    "Haskell" => -> { `ghc --numeric-version` },
    "Tcl" => -> { `echo 'puts "$tcl_version"' | tclsh` },
    "Kotlin" => -> {
      prog = <<-END
fun main(args: Array<String>){
  println(KotlinVersion.CURRENT)
}
END
      `kotlinc #{cat('kt.kt', prog)} -include-runtime -d #{cat('kt.jar')} && java -jar #{cat('kt.jar')}` },
    "PHP" => -> { `php -r "echo phpversion();"` },
    "Elixir" => -> { `elixir -e "IO.puts System.version"` },
    "Lua" => -> { `lua -e "print(_VERSION)"` },
    "LuaJIT" => -> { `luajit -e "print(jit.version)"` },
    "OCaml" => -> { `ocaml -vnum` },
    "Racket" => -> { `racket -e "(version)"` },
    "Chez Scheme" => -> { `scheme --version 2>&1` },
    "V" => -> {
      prog = <<-END
import compiler
fn main() {
  print('${compiler.Version} ${compiler.vhash()}')
}
END
      `v run #{cat('v.v', prog)}`
    },
    "Clojure" => -> { `clojure -e '(clojure-version)'` },
  }
  langs.sort.each do | name, version_lambda |
    STDERR.puts "Fetching #{name} version..."
    version = version_lambda.call
    table << "| #{lpad.(name)} | #{rpad.(version)} |"
  end

  FileUtils.rm_r BUILD_DIR
  STDERR.puts "\n"
  table.join("\n")
end

puts versions
