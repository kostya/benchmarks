#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'json'

BUILD_DIR = 'build'

def dotnet_base_path
  info = `dotnet --info`
  info.match(/Base Path:\s+(.*)\s*/)[1]
end

def cat(filename, content = nil)
  name = File.join(BUILD_DIR, filename)
  unless content.nil?
    File.open(name, 'w') do |f|
      f.puts content
    end
  end
  name
end

LANGS = {
  'Rust' => -> { `rustc --version`.split[1] },
  'Vala' => -> { `vala --version`.split[1] },
  'D/ldc2' => lambda do
    line = `ldc2 -v -X -Xf=#{cat('ldc.json')} -Xi=compilerInfo`.split("\n")[1]
    line.match(/version\s+(.*)\s+\(/)[1]
  end,
  'Swift' => -> { `swift --version`.split("\n")[0].match(/\((.*)\)/)[1] },
  'MLton' => -> { `mlton`.split[1] },
  'F#/.NET Core' => lambda do
    fsharpc = File.join(dotnet_base_path, 'FSharp', 'fsc.exe')
    `dotnet #{fsharpc} --help | sed -n 1p`.match(/version\s(.*)/)[1]
  end,
  'C/gcc' => -> { `gcc -dumpfullversion` },
  'Go/gccgo' => -> { `gccgo -dumpfullversion` },
  'D/gdc' => -> { `gdc -dumpfullversion` },
  'Nim' => lambda do
    `nim c --verbosity:0 --hint[Processing]:off \
           -r #{cat('nim.nim', 'echo NimVersion')}`
  end,
  'Crystal' => -> { `crystal eval "puts Crystal::VERSION"` },
  'Go' => lambda do
    prog = <<~GO
      package main
      import (
        "fmt"
        "runtime"
      )
      func main() {
        fmt.Printf(runtime.Version())
      }
    GO
    `go run #{cat('go.go', prog)}`
  end,
  'D/dmd' => lambda do
    json = `dmd -X -Xf=#{cat('dmd.json')} -Xi=compilerInfo \
      && cat #{cat('dmd.json')}`
    JSON.parse(json)['compilerInfo']['version']
  end,
  'C/clang' => lambda do
    prog = <<~CLANG
      #include <stdio.h>
      int main(void) {
        printf(__clang_version__);
        return 0;
      }
    CLANG
    `clang -o #{cat('clang')} #{cat('clang.c', prog)} && ./#{cat('clang')}`
  end,
  'Scala' => -> { `scala -e "print(util.Properties.versionNumberString)"` },
  'Node.js' => -> { `node -e "console.log(process.version)"` },
  'Python' => lambda do
    `python3 -c "import platform;print(platform.python_version())"`
  end,
  'Python/pypy' => lambda do
    prog = <<~PYPY
      import platform, sys
      pypy = "%d.%d.%d-%s%d" % sys.pypy_version_info
      print("%s for Python %s" % (pypy, platform.python_version()))
    PYPY
    `pypy3 #{cat('pypy.py', prog)}`
  end,
  'Ruby' => -> { `ruby -e 'puts "#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}"'` },
  'Ruby/jruby' => -> { `jruby -e 'puts JRUBY_VERSION'` },
  'Ruby/truffleruby' => -> { `truffleruby -e 'puts RUBY_ENGINE_VERSION'` },
  'Java' => lambda do
    prog = <<~JAVA
      class Test {
        public static void main(String[] argv) {
          System.out.print(System.getProperty("java.version"));
        }
      }
    JAVA
    `java #{cat('java.java', prog)}`
  end,
  'Julia' => -> { `julia -E 'VERSION'` },
  'C#/Mono' => -> { `mono --version=number` },
  '.NET Core' => -> { `dotnet --version` },
  'C#/.NET Core' => lambda do
    csc = File.join(dotnet_base_path, 'Roslyn', 'bincore', 'csc.dll')
    `dotnet #{csc} -version`
  end,
  'Perl' => -> { `perl -e 'print $^V;'` },
  'Haskell' => -> { `ghc --numeric-version` },
  'Tcl' => -> { `echo 'puts "$tcl_version"' | tclsh` },
  # TODO: remove JAVA_OPTS as soon as new Kotlin is released
  # (see https://youtrack.jetbrains.com/issue/KT-43704)
  'Kotlin' => lambda do
    `JAVA_OPTS="--illegal-access=permit" kotlin -e KotlinVersion.CURRENT`
  end,
  'PHP' => -> { `php -r "echo phpversion();"` },
  'Elixir' => -> { `elixir -e "IO.puts System.version"` },
  'Lua' => -> { `lua -e "print(_VERSION)"` },
  'Lua/luajit' => -> { `luajit -e "print(jit.version)"` },
  'OCaml' => -> { `ocaml -vnum` },
  'Racket' => -> { `racket -e "(version)"` },
  'Chez Scheme' => -> { `scheme --version 2>&1` },
  'V' => -> { `v version`.split[1] },
  'Clojure' => -> { `clojure -M -e '(clojure-version)'` }
}.freeze

def pad(num, str, padstr)
  str.strip.ljust(num, padstr)
end

def lpad(str, padstr = ' ')
  pad(16, str, padstr)
end

def rpad(str, padstr = ' ')
  pad(31, str, padstr)
end

def versions
  table = [
    "| #{lpad('Language')} | #{rpad('Version')} |",
    "| #{lpad('-', '-')} | #{rpad('-', '-')} |"
  ]
  LANGS.sort.each do |name, version_lambda|
    warn "Fetching #{name} version..."
    version = version_lambda.call
    table << "| #{lpad(name)} | #{rpad(version)} |"
  end

  table.join("\n")
end

FileUtils.mkdir_p BUILD_DIR
puts versions
warn "\n"
FileUtils.rm_r BUILD_DIR
