BF_SRC := bench.b

include ../common/commands.mk
.DEFAULT_GOAL := build

executables := \
	target/bin_hs \
	target/bin_cpp_gcc \
	target/bin_cpp_clang \
	target/bin_d \
	target/bin_d_gdc \
	target/bin_d_ldc \
	bf.rs/target/release/bf \
	target/bin_swift \
	target/bin_nim_clang \
	target/bin_nim_gcc \
	target/bin_cr \
	target/bin_hs_marray \
	target/bin_ocaml \
	$(GO_TARGETS) \
	target/bin_go_gccgo \
	target/bin_vala_gcc \
	target/bin_vala_clang \
	target/bin_sml \
	target/bin_v_gcc \
	target/bin_v_clang \
	target/bin_c_gcc \
	target/bin_c_clang \
	fsharp/target/Release/net8.0/brainfuck \
	target/Release/net8.0/brainfuck \
	target/bin_zig \
	target/bin_idris2 \
	target/bin_idris2_array

artifacts := $(executables) \
	target/bf_scala.jar \
	target/bf_staged_scala.jar \
	target/bf2-kt.jar \
	target/bf.exe \
	target/bf.class

fast_runners := $(patsubst %,run[%], $(artifacts)) \
	run[bf.js] \
	run[pypy][bf3.py] \
	run[jit][bf_jit.lua] \
	run[bf.rkt] \
	run[bf-staged.rkt] \
	run[bf.ss] \
	run[truby-jvm][bf.rb] \
	run[truby-native][bf.rb] \
	run[bf.jl]

all_runners := $(fast_runners) \
	run[bf.ex] \
	run[bf3.py] \
	run[bf.pl] \
	run[bf.rb] \
	run[jit][bf.rb] \
	run[jruby][bf.rb] \
	run[bf.lua] \
	run[bf.tcl] \
	run[bf_oo.tcl] \
	run[bf.php]

# Build

.PHONY: build
build: $(artifacts)

target/bin_cr: bf.cr | target
	$(CRYSTAL_BUILD)

target/bin_cpp_clang: bf.cpp | target libnotify
	$(CLANG_CPP_BUILD)

target/bin_cpp_gcc: bf.cpp | target libnotify
	$(GCC_CPP_BUILD)

bf-rs-toml := bf.rs/Cargo.toml
.PHONY: bf.rs/target/release/bf
bf.rs/target/release/bf: $(bf-rs-toml)
	$(CARGO_BUILD)

target/bin_swift: bf.swift | target
	$(SWIFTC_BUILD)

target/bf_scala.jar: bf.scala | target
	$(SCALAC_BUILD)

target/bf_staged_scala.jar: bf-staged.scala | target
	$(SCALAC_BUILD)

target/bf.exe: bf.cs | target
	$(MCS_BUILD)

.PHONY: target/Release/net8.0/brainfuck
target/Release/net8.0/brainfuck: brainfuck.csproj | target
	$(DOTNET_BUILD)

.PHONY: fsharp/target/Release/net8.0/brainfuck
fsharp/target/Release/net8.0/brainfuck: fsharp/brainfuck.fsproj | target
	$(DOTNET_BUILD)

target/bf2-kt.jar: bf2.kt | target
	$(KOTLINC_BUILD)

target/bin_go_gccgo: go/bf $(GO_SOURCE) | gofmt
	$(GCC_GO_BUILD)

target/bin_d: bf.d | $(dfmt)
	$(DMD_BUILD)

target/bin_d_gdc: bf.d | $(dfmt)
	$(GDC_BUILD)

target/bin_d_ldc: bf.d | $(dfmt)
	$(LDC2_BUILD)

target/bin_nim_clang: bf.nim | target
	$(NIM_CLANG_BUILD)

target/bin_nim_gcc: bf.nim | target
	$(NIM_GCC_BUILD)

target/bin_hs: bf.hs | $(hlint)
	$(GHC_BUILD)

target/bin_hs_marray: bf-marray.hs | $(hlint)
	$(GHC_BUILD)

target/bin_ocaml: bf.ml | target
	$(OCAML_BUILD)

target/bin_vala_gcc: bf.vala | target
	$(VALAC_GCC_BUILD)

target/bin_vala_clang: bf.vala | target
	$(VALAC_CLANG_BUILD)

target/bin_sml: bf.sml | target
	$(MLTON_BUILD)

target/bin_v_gcc: bf.v | $(v_fmt)
	$(V_GCC_BUILD)

target/bin_v_clang: bf.v | $(v_fmt)
	$(V_CLANG_BUILD)

target/bin_c_gcc: bf.c | target libnotify
	$(GCC_BUILD)

target/bin_c_clang: bf.c | target libnotify
	$(CLANG_BUILD)

target/bf.class: bf.java | target
	$(JAVAC_BUILD)

target/bin_zig: bf.zig | $(zig_fmt)
	$(ZIG_BUILD)

target/bin_idris2: idris2-brainfuck | target
	$(IDRIS_BUILD)

target/bin_idris2_array: idris2-brainfuck-array | target
	$(IDRIS_BUILD)

# Run

.PHONY: run
run: $(all_runners)

.PHONY: run2
run2: BF_SRC := mandel.b > /dev/null
run2: $(fast_runners)

## Common recipe for all runners
.PHONY: $(all_runners)
$(all_runners)::
	$(ECHO_RUN)

## Runners
executable_runners := $(patsubst %,run[%], $(executables))
$(executable_runners):: run[%] : %
	$(EXECUTABLE_RUN) $(BF_SRC)

run[target/bf_scala.jar]:: run[%]: %
	$(SCALA_RUN) BrainFuck $(BF_SRC)

run[target/bf_staged_scala.jar]:: run[%]: %
	$(SCALA_RUN) BrainFuckStaged $(BF_SRC)

run[target/bf.exe]:: run[%]: %
	$(MONO_RUN) $(BF_SRC)

run[target/bf2-kt.jar]:: run[%]: %
	$(JAVA_JAR_RUN) $(BF_SRC)

run[target/bf.class]:: run[%]: %
	$(JAVA_CLASS_RUN) $(BF_SRC)

run[bf.ex]:: run[%]: %
	$(ELIXIR_RUN) $(BF_SRC)

run[bf.js]:: run[%]: %
	$(NODE_RUN) $(BF_SRC)

run[pypy][bf3.py]:: run[pypy][%]: % | $(py_fmt)
	$(PYPY3_RUN) $(BF_SRC)

run[bf3.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN) $(BF_SRC)

run[bf.pl]:: run[%]: %
	$(PERL_RUN) $(BF_SRC)

run[bf.rb]:: run[%]: % | $(rubocop)
	$(RUBY_RUN) $(BF_SRC)

run[jit][bf.rb]:: run[jit][%]: % | $(rubocop)
	$(RUBY_JIT_RUN) $(BF_SRC)

run[truby-jvm][bf.rb]:: run[truby-jvm][%]: % | $(rubocop)
	$(TRUBY_JVM_RUN) $(BF_SRC)

run[truby-native][bf.rb]:: run[truby-native][%]: % | $(rubocop)
	$(TRUBY_NATIVE_RUN) $(BF_SRC)

run[jruby][bf.rb]:: run[jruby][%]: % | $(rubocop)
	$(JRUBY_RUN) $(BF_SRC)

run[bf.lua]:: run[%]: % | libnotify
	$(LUA_RUN) $(BF_SRC)

run[jit][bf_jit.lua]:: run[jit][%]: %
	$(LUA_JIT_RUN) $(BF_SRC)

run[bf.tcl]:: run[%]: %
	$(TCLSH_RUN) $(BF_SRC)

run[bf_oo.tcl]:: run[%]: %
	$(TCLSH_RUN) $(BF_SRC)

run[bf.rkt]:: run[%]: %
	$(RACKET_RUN) $(BF_SRC)

run[bf-staged.rkt]:: run[%]: %
	$(RACKET_RUN) $(BF_SRC)

run[bf.ss]:: run[%]: % | libnotify
	$(SCHEME_RUN) $(BF_SRC)

run[bf.jl]:: run[%]: % | $(julia_fmt)
	$(JULIA_RUN) $(BF_SRC)

run[bf.php]:: run[%]: %
	$(PHP_RUN) $(BF_SRC)	

run[bin_idris2]:: run[%]: %
	$(IDRIS_RUN) $(BF_SRC)

run[bin_idris2_array]:: run[%]: %
	$(IDRIS_RUN) $(BF_SRC)

# Utilities

.PHONY: clean
clean: clean-go
	$(DOTNET_CLEAN) fsharp/brainfuck.fsproj
	-rm -rf target
	-rm -rf zig-cache
