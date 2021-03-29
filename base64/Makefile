include ../common/commands.mk
.DEFAULT_GOAL := build

executables := \
	target/base64_cr \
	target/base64_go \
	target/base64_go_gccgo \
	target/base64_cpp_crypto \
	target/base64_c_nolib \
	target/base64_d \
	target/base64_d_gdc \
	target/base64_d_ldc \
	target/base64_nim_gcc \
	target/base64_nim_clang \
	base64.rs/target/release/base64 \
	target/base64_c_ak \
	target/base64_v_gcc \
	target/base64_v_clang \
	target/base64_vala_gcc \
	target/base64_vala_clang \
	target/Release/net5.0/base64 \
	base64.fsharp/target/Release/net5.0/base64

artifacts := $(executables) \
	target/base64_scala.jar \
	target/Base64Java.class \
	target/Test-kt.jar \
	target/test.exe

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[test.pl] \
	run[test.jl] \
	run[test.js] \
	run[pypy][test.py] \
	run[test.py] \
	run[test.rb] \
	run[jit][test.rb] \
	run[truby-jvm][test.rb] \
	run[truby-native][test.rb] \
	run[jruby][test.rb] \
	run[test-xs.pl] \
	run[test.tcl] \
	run[test.php]

# Build

.PHONY: build
build: $(artifacts)

target/base64_cr: test.cr | target
	$(CRYSTAL_BUILD)

target/base64_go: test.go | $(gofmt)
	$(GO_BUILD)

target/base64_go_gccgo: test.go | $(gofmt)
	$(GCC_GO_BUILD)

target/base64_cpp_crypto: test-crypto.cpp | target libnotify
	$(GCC_CPP_BUILD) -lcrypto

target/base64_c_nolib: test-nolib.c | target libnotify
	$(GCC_BUILD)

target/base64_scala.jar: test.scala | target
	$(SCALAC_BUILD)

target/Base64Java.class: Base64Java.java | target
	$(JAVAC_BUILD)

target/Test-kt.jar: Test.kt | target
	$(KOTLINC_BUILD)

target/base64_d: test.d | $(dfmt)
	$(DMD_BUILD)

target/base64_d_gdc: test.d | $(dfmt)
	$(GDC_BUILD)

target/base64_d_ldc: test.d | $(dfmt)
	$(LDC2_BUILD)

target/base64_nim_gcc: test.nim | target
	$(NIM_GCC_BUILD)

target/base64_nim_clang: test.nim | target
	$(NIM_CLANG_BUILD)

aklomp-base64-dir := target/aklomp-base64
$(aklomp-base64-dir): | target
	$(GIT_CLONE) "https://github.com/aklomp/base64.git" $@

aklomp-base64-lib := lib/libbase64.o
$(aklomp-base64-dir)/$(aklomp-base64-lib): $(aklomp-base64-dir)
	AVX2_CFLAGS=-mavx2 SSSE3_CFLAGS=-mssse3 SSE41_CFLAGS=-msse4.1 \
	SSE42_CFLAGS=-msse4.2 AVX_CFLAGS=-mavx \
	$(MAKE) -C $< $(aklomp-base64-lib)

target/base64_c_ak: $(aklomp-base64-dir)/$(aklomp-base64-lib) test-aklomp.c | target libnotify
	$(GCC_BUILD) -I$(aklomp-base64-dir)/include/

base64-rs-toml := base64.rs/Cargo.toml
.PHONY: base64.rs/target/release/base64
base64.rs/target/release/base64: $(base64-rs-toml)
	$(CARGO_BUILD)

target/test.exe: test.cs | target
	$(MCS_BUILD)

.PHONY: target/Release/net5.0/base64
target/Release/net5.0/base64: base64.csproj | target
	$(DOTNET_BUILD)

base64.fsharp/target/Release/net5.0/base64: base64.fsharp/base64.fsproj | target
	$(DOTNET_BUILD)

target/base64_v_gcc: test.v | $(v_fmt)
	$(V_GCC_BUILD)

target/base64_v_clang: test.v | $(v_fmt)
	$(V_CLANG_BUILD)

target/base64_vala_gcc: test.vala | target
	$(VALAC_GCC_BUILD)

target/base64_vala_clang: test.vala | target
	$(VALAC_CLANG_BUILD)

# Run

.PHONY: run
run: $(all_runners)

## Common recipe for all runners
.PHONY: $(all_runners)
$(all_runners)::
	$(ECHO_RUN)

## Runners
executable_runners := $(patsubst %,run[%], $(executables))
$(executable_runners):: run[%] : %
	$(EXECUTABLE_RUN)

run[target/base64_scala.jar]:: run[%]: %
	$(SCALA_RUN) Base64

run[target/Base64Java.class]:: run[%]: %
	$(JAVA_CLASS_RUN)

run[target/Test-kt.jar]:: run[%]: %
	$(JAVA_JAR_RUN)

run[target/test.exe]:: run[%]: %
	$(MONO_RUN)

.PHONY: base64_perl
base64_perl:
	$(CPANM) "MIME::Base64::Perl"

run[test.pl]:: run[%]: % | base64_perl
	$(PERL_RUN)

run[test.jl]:: run[%]: % | $(julia_fmt)
	$(JULIA_RUN)

run[test.js]:: run[%]: %
	$(NODE_RUN)

run[pypy][test.py]:: run[pypy][%]: % | $(py_fmt)
	$(PYPY3_RUN)

run[test.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN)

run[test.rb]:: run[%]: % | $(rubocop)
	$(RUBY_RUN)

run[jit][test.rb]:: run[jit][%]: % | $(rubocop)
	$(RUBY_JIT_RUN)

run[truby-jvm][test.rb]:: run[truby-jvm][%]: % | $(rubocop)
	$(TRUBY_JVM_RUN)

run[truby-native][test.rb]:: run[truby-native][%]: % | $(rubocop)
	$(TRUBY_NATIVE_RUN)

run[jruby][test.rb]:: run[jruby][%]: % | $(rubocop)
	$(JRUBY_RUN)

run[test-xs.pl]:: run[%]: %
	$(PERL_RUN)

run[test.tcl]:: run[%]: %
	$(TCLSH_RUN)

run[test.php]:: run[%]: %
	$(PHP_RUN)

# Utilities

.PHONY: clean
clean:
	if [ -d $(aklomp-base64-dir) ]; then \
		$(MAKE) -C $(aklomp-base64-dir) clean; \
	fi
	cargo clean --manifest-path $(base64-rs-toml)
	-rm -rf target
