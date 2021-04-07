# The matrix size
MSIZE := 1500

include ../common/commands.mk
.DEFAULT_GOAL := build

executables := \
	target/matmul_nim_gcc \
	target/matmul_nim_clang \
	target/matmul_nim_arraymancer_gcc \
	target/matmul_nim_arraymancer_clang \
	target/matmul_cr \
	target/matmul_go \
	target/matmul_go_gccgo \
	target/matmul_c \
	target/matmul_rs \
	target/matmul_d \
	target/matmul_d_gdc \
	target/matmul_d_ldc \
	target/matmul_d_lubeck \
	java-nd4j/target/application \
	target/matmul_v_gcc \
	target/matmul_v_clang \
	target/matmul_vala_gcc \
	target/matmul_vala_clang \
	target/Release/net5.0/matmul \
	target/matmul_cpp

artifacts := $(executables) \
	target/matmul_scala.jar \
	target/matmul.class \
	target/matmul-kt.jar \
	target/matmul.exe

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[matmul.js] \
	run[matmul-native.jl] \
	run[single_thread][matmul-native.jl] \
	run[matmul.jl] \
	run[pypy][matmul.py] \
	run[matmul.py] \
	run[matmul-numpy.py] \
	run[matmul.pl] \
	run[matmul.tcl] \
	run[matmul.swift] \
	run[matmul.rb] \
	run[jit][matmul.rb] \
	run[truby-jvm][matmul.rb] \
	run[truby-native][matmul.rb] \
	run[jruby][matmul.rb]

# Build

.PHONY: build
build: $(artifacts)

target/matmul_cr: matmul.cr | target
	$(CRYSTAL_BUILD)

target/matmul_go: matmul.go | $(gofmt)
	$(GO_BUILD)

target/matmul_go_gccgo: matmul.go | $(gofmt)
	$(GCC_GO_BUILD)

target/matmul_rs: matmul.rs | target
	$(RUSTC_BUILD)

target/matmul_d: matmul.d | $(dfmt)
	$(DMD_BUILD)

target/matmul_d_gdc:matmul.d | $(dfmt)
	$(GDC_BUILD)

target/matmul_d_ldc:matmul.d | $(dfmt)
	$(LDC2_BUILD)

target/matmul_d_lubeck: matmul_d_lubeck.d | $(dfmt)
	$(DUB_BUILD)

target/matmul_nim_gcc: matmul.nim | target
	$(NIM_GCC_BUILD)

target/matmul_nim_clang: matmul.nim | target
	$(NIM_CLANG_BUILD)

.PHONY: arraymancer
arraymancer:
	nimble install -y arraymancer

target/matmul_nim_arraymancer_gcc: NIM_FLAGS += -d:openmp
target/matmul_nim_arraymancer_gcc: matmul_arraymancer.nim | target arraymancer
	$(NIM_GCC_BUILD)

target/matmul_nim_arraymancer_clang: NIM_FLAGS += -d:openmp
target/matmul_nim_arraymancer_clang: matmul_arraymancer.nim | target arraymancer
	$(NIM_CLANG_BUILD)

target/matmul_c: matmul.c | target libnotify
	$(GCC_BUILD)

# Marked phony to run always
.PHONY: java-nd4j/target/application
java-nd4j/target/application:
	$(MAKE) -C java-nd4j target/application

target/matmul_v_gcc: matmul.v | $(v_fmt)
	$(V_GCC_BUILD)

target/matmul_v_clang: matmul.v | $(v_fmt)
	$(V_CLANG_BUILD)

target/matmul_vala_gcc: matmul.vala | target
	$(VALAC_GCC_BUILD)

target/matmul_vala_clang: matmul.vala | target
	$(VALAC_CLANG_BUILD)

target/matmul_scala.jar: matmul.scala | target
	$(SCALAC_BUILD)

target/matmul.class: matmul.java | target
	$(JAVAC_BUILD)

target/matmul-kt.jar: matmul.kt | target
	$(KOTLINC_BUILD)

target/matmul.exe: matmul.cs | target
	$(MCS_BUILD)

.PHONY: target/Release/net5.0/matmul
target/Release/net5.0/matmul: matmul.csproj | target
	$(DOTNET_BUILD)

eigen := target/eigen
$(eigen): | target
	$(GIT_CLONE) "https://gitlab.com/libeigen/eigen.git" $@

target/matmul_cpp: matmul.cpp | $(eigen) libnotify
	$(GCC_CPP_BUILD) -I$(eigen)

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
	$(EXECUTABLE_RUN) $(MSIZE)

run[target/matmul_scala.jar]:: run[%]: %
	$(SCALA_RUN) MatMul $(MSIZE)

run[target/matmul.class]:: run[%]: %
	$(JAVA_CLASS_RUN) $(MSIZE)

run[target/matmul-kt.jar]:: run[%]: %
	$(JAVA_JAR_RUN) $(MSIZE)

run[target/matmul.exe]:: run[%]: %
	$(MONO_RUN) $(MSIZE)

run[matmul.js]:: run[%]: %
	$(NODE_RUN) $(MSIZE)

run[matmul-native.jl]:: run[%]: % | $(julia_fmt)
	$(JULIA_RUN) $(MSIZE)

run[single_thread][matmul-native.jl]:: run[single_thread][%]: % | $(julia_fmt)
	OPENBLAS_NUM_THREADS=1 $(JULIA_RUN) $(MSIZE)

run[matmul.jl]:: run[%]: %  | $(julia_fmt)
	$(JULIA_RUN) $(MSIZE)

run[pypy][matmul.py]:: run[pypy][%]: % | $(py_fmt)
	$(PYPY3_RUN) $(MSIZE)

run[matmul.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN) $(MSIZE)

run[matmul-numpy.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN) $(MSIZE)

run[matmul.pl]:: run[%]: %
	$(PERL_RUN) $(MSIZE)

run[matmul.tcl]:: run[%]: %
	$(TCLSH_RUN) $(MSIZE)

run[matmul.swift]:: run[%]: %
	$(SWIFT_RUN) $(MSIZE)

run[matmul.rb]:: run[%]: % | $(rubocop)
	$(RUBY_RUN) $(MSIZE)

run[jit][matmul.rb]:: run[jit][%]: % | $(rubocop)
	$(RUBY_JIT_RUN) $(MSIZE)

run[truby-jvm][matmul.rb]:: run[truby-jvm][%]: % | $(rubocop)
	$(TRUBY_JVM_RUN) $(MSIZE)

run[truby-native][matmul.rb]:: run[truby-native][%]: % | $(rubocop)
	$(TRUBY_NATIVE_RUN) $(MSIZE)

run[jruby][matmul.rb]:: run[jruby][%]: % | $(rubocop)
	$(JRUBY_RUN) $(MSIZE)

# Utilities

.PHONY: clean
clean:
	-rm -rf target
	$(MAKE) -C java-nd4j clean
