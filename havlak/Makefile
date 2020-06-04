include ../common/commands.mk
.DEFAULT_GOAL := build

executables := \
	target/havlak_cr \
	target/havlak_go \
	target/havlak_go_gccgo \
	target/havlak_cpp \
	target/havlak_d \
	target/havlak_d_gdc \
	target/havlak_d_ldc \
	target/havlak_nim_gcc \
	target/havlak_nim_clang

artifacts := $(executables) \
	target/havlak_scala.jar \
	target/havlak.exe \
	target/Release/netcoreapp3.0/havlak.dll

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[pypy][havlak.py] \
	run[havlak.py] \

# Build

.PHONY: build
build: $(artifacts)

target/havlak_cr: havlak.cr | target
	$(CRYSTAL_BUILD)

target/havlak_go: havlak.go | $(gofmt)
	$(GO_BUILD)

target/havlak_go_gccgo: havlak.go | $(gofmt)
	$(GCC_GO_BUILD)

target/havlak_cpp: havlak.cpp | target libnotify
	$(GCC_CPP_BUILD)

target/havlak_d: havlak.d | $(dfmt)
	$(DMD_BUILD)

target/havlak_d_gdc: havlak.d | $(dfmt)
	$(GDC_BUILD)

target/havlak_d_ldc: havlak.d | $(dfmt)
	$(LDC2_BUILD)

target/havlak_scala.jar: havlak.scala | target
	$(SCALAC_BUILD)

target/havlak_nim_gcc: havlak.nim | target
	$(NIM_GCC_BUILD)

target/havlak_nim_clang: havlak.nim | target
	$(NIM_CLANG_BUILD)

target/havlak.exe: havlak.cs | target
	$(MCS_BUILD)

.PHONY: target/Release/netcoreapp3.0/havlak.dll
target/Release/netcoreapp3.0/havlak.dll: havlak.csproj | target
	$(DOTNET_BUILD)

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

run[target/havlak_scala.jar]:: run[%]: %
	$(SCALA_RUN) LoopTesterApp

run[target/havlak.exe]:: run[%]: %
	$(MONO_RUN)

run[target/Release/netcoreapp3.0/havlak.dll]:: run[%]: %
	$(DOTNET_RUN)

run[pypy][havlak.py]:: run[pypy][%]: %
	$(PYPY3_RUN)

run[havlak.py]:: run[%]: %
	$(PYTHON3_RUN)

# Utilities

.PHONY: clean
clean:
	-rm -rf target
