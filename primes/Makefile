include ../common/commands.mk
.DEFAULT_GOAL := build

executables := target/primes_cpp \
	target/primes_rs

artifacts := $(executables) \
	target/primes_scala.jar \
	target/Primes.class

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[pypy][primes.py] \
	run[primes.py] \
	run[primes.js]

# Build

.PHONY: build
build: $(artifacts)

target/primes_scala.jar: primes.scala | target
	$(SCALAC_BUILD)

target/Primes.class: Primes.java | target
	$(JAVAC_BUILD)

target/primes_rs: primes.rs | target
	$(RUSTC_BUILD)

target/primes_cpp: primes.cpp | target libnotify
	$(GCC_CPP_BUILD)

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

run[target/primes_scala.jar]:: run[%]: %
	$(SCALA_RUN) Primes

run[target/Primes.class]:: run[%]: %
	$(JAVA_CLASS_RUN)

run[pypy][primes.py]:: run[pypy][%]: % | $(py_fmt)
	$(PYPY3_RUN)

run[primes.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN)

run[primes.js]:: run[%]: %
	$(NODE_RUN)

# Utilities

.PHONY: clean
clean:
	-rm -rf target
