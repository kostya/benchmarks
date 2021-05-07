include ../common/commands.mk
.DEFAULT_GOAL := build

executables :=

artifacts := $(executables) \
	target/primes_scala.jar

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[pypy][primes.py] \
	run[primes.py] \
	run[primes.js]

# Build

.PHONY: build
build: $(artifacts)

target/primes_scala.jar: primes.scala | target
	$(SCALAC_BUILD)

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
