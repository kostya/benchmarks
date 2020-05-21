# The matrix size
SIZE := 1500

nim_opts := -d:danger --opt:speed --verbosity:0
julia_opts := --optimize=3 --check-bounds=no
crystal_opts := --release --no-debug
gccgo_opts := -O3 -g
rustc_opts := -C opt-level=3 -C lto
dmd_opts := -O -release -inline
gdc_opts := -O3 -frelease -finline
ldc2_opts := -O5 -release
dub_opts := --build=release --compiler=ldc2
gcc_opts := -O3
v_opts := -prod
mcs_opts := -debug- -optimize+

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
	target/matmul_v_clang

artifacts := $(executables) \
	target/matmul_scala.jar \
	target/matmul.class \
	target/matmul-kt.jar \
	target/matmul.exe \
	target/Release/netcoreapp3.0/matmul.dll

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
	crystal build $^ $(crystal_opts) -o $@

target/matmul_go: matmul.go | target
	gofmt -s -w $^
	go build -o $@ $^

target/matmul_go_gccgo: matmul.go | target
	gofmt -s -w $^
	gccgo $(gccgo_opts) -o $@ $^

target/matmul_rs: matmul.rs | target
	rustc $(rustc_opts) $^ -o $@

target/matmul_d: matmul.d | target
	dmd -of$@ $(dmd_opts) $^

target/matmul_d_gdc:matmul.d | target
	gdc -o $@ $(gdc_opts) $^

target/matmul_d_ldc:matmul.d | target
	ldc2 -of$@ $(ldc2_opts) $^

target/matmul_d_lubeck: matmul_d_lubeck.d | target
	dub build $(dub_opts) --single $^

target/matmul_nim_gcc: matmul.nim | target
	nim c -o:$@ $(nim_opts) $^

target/matmul_nim_clang:matmul.nim | target
	nim c -o:$@ --cc:clang $(nim_opts) $^

target/matmul_nim_arraymancer_gcc:matmul_arraymancer.nim | target
	nimble install -y arraymancer
	nim c -o:$@ -d:openmp --cc:gcc $(nim_opts) $^

target/matmul_nim_arraymancer_clang:matmul_arraymancer.nim | target
	nimble install -y arraymancer
	nim c -o:$@ -d:openmp --cc:clang $(nim_opts) $^

target/matmul_c: matmul.c | target
	make -C ../common/libnotify
	gcc $(gcc_opts) -o $@ $^ -I../common/libnotify -L../common/libnotify -lnotify

java-nd4j/target/application: java-nd4j/src/main/java/bench/matmulnd4j.java
	make -C java-nd4j target/application

target/matmul_v_gcc: matmul.v | target
	v fmt -w $^
	v $(v_opts) -cc gcc -o $@ $^

target/matmul_v_clang: matmul.v | target
	v fmt -w $^
	v $(v_opts) -cc clang -o $@ $^

target/matmul_scala.jar: matmul.scala | target
	scalac $^ -d $@

target/matmul.class: matmul.java | target
	javac -d $(@D) $^

target/matmul-kt.jar: matmul.kt | target
	kotlinc $^ -include-runtime -jvm-target 13 -d $@

target/matmul.exe: matmul.cs | target
	mcs $(mcs_opts) -out:$@ $^

target/Release/netcoreapp3.0/matmul.dll: matmul.cs | target
	dotnet build -c Release

# Run

.PHONY: run
run: $(all_runners)

## Common recipe for all runners
.PHONY: $(all_runners)
$(all_runners)::
	@echo "\e[1mmake $@\e[0m"

## Runners
executable_runners := $(patsubst %,run[%], $(executables))
$(executable_runners):: run[%] : %
	../xtime.rb $^ $(SIZE)

run[target/matmul_scala.jar]:: run[%]: %
	../xtime.rb scala -classpath $^ MatMul $(SIZE)

run[target/matmul.class]:: run[%]: %
	../xtime.rb java -cp $(^D) $(basename $(^F)) $(SIZE)

run[target/matmul-kt.jar]:: run[%]: %
	../xtime.rb java -jar $^ $(SIZE)

run[target/matmul.exe]:: run[%]: %
	../xtime.rb mono -O=all --gc=sgen $^ $(SIZE)

run[target/Release/netcoreapp3.0/matmul.dll]:: run[%]: %
	../xtime.rb dotnet $^ $(SIZE)

run[matmul.js]:: run[%]: %
	../xtime.rb node $^ $(SIZE)

run[matmul-native.jl]:: run[%]: %
	../xtime.rb julia $(julia_opts) $^ $(SIZE)

run[single_thread][matmul-native.jl]:: run[single_thread][%]: %
	OPENBLAS_NUM_THREADS=1 ../xtime.rb julia $(julia_opts) $^ $(SIZE)

run[matmul.jl]:: run[%]: %
	../xtime.rb julia $(julia_opts) $^ $(SIZE)

run[pypy][matmul.py]:: run[pypy][%]: %
	../xtime.rb pypy3 $^ $(SIZE)

run[matmul.py]:: run[%]: %
	../xtime.rb python3 $^ $(SIZE)

run[matmul-numpy.py]:: run[%]: %
	../xtime.rb python3 $^ $(SIZE)

run[matmul.pl]:: run[%]: %
	../xtime.rb perl $^ $(SIZE)

run[matmul.tcl]:: run[%]: %
	../xtime.rb tclsh $^ $(SIZE)

run[matmul.swift]:: run[%]: %
	../xtime.rb swift -O $^ $(SIZE)

target/matmul.rubocop: *.rb | target
	rubocop $^
	touch target/matmul.rubocop

run[matmul.rb]:: run[%]: % | target/matmul.rubocop
	../xtime.rb ruby $^ $(SIZE)

run[jit][matmul.rb]:: run[jit][%]: % | target/matmul.rubocop
	../xtime.rb ruby --jit $^ $(SIZE)

run[truby-jvm][matmul.rb]:: run[truby-jvm][%]: % | target/matmul.rubocop
	../xtime.rb truffleruby --jvm $^ $(SIZE)

run[truby-native][matmul.rb]:: run[truby-native][%]: % | target/matmul.rubocop
	../xtime.rb truffleruby $^ $(SIZE)

run[jruby][matmul.rb]:: run[jruby][%]: % | target/matmul.rubocop
	../xtime.rb jruby $^ $(SIZE)

# Utilities

target:
	mkdir -p target

.PHONY: clean
clean:
	-rm -rf target
	make -C java-nd4j clean
