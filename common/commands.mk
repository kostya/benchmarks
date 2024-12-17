COMMON_FLAGS := -Wall -Wextra -pedantic -Wcast-align -O3 -march=native -flto=auto -Wa,-mbranches-within-32B-boundaries
GCC_FLAGS := $(COMMON_FLAGS)
CLANG_FLAGS := $(COMMON_FLAGS)
LIBNOTIFY_FLAGS := -I../common/libnotify ../common/libnotify/target/libnotify.a
NIM_FLAGS := -d:danger --verbosity:0 --opt:speed --hints:off --passC:"$(COMMON_FLAGS)" --passL:"-march=native -flto=auto" --mm:markAndSweep
RUSTC_FLAGS := -C target-cpu=native -C llvm-args=--x86-branches-within-32B-boundaries
VALAC_FLAGS := --disable-assert -X -O3 -X -march=native -X -flto -X -Wa,-mbranches-within-32B-boundaries --pkg gio-2.0 --pkg posix
V_FLAGS := -prod -no-bounds-checking -prealloc -cflags "-Wno-error=implicit-function-declaration $(COMMON_FLAGS)"
V_VSL_CBLAS_FLAGS := $(V_FLAGS) -d cblas
ZIG_FLAGS := -O ReleaseFast

CLANG_BUILD =		clang $(CLANG_FLAGS) -std=c2x -o $@ $^ $(LIBNOTIFY_FLAGS)
CLANG_CPP_BUILD =	clang++ $(CLANG_FLAGS) -std=c++2b -stdlib=libc++ -o $@ $^ $(LIBNOTIFY_FLAGS)
CRYSTAL_BUILD =	crystal build --release --no-debug -o $@ $^
DMD_BUILD =		dmd -of$@ -O -release -inline -boundscheck=off $^
DOTNET_BUILD =		dotnet build --nologo -v q $< -c Release
DUB_BUILD =		dub -q build --build=release-nobounds --compiler=ldc2 --single $^
GCC_BUILD =		gcc $(GCC_FLAGS) -std=c2x -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_CPP_BUILD =	g++ $(GCC_FLAGS) -std=c++23 -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_GO_BUILD =	go  build  -C $< -compiler gccgo -buildvcs=false -gccgoflags="$(GCC_FLAGS)" -o $(abspath $@) .
GDC_BUILD =		gdc -o $@ -O3 -frelease -finline -fbounds-check=off $^
GHC_BUILD =		ghc -v0 -O2 -fforce-recomp -Wall $^ -o $@ -outputdir $(@D)
JAVAC_BUILD =		javac --release 22 -Xlint:unchecked -d $(@D) $^
KOTLINC_BUILD =	kotlinc -include-runtime -jvm-target 22 -d $@ $^
LDC2_BUILD =		ldc2 -of$@ -O5 -release -boundscheck=off $^
MCS_BUILD =		mcs -debug- -optimize+ -out:$@ $^
MLTON_BUILD =		mlton -output $@ $^
NIM_CLANG_BUILD =	nim c -o:$@ --cc:clang $(NIM_FLAGS) $^
NIM_GCC_BUILD =	nim c -o:$@ --cc:gcc $(NIM_FLAGS) $^
RUSTC_BUILD =		rustc $(RUSTC_FLAGS) -C opt-level=3 -C lto -C codegen-units=1 -C panic=abort -C strip=symbols -o $@ $^
RUST_CLIPPY =		clippy-driver -o $@.clippy $^
SWIFTC_BUILD =		swiftc -parse-as-library -O -lto=llvm-full -cross-module-optimization $(LIBNOTIFY_FLAGS) -import-objc-header ../common/libnotify/libnotify.h -o $@ $^
SCALAC_BUILD =		scalac -java-output-version 22 -d $@ $^
VALAC_CLANG_BUILD =	valac $^ --cc=clang -D CLANG_TEST $(VALAC_FLAGS) -o $@
VALAC_GCC_BUILD =	valac $^ --cc=gcc -D GCC_TEST $(VALAC_FLAGS) -o $@
V_CLANG_BUILD =	v $(V_FLAGS) -cc clang -o $@ $^
V_GCC_BUILD =		v $(V_FLAGS) -cc gcc -o $@ $^
V_VSL_CBLAS_CLANG_BUILD =	v $(V_VSL_CBLAS_FLAGS) -cc clang -o $@ $^
V_VSL_CBLAS_GCC_BUILD =		v $(V_VSL_CBLAS_FLAGS) -cc gcc -o $@ $^
ZIG_BUILD =		zig build-exe $(ZIG_FLAGS) -lc -femit-bin=$@ $^
#IDRIS_BUILD =		idris2 -p network $^ --output-dir $(@D) -o $(@F)
IDRIS_BUILD = pack build

define OCAML_BUILD =
cp $^ target && cd target && ocamlopt -O3 -unsafe -I +unix unix.cmxa $^ -o $(@F)
endef

define CARGO_BUILD =
cargo fmt --manifest-path $<
cargo clippy -q --manifest-path $<
RUSTFLAGS="$(RUSTC_FLAGS)" cargo build -q --manifest-path $< --release
endef

ECHO_RUN = @tput bold; echo "$(MAKE) $@"; tput sgr0
XTIME := ../xtime.rb
CPANM := cpanm -l ~/perl5

CLOJURE_RUN =		$(XTIME) clojure $(CLOJURE_FLAGS) -M $^
ELIXIR_RUN =		$(XTIME) elixir $^
EXECUTABLE_RUN =	$(XTIME) $^
JAVA_CLASS_RUN =	$(XTIME) java -cp $(^D) $(basename $(^F))
JAVA_JAR_RUN =		$(XTIME) java -jar $^
JRUBY_RUN =		$(XTIME) jruby $^
LUA_JIT_RUN =		$(XTIME) luajit $^
LUA_RUN =		$(XTIME) lua $^
MONO_RUN =		$(XTIME) mono -O=all --gc=sgen $^
NODE_RUN =		$(XTIME) node $^
PERL_RUN =		$(XTIME) perl -I ~/perl5/lib/perl5 $^
PHP_RUN =		$(XTIME) php $^
PYPY3_RUN =		$(XTIME) pypy3 $^
PYTHON3_RUN =		$(XTIME) python3 $^
RACKET_RUN =		PLT_CS_COMPILE_LIMIT=100000 $(XTIME) racket $^
RUBY_JIT_RUN =		$(XTIME) ruby --jit $^
RUBY_RUN =		$(XTIME) ruby $^
SCALA_RUN =		$(XTIME) scala -J-Xss100m -cp $^
TCLSH_RUN =		$(XTIME) tclsh $^
TRUBY_JVM_RUN =		$(XTIME) truffleruby-jvm-bin $^
TRUBY_NATIVE_RUN =	$(XTIME) truffleruby $^
SCHEME_RUN =		$(XTIME) scheme --optimize-level 3 --program $^
JULIA_RUN =		$(XTIME) julia --optimize=3 --check-bounds=no $^
PHP_RUN =		$(XTIME) php $^

GIT_CLONE = git clone --depth 1 -q
DOTNET_CLEAN = -dotnet clean --nologo -v q -c Release
NUGET_INSTALL = nuget install -ExcludeVersion -Verbosity quiet

py_fmt := target/.py_fmt
$(py_fmt): *.py | target
	@pipx install black
	black $^
	@touch $@

julia_fmt := target/.julia_fmt
$(julia_fmt): *.jl | target
	../common/julia/julia_fmt.jl $^
	@touch $@

.PHONY: gofmt
gofmt:
	gofmt -s -w .

GO_SOURCE = $(wildcard go/* go/*/*.go ../common/go/*)

go/go.sum: $(filter-out go/go.sum, $(GO_SOURCE))
	cd go && go mod tidy && go get -u ./... && touch go.sum

GO_TARGETS := $(patsubst %/,%/target/benchmark, $(shell ls -d go/*/))

$(GO_TARGETS): $(GO_SOURCE) | gofmt
	go build -C $(dir $(@D)) -buildvcs=false -ldflags="-s -w" -o $(abspath $@) .

.PHONY: clean-go
clean-go:
	-rm -rf go/*/target

rubocop := target/.rubocop
$(rubocop): *.rb | target
	@gem i --conservative rubocop
	rubocop $^
	@touch $@

v_fmt := target/.v_fmt
$(v_fmt): *.v | target
	v fmt -w $^
	@touch $@

dfmt := target/.dfmt
$(dfmt): *.d | target
	dub -q run -y dfmt -- -i $^
	@touch $@

hlint := target/.hlint
$(hlint): *.hs | target
	~/.cabal/bin/hlint $^
	@touch $@

rs_fmt := target/.rs_fmt
$(rs_fmt): *.rs | target
	rustfmt $^
	@touch $@

zig_fmt := target/.zig_fmt
$(zig_fmt): *.zig | target
	zig fmt $^
	@touch $@

.PHONY: libnotify
libnotify:
	$(MAKE) -C ../common/libnotify

target:
	mkdir -p target
