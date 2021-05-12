COMMON_FLAGS := -march=native -flto -Wa,-mbranches-within-32B-boundaries
GCC_FLAGS := -Wall -O3 $(COMMON_FLAGS)
CLANG_FLAGS := -Wall -O3 $(COMMON_FLAGS)
LIBNOTIFY_FLAGS := -I../common/libnotify ../common/libnotify/target/libnotify.a
NIM_FLAGS := -d:danger --verbosity:0 --opt:speed --hints:off --passC:"$(COMMON_FLAGS)" --passL:"-march=native -flto"
RUSTC_FLAGS := -C opt-level=3 -C target-cpu=native -C llvm-args=--x86-branches-within-32B-boundaries
VALAC_FLAGS := --disable-assert -X -O3 -X -march=native -X -flto -X -Wa,-mbranches-within-32B-boundaries --pkg gio-2.0 --pkg posix
V_FLAGS := -prod

CLANG_BUILD =		clang $(CLANG_FLAGS) -o $@ $^ $(LIBNOTIFY_FLAGS)
CRYSTAL_BUILD =	crystal build --release --no-debug -o $@ $^
DMD_BUILD =		dmd -of$@ -O -release -inline -boundscheck=off $^
DOTNET_BUILD =		dotnet build --nologo -v q $< -c Release
DUB_BUILD =		dub -q build --build=release-nobounds --compiler=ldc2 --single $^
GCC_BUILD =		gcc $(GCC_FLAGS) -std=c17 -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_CPP_BUILD =	g++ $(GCC_FLAGS) -std=c++2a -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_GO_BUILD =	gccgo $(GCC_FLAGS) -o $@ $^
GDC_BUILD =		gdc -o $@ -O3 -frelease -finline -fbounds-check=off $^
GHC_BUILD =		ghc -v0 -O2 -fforce-recomp -Wall $^ -o $@ -outputdir $(@D)
GO_BUILD =		GO111MODULE=auto go build -o $@ $^
JAVAC_BUILD =		javac -Xlint:unchecked -d $(@D) $^
# TODO: remove JAVA_OPTS as soon as new Kotlin is released (see https://youtrack.jetbrains.com/issue/KT-43704)
KOTLINC_BUILD =	JAVA_OPTS="--illegal-access=permit" kotlinc -include-runtime -jvm-target 15 -d $@ $^
LDC2_BUILD =		ldc2 -of$@ -O5 -release -boundscheck=off $^
MCS_BUILD =		mcs -debug- -optimize+ -out:$@ $^
MLTON_BUILD =		mlton -output $@ $^
NIM_CLANG_BUILD =	nim c -o:$@ --cc:clang $(NIM_FLAGS) $^
NIM_GCC_BUILD =	    nim c -o:$@ --cc:gcc $(NIM_FLAGS) $^
RUSTC_BUILD =		rustc $(RUSTC_FLAGS) -C lto -C codegen-units=1 -o $@ $^
SCALAC_BUILD =		scalac -d $@ $^
VALAC_CLANG_BUILD =	valac $^ --cc=clang -D CLANG_TEST $(VALAC_FLAGS) -o $@
VALAC_GCC_BUILD =	valac $^ --cc=gcc -D GCC_TEST $(VALAC_FLAGS) -o $@
V_CLANG_BUILD =	v $(V_FLAGS) -cc clang -o $@ $^
V_GCC_BUILD =		v $(V_FLAGS) -cc gcc -o $@ $^

define OCAML_BUILD =
cp $^ target && cd target && ocamlopt -O3 -unsafe unix.cmxa $^ -o $(@F)
endef

define CARGO_BUILD =
cargo fmt --manifest-path $<
cargo clippy -q --manifest-path $<
RUSTFLAGS="$(RUSTC_FLAGS)" cargo build -q --manifest-path $< --release
endef

ECHO_RUN = @tput bold; echo "$(MAKE) $@"; tput sgr0
XTIME := ../xtime.rb
CPANM := cpanm

LUAROCKS_LUA = PATH=$(PATH):/opt/luarocks/lua/bin luarocks
LUAROCKS_LUAJIT = PATH=$(PATH):/opt/luarocks/luajit/bin luarocks

CLOJURE_RUN =		$(XTIME) clojure $(CLOJURE_FLAGS) -M $^
ELIXIR_RUN =		$(XTIME) elixir $^
EXECUTABLE_RUN =	$(XTIME) $^
JAVA_CLASS_RUN =	$(XTIME) java -cp $(^D) $(basename $(^F))
JAVA_JAR_RUN =		$(XTIME) java -jar $^
JRUBY_RUN =		$(XTIME) jruby $^
LUA_JIT_RUN =		$(shell $(LUAROCKS_LUAJIT) path) && $(XTIME) luajit $^
LUA_RUN =		$(shell $(LUAROCKS_LUA) path) && $(XTIME) lua $^
MONO_RUN =		$(XTIME) mono -O=all --gc=sgen $^
NODE_RUN =		$(XTIME) node $^
PERL_RUN =		$(XTIME) perl $^
PHP_RUN =		$(XTIME) php $^
PYPY3_RUN =		$(XTIME) pypy3 $^
PYTHON3_RUN =		$(XTIME) python3 $^
RACKET_RUN =		PLT_CS_COMPILE_LIMIT=100000 $(XTIME) racket $^
RUBY_JIT_RUN =		$(XTIME) ruby --jit $^
RUBY_RUN =		$(XTIME) ruby $^
SCALA_RUN =		$(XTIME) scala -J-Xss100m -cp $^
SWIFT_RUN =		$(XTIME) swift -O $^
TCLSH_RUN =		$(XTIME) tclsh $^
TRUBY_JVM_RUN =	$(XTIME) truffleruby --jvm $^
TRUBY_NATIVE_RUN =	$(XTIME) truffleruby $^
SCHEME_RUN =		$(XTIME) scheme --optimize-level 3 --program $^
JULIA_RUN =		$(XTIME) julia --optimize=3 --check-bounds=no $^

GIT_CLONE = git clone --depth 1 -q
DOTNET_CLEAN = -dotnet clean --nologo -v q -c Release
NUGET_INSTALL = nuget install -ExcludeVersion -Verbosity quiet

py_fmt := target/.py_fmt
$(py_fmt): *.py | target
	@pip install -q black
	black $^
	@touch $@

julia_fmt := target/.julia_fmt
$(julia_fmt): *.jl | target
	../common/julia/julia_fmt.jl $^
	@touch $@

gofmt := target/.gofmt
$(gofmt): *.go | target
	gofmt -s -w $^
	@touch $@

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

.PHONY: libnotify
libnotify:
	$(MAKE) -C ../common/libnotify

target:
	mkdir -p target
