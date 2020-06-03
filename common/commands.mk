GCC_FLAGS := -O3 -Wall -Wextra -pedantic -flto
LIBNOTIFY_FLAGS := -I../common/libnotify -L../common/libnotify -lnotify
NIM_FLAGS := -d:danger --verbosity:0 --opt:speed --hints:off
VALAC_FLAGS := --disable-assert -X -O3 --pkg gio-2.0 --pkg posix
V_FLAGS := -prod

CARGO_BUILD =		cargo build --manifest-path $< --release
CLANG_BUILD =		clang -O3 -o $@ $^ $(LIBNOTIFY_FLAGS)
CRYSTAL_BUILD =	crystal build --release --no-debug -o $@ $^
DMD_BUILD =		dmd -of$@ -O -release -inline $^
DOTNET_BUILD =		dotnet build $< -c Release
GCC_BUILD =		gcc $(GCC_FLAGS) -std=c17 -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_CPP_BUILD =	g++ $(GCC_FLAGS) -std=c++17 -o $@ $^ $(LIBNOTIFY_FLAGS)
GCC_GO_BUILD =		gccgo -O3 -o $@ $^
GDC_BUILD =		gdc -o $@ -O3 -frelease -finline $^
GHC_BUILD =		ghc -O2 -fforce-recomp $^ -o $@ -outputdir $(@D)
GO_BUILD =		go build -o $@ $^
JAVAC_BUILD =		javac -d $(@D) $^
KOTLINC_BUILD =	kotlinc -include-runtime -jvm-target 13 -d $@ $^
LDC2_BUILD =		ldc2 -of$@ -O5 -release $^
MCS_BUILD =		mcs -debug- -optimize+ -out:$@ $^
MLTON_BUILD =		mlton -output $@ $^
NIM_CLANG_BUILD =	nim c -o:$@ --cc:clang $(NIM_FLAGS) $^
NIM_GCC_BUILD =	nim c -o:$@ --cc:gcc $(NIM_FLAGS) $^
RUSTC_BUILD =		rustc -C opt-level=3 -C lto -o $@ $^
SCALAC_BUILD =		scalac -d $@ $^
VALAC_CLANG_BUILD =	valac $^ --cc=clang -D CLANG_TEST $(VALAC_FLAGS) -o $@
VALAC_GCC_BUILD =	valac $^ --cc=gcc -D GCC_TEST $(VALAC_FLAGS) -o $@
V_CLANG_BUILD =	v $(V_FLAGS) -cc clang -o $@ $^
V_GCC_BUILD =		v $(V_FLAGS) -cc gcc -o $@ $^

define OCAML_BUILD =
cp $^ target && cd target && ocamlopt -O3 -unsafe unix.cmxa $^ -o $(@F)
endef

ECHO_RUN = @echo "\e[1m$(MAKE) $@\e[0m"
XTIME := ../xtime.rb
CPANM := cpanm

CLOJURE_RUN =		$(XTIME) clojure $(CLOJURE_FLAGS) $^
DOTNET_RUN =		$(XTIME) dotnet $^
ELIXIR_RUN =		$(XTIME) elixir $^
EXECUTABLE_RUN =	$(XTIME) $^
JAVA_CLASS_RUN =	$(XTIME) java -cp $(^D) $(basename $(^F))
JAVA_JAR_RUN =		$(XTIME) java -jar $^
JRUBY_RUN =		$(XTIME) jruby $^
JULIA_RUN =		$(XTIME) julia --optimize=3 --check-bounds=no $^
LUA_JIT_RUN =		$(XTIME) luajit $^
LUA_RUN =		$(XTIME) lua5.3 $^
MONO_RUN =		$(XTIME) mono -O=all --gc=sgen $^
NODE_RUN =		$(XTIME) node $^
PERL_RUN =		$(XTIME) perl $^
PHP_RUN =		$(XTIME) php $^
PYPY3_RUN =		$(XTIME) pypy3 $^
PYTHON3_RUN =		$(XTIME) python3 $^
RACKET_RUN =		$(XTIME) racket $^
RUBY_JIT_RUN =		$(XTIME) ruby --jit $^
RUBY_RUN =		$(XTIME) ruby $^
SCALA_RUN =		$(XTIME) scala -J-Xss100m -cp $^
SWIFT_RUN =		$(XTIME) swift -O $^
TCLSH_RUN =		$(XTIME) tclsh $^
TRUBY_JVM_RUN =	$(XTIME) truffleruby --jvm $^
TRUBY_NATIVE_RUN =	$(XTIME) truffleruby $^
SCHEME_RUN =		$(XTIME) scheme --optimize-level 3 --program $^

gofmt := target/.gofmt
$(gofmt): *.go | target
	gofmt -s -w $^
	@touch $@

rubocop := target/.rubocop
$(rubocop): *.rb | target
	rubocop $^
	@touch $@

v_fmt := target/.v_fmt
$(v_fmt): *.v | target
	v fmt -w $^
	@touch $@

.PHONY: libnotify
libnotify:
	$(MAKE) -C ../common/libnotify

target:
	mkdir -p target
