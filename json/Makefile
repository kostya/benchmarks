json_file := /tmp/1.json

include ../common/commands.mk
.DEFAULT_GOAL := build

json-rs-targets := \
	json.rs/target/release/json-pull-rs \
	json.rs/target/release/json-struct-rs \
	json.rs/target/release/json-value-rs \
	json.rs/target/release/json-jq-rs

executables := \
	target/json_cr \
	target/json_pull_cr \
	target/json_schema_cr \
	$(json-rs-targets) \
	target/json_d \
	target/json_d_gdc \
	target/json_d_ldc \
	target/json_d_mir_asdf \
	target/json_d_mir_ion \
	target/json_nim_gcc \
	target/json_nim_clang \
	target/packedjson_nim_gcc \
	target/packedjson_nim_clang \
	target/json_go \
	target/json_go_gccgo \
	target/json_iter_go \
	target/json_boost_ptree_cpp \
	target/json_boost_json_cpp \
	target/json_rapid_cpp \
	target/json_rapid_sax_cpp \
	target/json_rapid_cpp_precised \
	target/json_rapid_sax_cpp_precised \
	target/json_gason_cpp \
	target/json_libjson_cpp \
	target/json_hs \
	json-java/target/application \
	target/json_simdjson_cpp \
	target/json_simdjson_dom_cpp \
	target/json_v_gcc \
	target/json_v_clang \
	target/json_vala_gcc \
	target/json_vala_clang \
	target/json_dawjsonlink_cpp \
	target/json_dawjsonlink_cpp_unchecked \
	json-fsharp/target/Release/net5.0/json-fsharp \
	target/Release/net5.0/json \
	json-core/target/Release/net5.0/json-core \
	target/json_nlohmann_json_cpp

artifacts := $(executables) \
	target/test.exe \
	json-scala/target/application.jar

all_runners := $(patsubst %,run[%], $(artifacts)) \
	run[test.jl] \
	run[test_yajl.rb] \
	run[test.pl] \
	run[test-xs.pl] \
	run[test.js] \
	run[pypy][test.py] \
	run[test.py] \
	run[test_ujson.py] \
	run[test.rb] \
	run[jit][test.rb] \
	run[truby-jvm][test.rb] \
	run[truby-native][test.rb] \
	run[jruby][test.rb] \
	run[test.php] \
	run[test.clj]

# Build

.PHONY: build
build: $(artifacts)

target/json_cr: test.cr | target
	$(CRYSTAL_BUILD)

target/json_pull_cr: test_pull.cr | target
	$(CRYSTAL_BUILD)

target/json_schema_cr: test_schema.cr | target
	$(CRYSTAL_BUILD)

json-rs-toml := json.rs/Cargo.toml
.PHONY: $(json-rs-targets)
$(json-rs-targets): $(json-rs-toml)
	$(CARGO_BUILD)

target/json_d: test.d | $(dfmt)
	$(DMD_BUILD)

target/json_d_gdc: test.d | $(dfmt)
	$(GDC_BUILD)

target/json_d_ldc: test.d | $(dfmt)
	$(LDC2_BUILD)

target/json_d_mir_asdf: test_mir_asdf.d | $(dfmt)
	$(DUB_BUILD)

target/json_d_mir_ion: test_mir_ion.d | $(dfmt)
	$(DUB_BUILD)

# NOTE: the test is not used in the benchmark
# as it operates the file directly, not string as other tests.
# Left it here for the reference.
target/json_d_mir_ion_file: test_mir_ion_file.d | $(dfmt)
	$(DUB_BUILD)

target/json_nim_gcc: test.nim | target
	$(NIM_GCC_BUILD)

target/json_nim_clang: test.nim | target
	$(NIM_CLANG_BUILD)

.PHONY: packedjson
packedjson:
	nimble -y install packedjson

target/packedjson_nim_gcc: test_packedjson.nim | target packedjson
	$(NIM_GCC_BUILD)

target/packedjson_nim_clang: test_packedjson.nim | target packedjson
	$(NIM_CLANG_BUILD)

target/json_go: test.go | $(gofmt)
	$(GO_BUILD)

target/json_go_gccgo: test.go | $(gofmt)
	$(GCC_GO_BUILD)

# NOTE: quite time consuming
# TODO: switch to Debian package as soon as they update to 1.75.0
# https://packages.debian.org/bullseye/libboost-dev

# Boost releases:
# https://www.boost.org/users/download/
BOOST_VERSION := boost_1_76_0
boost = target/$(BOOST_VERSION)
$(boost): | target
	cd target && \
	wget --progress=dot:giga -O - \
	https://boostorg.jfrog.io/artifactory/main/release/1.76.0/source/$(BOOST_VERSION).tar.bz2 \
	| tar -xj

target/json_boost_ptree_cpp: test_boost_ptree.cpp | $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(boost)

target/json_boost_json_cpp: test_boost_json.cpp | $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(boost)

nlohmann_json := target/nlohmann_json
$(nlohmann_json): | target
	$(GIT_CLONE) "https://github.com/nlohmann/json.git" $@

target/json_nlohmann_json_cpp: test_nlohmann_json.cpp | $(nlohmann_json) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(nlohmann_json)/single_include -I$(boost)

rapidjson-dir := target/rapidjson
$(rapidjson-dir): | target
	$(GIT_CLONE) "https://github.com/miloyip/rapidjson.git" $@

target/json_rapid_cpp: test_rapid.cpp | $(rapidjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include -I$(boost)

target/json_rapid_cpp_precised: test_rapid.cpp | $(rapidjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include -I$(boost) -DPRECISED

target/json_rapid_sax_cpp: test_rapid_sax.cpp | $(rapidjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include -I$(boost)

target/json_rapid_sax_cpp_precised: test_rapid_sax.cpp | $(rapidjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include -I$(boost) -DPRECISED

gason-dir := target/gason
$(gason-dir): | target
	$(GIT_CLONE) "https://github.com/vivkin/gason.git" $@

target/json_gason_cpp: test_gason.cpp | $(gason-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(gason-dir)/src -I$(boost) $(gason-dir)/src/gason.cpp

target/json_libjson_cpp: test_libjson.cpp | $(boost) target libnotify
	$(GCC_CPP_BUILD) -I$(boost) -ljson-c

newtonsoft_json_dir := target/Newtonsoft.Json/lib/net45
newtonsoft_json := $(newtonsoft_json_dir)/Newtonsoft.Json.dll
$(newtonsoft_json): | target
	$(NUGET_INSTALL) Newtonsoft.Json -OutputDirectory target

target/test.exe: test.cs | $(newtonsoft_json)
	$(MCS_BUILD) -r:$(newtonsoft_json)

.PHONY: target/Release/net5.0/json
target/Release/net5.0/json: json.csproj | target
	$(DOTNET_BUILD)

.PHONY: json-core/target/Release/net5.0/json-core
json-core/target/Release/net5.0/json-core: json-core/json-core.csproj
	$(DOTNET_BUILD)

.PHONY: target/json_hs
target/json_hs:
	$(MAKE) -C json-hs

.PHONY: json-java/target/application
json-java/target/application:
	$(MAKE) -C json-java target/application

.PHONY: json-scala/target/application.jar
json-scala/target/application.jar:
	$(MAKE) -C json-scala target/application.jar

simdjson-dir := target/simdjson

$(simdjson-dir): | target
	$(GIT_CLONE) "https://github.com/simdjson/simdjson.git" $@

target/json_simdjson_cpp: test_simdjson.cpp | $(simdjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) $(simdjson-dir)/singleheader/simdjson.cpp \
		-I$(simdjson-dir)/singleheader/ -I$(boost)

target/json_simdjson_dom_cpp: test_simdjson_dom.cpp | $(simdjson-dir) $(boost) libnotify
	$(GCC_CPP_BUILD) $(simdjson-dir)/singleheader/simdjson.cpp \
		-I$(simdjson-dir)/singleheader/ -I$(boost)

target/json_v_gcc: test.v | $(v_fmt)
	$(V_GCC_BUILD)

target/json_v_clang: test.v | $(v_fmt)
	$(V_CLANG_BUILD)

target/json_vala_gcc: test.vala | target
	$(VALAC_GCC_BUILD) --pkg json-glib-1.0

target/json_vala_clang: test.vala | target
	$(VALAC_CLANG_BUILD) --pkg json-glib-1.0

.PHONY: json-iterator
json-iterator:
	GO111MODULE=auto go get github.com/json-iterator/go

target/json_iter_go: test_jsoniter.go | json-iterator
	$(GO_BUILD)

daw_json_link_all := target/daw_json_link_all
$(daw_json_link_all): | target
	$(GIT_CLONE) "https://github.com/beached/daw_json_link.git" $@/daw_json_link
	$(GIT_CLONE) "https://github.com/beached/header_libraries.git" $@/header_libraries
	$(GIT_CLONE) "https://github.com/beached/utf_range.git" $@/utf_range

target/json_dawjsonlink_cpp: test_dawjsonlink.cpp | $(daw_json_link_all) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(daw_json_link_all)/daw_json_link/include \
		-I$(daw_json_link_all)/header_libraries/include \
		-I$(daw_json_link_all)/utf_range/include -I$(boost)

target/json_dawjsonlink_cpp_unchecked: test_dawjsonlink.cpp | $(daw_json_link_all) $(boost) libnotify
	$(GCC_CPP_BUILD) -I$(daw_json_link_all)/daw_json_link/include \
		-I$(daw_json_link_all)/header_libraries/include \
		-I$(daw_json_link_all)/utf_range/include -I$(boost) -DUNCHECKED

json-fsharp/target/Release/net5.0/json-fsharp: json-fsharp/json-fsharp.fsproj json-fsharp/Program.fs
	$(DOTNET_BUILD)

# Run

.PHONY: run
run: $(all_runners)

## Common recipe for all runners
.PHONY: $(all_runners)
$(all_runners):: $(json_file)
	$(ECHO_RUN)

## Runners
executable_runners := $(patsubst %,run[%], $(executables))
$(executable_runners):: run[%] : %
	$(EXECUTABLE_RUN)

run[test.jl]:: run[%]: % | $(julia_fmt)
	$(JULIA_RUN)

run[target/test.exe]:: run[%]: %
	MONO_PATH=$(newtonsoft_json_dir) $(MONO_RUN)

.PHONY: yajl-ruby
yajl-ruby:
	gem install yajl-ruby

run[test_yajl.rb]:: run[%]: % | yajl-ruby
	$(RUBY_RUN)

.PHONY: file-slurper
file-slurper:
	$(CPANM) "File::Slurper"

.PHONY: json-tiny
json-tiny:
	$(CPANM) "JSON::Tiny"

.PHONY: cpanel-json-xs
cpanel-json-xs:
	$(CPANM) "Cpanel::JSON::XS"

run[test.pl]:: run[%]: % | file-slurper json-tiny
	$(PERL_RUN)

run[test-xs.pl]:: run[%]: % | file-slurper cpanel-json-xs
	$(PERL_RUN)

run[json-scala/target/application.jar]:: run[%]: %
	$(SCALA_RUN) JsonTest

run[test.js]:: run[%]: %
	$(NODE_RUN)

run[pypy][test.py]:: run[pypy][%]: % | $(py_fmt)
	$(PYPY3_RUN)

run[test.py]:: run[%]: % | $(py_fmt)
	$(PYTHON3_RUN)

run[test_ujson.py]:: run[%]: % | $(py_fmt)
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

run[test.php]:: run[%]: %
	$(PHP_RUN)

run[test.clj]:: CLOJURE_FLAGS := -Sdeps '{:deps {cheshire/cheshire {:mvn/version "5.10.0"}}}'
run[test.clj]:: run[%]: %
	$(CLOJURE_RUN)

# Utilities

.PHONY: clean
clean:
	$(MAKE) -C json-scala clean
	$(MAKE) -C json-java clean
	$(MAKE) -C json-hs clean
	$(DOTNET_CLEAN) json-core/json-core.csproj
	$(DOTNET_CLEAN) json-fsharp/json-fsharp.fsproj
	cargo clean --manifest-path $(json-rs-toml)
	-rm -rf target
	-rm -rf mir-common-json/target

$(json_file):
	ruby generate_json.rb
