json_file := /tmp/1.json

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
	target/json_nim_gcc \
	target/json_nim_clang \
	target/packedjson_nim_gcc \
	target/packedjson_nim_clang \
	target/json_go \
	target/json_go_gccgo \
	target/json_boost_cpp \
	target/json_d_gdc_fast \
	target/json_rapid_cpp \
	target/json_rapid_sax_cpp \
	target/json_gason_cpp \
	target/json_libjson_cpp \
	target/json_hs \
	json-java/target/application \
	target/json_simdjson_cpp \
	target/json_v_gcc \
	target/json_v_clang \
	target/json_iter_go \
	target/json_dawjsonlink_cpp

artifacts := $(executables) \
	target/test.exe \
	target/Release/netcoreapp3.0/json.dll \
	json-core/target/Release/netcoreapp3.0/json-core.dll \
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
$(json-rs-targets) &: $(json-rs-toml)
	$(CARGO_BUILD)

target/json_d: test.d | target
	$(DMD_BUILD)

target/json_d_gdc: test.d | target
	$(GDC_BUILD)

target/json_d_ldc: test.d | target
	$(LDC2_BUILD)

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

target/json_boost_cpp: test_boost.cpp | target libnotify
	$(GCC_CPP_BUILD)

fast-dir := target/fast
$(fast-dir): | target
	git clone --depth 1 "https://github.com/mleise/fast.git" $@

target/json_d_gdc_fast: test_fast.d | $(fast-dir)
	$(GDC_BUILD) \
		$(fast-dir)/source/fast/cstring.d \
		$(fast-dir)/source/fast/buffer.d \
		$(fast-dir)/source/fast/json.d \
		$(fast-dir)/source/fast/parsing.d \
		$(fast-dir)/source/fast/intmath.d \
		$(fast-dir)/source/fast/internal/sysdef.di \
		$(fast-dir)/source/fast/internal/helpers.d \
		$(fast-dir)/source/fast/unicode.d \
		$(fast-dir)/source/fast/internal/unicode_tables.d \
		$(fast-dir)/source/std/simd.d

rapidjson-dir := target/rapidjson
$(rapidjson-dir): | target
	git clone --depth 1 "https://github.com/miloyip/rapidjson.git" $@

target/json_rapid_cpp: test_rapid.cpp | $(rapidjson-dir)
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include

target/json_rapid_sax_cpp: test_rapid_sax.cpp | $(rapidjson-dir)
	$(GCC_CPP_BUILD) -I$(rapidjson-dir)/include

gason-dir := target/gason
$(gason-dir): | target
	git clone --depth 1 "https://github.com/vivkin/gason.git" $@

target/json_gason_cpp: test_gason.cpp | $(gason-dir)
	$(GCC_CPP_BUILD) -I$(gason-dir)/src $(gason-dir)/src/gason.cpp

target/json_libjson_cpp: test_libjson.cpp | target
	$(GCC_CPP_BUILD) -ljson-c

newtonsoft_json_dir := target/Newtonsoft.Json/lib/net45
newtonsoft_json := $(newtonsoft_json_dir)/Newtonsoft.Json.dll
$(newtonsoft_json): | target
	nuget install Newtonsoft.Json -ExcludeVersion -OutputDirectory target

target/test.exe: test.cs | $(newtonsoft_json)
	$(MCS_BUILD) -r:$(newtonsoft_json)

.PHONY: target/Release/netcoreapp3.0/json.dll
target/Release/netcoreapp3.0/json.dll: json.csproj | target
	$(DOTNET_BUILD)

.PHONY: json-core/target/Release/netcoreapp3.0/json-core.dll
json-core/target/Release/netcoreapp3.0/json-core.dll: json-core/json-core.csproj
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
	git clone --depth 1 --no-checkout \
		https://github.com/simdjson/simdjson.git $@ \
	&& cd $@ \
	&& git sparse-checkout init --cone \
	&& git sparse-checkout set singleheader

target/json_simdjson_cpp: test_simdjson.cpp | $(simdjson-dir)
	$(GCC_CPP_BUILD) $(simdjson-dir)/singleheader/simdjson.cpp \
		-I$(simdjson-dir)/singleheader/

target/json_v_gcc: test.v | $(v_fmt)
	$(V_GCC_BUILD)

target/json_v_clang: test.v | $(v_fmt)
	$(V_CLANG_BUILD)

.PHONY: json-iterator
json-iterator:
	go get github.com/json-iterator/go

target/json_iter_go: test_jsoniter.go | json-iterator
	$(GO_BUILD)

daw_json_link_all := target/daw_json_link_all
$(daw_json_link_all): | target
	git clone --depth 1 \
		"https://github.com/beached/daw_json_link.git" \
		$@/daw_json_link
	git clone --depth 1 \
		"https://github.com/beached/header_libraries.git" \
		$@/header_libraries
	git clone --depth 1 \
		"https://github.com/beached/utf_range.git" \
		$@/utf_range

target/json_dawjsonlink_cpp: test_dawjsonlink.cpp | $(daw_json_link_all)
	$(GCC_CPP_BUILD) -I$(daw_json_link_all)/daw_json_link/include \
		-I$(daw_json_link_all)/header_libraries/include \
		-I$(daw_json_link_all)/utf_range/include

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

run[test.jl]:: run[%]: %
	$(JULIA_RUN)

run[target/test.exe]:: run[%]: %
	MONO_PATH=$(newtonsoft_json_dir) $(MONO_RUN)

run[target/Release/netcoreapp3.0/json.dll]:: run[%]: %
	$(DOTNET_RUN)

run[json-core/target/Release/netcoreapp3.0/json-core.dll]:: run[%]: %
	$(DOTNET_RUN)

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

run[pypy][test.py]:: run[pypy][%]: %
	$(PYPY3_RUN)

run[test.py]:: run[%]: %
	$(PYTHON3_RUN)

run[test_ujson.py]:: run[%]: %
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

run[test.clj]:: CLOJURE_FLAGS := -Sdeps '{:deps {cheshire {:mvn/version "5.10.0"}}}'
run[test.clj]:: run[%]: %
	$(CLOJURE_RUN)

# Utilities

.PHONY: clean
clean:
	$(MAKE) -C json-scala clean
	$(MAKE) -C json-java clean
	$(MAKE) -C json-hs clean
	cargo clean --manifest-path $(json-rs-toml)
	dotnet clean json-core/json-core.csproj
	-rm -rf target

$(json_file):
	ruby generate_json.rb

include ../common/commands.mk
