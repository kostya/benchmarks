sources=$(shell find src/ -type f -name '*.scala')
main=JsonTest

.PHONY: all
all: target/library.jar

# contains jars of all dependencies (including transitive ones)
.PHONY: target/dependencies.list
target/dependencies.list: Maven.list | target
	coursier fetch $(shell sed '/^#/d' Maven.list) > $@
	sed -i '/scala-.*/d' $@

# concatenate dependencies to be parseable as a classpath argument
target/classpath.line: target/dependencies.list
	paste --serial --delimiters ':' $^ > $@

# compile scala source files (note that a stamp file is used, since
# many classfiles may be created per source file)
target/classfiles.stamp: $(sources) target/classpath.line
	mkdir -p target/classfiles
	scalac \
	  -d target/classfiles \
	  -cp $(shell cat target/classpath.line) \
	$(sources)
	touch target/classfiles.stamp

# bundle classfiles into a jar
target/library.jar: target/classfiles.stamp
	jar cf $@ -C target/classfiles .

# combine lirary jar with all dependencies to produce a fat jar
target/application.jar: target/library.jar target/dependencies.list
	mkdir -p target
	../../common/jvm/combinejars.sh $@ $(shell cat target/dependencies.list) $<

.PHONY: clean
clean:
	rm -rf target

target:
	mkdir -p target
