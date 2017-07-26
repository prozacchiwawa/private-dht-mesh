SOURCES=$(wildcard src/*.fs) $(wildcard src/wrap/*.fs)
TESTSOURCES=$(wildcard test/*.fs)
FABLE=fable --rollup --outDir out --ecma es5 --projFile $(1) && mv out/bundle.js $@

all: out/index.js out/trytrace.js test

out/index.js: src/index.fsx $(SOURCES)
	$(CALL FABLE,src/index.fsx)

out/trytrace.js: src/trytrace.fsx $(SOURCES)
	$(call FABLE,src/trytrace.fsx)

tests: out/test.js

out/test.js: $(call SOURCES,test/test.fsx) $(TESTSOURCES)
	$(call FABLE,test/test.fsx)

test: tests
	./test.sh

clean: 
	rm -rf out
