SOURCES=$(1) $(wildcard src/*.fs) $(wildcard src/wrap/*.fs)
TESTSOURCES=$(wildcard test/*.fs)
FABLE=fable --rollup --outDir out --ecma es5 --projFile $(1) && mv out/bundle.js $@

all: out/index.js out/trytrace.js test

tests: out/test.js

out/index.js: $(call SOURCES,src/index.fsx)
	$(call FABLE,$<)

out/trytrace.js: $(call SOURCES,src/trytrace.fsx)
	$(CALL FABLE,$<)

out/test.js: $(call SOURCES,test/test.fsx) $(TESTSOURCES)
	$(call FABLE,$<)

test: tests
	./test.sh

clean: 
	rm -rf out
