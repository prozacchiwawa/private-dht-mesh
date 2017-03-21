SOURCES=$(1) $(wildcard src/*.fs) $(wildcard src/wrap/*.fs)
FABLE=fable --rollup --outDir out --ecma es5 --projFile $(1) && mv out/bundle.js $@

all: out/index.js tests

tests: out/test.js

out/index.js: $(call SOURCES,src/index.fsx)
	$(call FABLE,$<)

out/test.js: $(call SOURCES,test/test.fsx)
	$(call FABLE,$<)
