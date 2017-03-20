SOURCES=$(1) $(wildcard src/*.fs) $(wildcard src/wrap/*.fs)
FABLE=fable --rollup --outDir out --ecma es5 --projFile $(1) && mv out/bundle.js $@

all: out/index.js

out/index.js: $(call SOURCES,src/index.fsx)
	$(call FABLE,$<)
