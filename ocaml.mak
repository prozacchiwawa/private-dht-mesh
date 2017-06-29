INCS=-I src/wrap -I src/elm-return -I src

.SUFFIXES: .mli .cmi .ml .cmo

%.mli: %.ml
	ocamlc $(INCS) -i $< > $@

%.cmi: %.mli
	ocamlc $(INCS) -c unix.cma str.cma $<

%.cmo: %.ml %.cmi
	ocamlc $(INCS) -c unix.cma str.cma $<

SRCS=\
	src/wrap/Util.ml \
	src/elm-return/Respond.ml \
	src/elm-return/Return.ml \
	src/Ports.ml \
	src/KBucket.ml \
	src/DHTData.ml \
	src/DHT.ml

OBJS=$(subst .ml,.cmi,$(SRCS))
INTF=$(subst .ml,.cmo,$(SRCS))

all: runme

runme: $(INTF) $(OBJS)
	:
