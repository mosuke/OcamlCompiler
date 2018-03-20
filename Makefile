OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=menhir
OCAMLLEX=ocamllex
INCLUDES= # all relevant -I options here
OCAMLFLAGS=$(INCLUDES) -annot -bin-annot
OCAMLOPTFLAGS=$(INCLUDES) -annot -bin-annot

PROGNAME=minimlc

# The list of object files
OBJS=misc.cmo mySet.cmo myMap.cmo pretty.cmo \
	syntax.cmo parser.cmo lexer.cmo \
	environment.cmo normal.cmo closure.cmo \
	flat.cmo vm.cmo arm_spec.cmo arm_noreg.cmo \
	cfg.cmo dfa.cmo live.cmo reg.cmo \
	opt.cmo arm_reg.cmo arm_simulator.cmo main.cmo

DEPEND += lexer.ml parser.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLC) -o $(PROGNAME) $(OCAMLFLAGS) $(OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

parser.ml parser.mli: parser.mly
	@rm -f $@
	$(OCAMLYACC) -v $<
	@chmod -w $@

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f parser.ml parser.mli parser.automaton parser.conflicts
	rm -f lexer.ml
	rm -f *.cm[ioxt] *.cmti *.o *.annot *~

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend
