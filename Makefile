
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

OCAMLC = ocamlfind ocamlc -safe-string -w +A-4
OCAMLOPT = ocamlfind ocamlopt -safe-string -w +A-4
JS_OF_OCAML = js_of_ocaml

OBJS = sexp.cmx time.cmx syntax.cmx filter.cmx


guira: $(OBJS) main.ml
	$(OCAMLOPT) -linkpkg -package unix $^ -o $@

install: guira
	install guira $(BINDIR)/guira

uninstall:
	rm -f $(BINDIR)/guira

js: js.byte
	$(JS_OF_OCAML) $^ -o guira.js

test: guira test.byte
	./test.byte
	./test.scm ./guira

clean:
	rm -f *.cm[iox] *.o
	rm -f guira js.byte test.byte guira.js


js.byte: $(OBJS:cmx=cmo) javascript.ml
	$(OCAMLC) -linkpkg -package js_of_ocaml,unix $^ -o $@

test.byte: $(OBJS:cmx=cmo)
	$(OCAMLC) -linkpkg -package unix $^ test.ml -o $@

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

.PRECIOUS: $(OBJS:cmx=cmi)
.PHONY: clean install uninstall test
