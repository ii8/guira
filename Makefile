
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

OCAMLC = ocamlfind ocamlc -safe-string -w +A-4
OCAMLOPT = ocamlfind ocamlopt -safe-string -w +A-4
JS_OF_OCAML = js_of_ocaml
CC = cc -Wall

OBJS = sexp.cmx time.cmx syntax.cmx filter.cmx


guira: $(OBJS) main.ml
	$(OCAMLOPT) -linkpkg -package unix $^ -o $@

install:
	if test -e guira; then install guira $(BINDIR)/guira; fi
	if test -e mysql-guira.so; then\
		cp mysql-guira.so `mysql_config --plugindir`/guira.so; fi

uninstall:
	rm -f $(BINDIR)/guira
	rm -f `mysql_config --plugindir`/guira.so

js: js.byte
	$(JS_OF_OCAML) $^ -o guira.js

mysql: mysql.o mysql-guira.o
	$(CC) -shared -o mysql-guira.so $^\
		-L`ocamlc -where` -lthreadsnat -lthreads -lunix -lasmrun

test: guira test.byte
	./test.byte
	./test.scm ./guira

clean:
	rm -f *.cm[iox] *.o
	rm -f guira js.byte test.byte guira.js mysql-guira.so


js.byte: $(OBJS:cmx=cmo) javascript.ml
	$(OCAMLC) -linkpkg -package js_of_ocaml,unix $^ -o $@

mysql-guira.o: mysql-guira.c
	ocamlopt -c -ccopt `mysql_config --include` $^

mysql.o: $(OBJS) mysql.ml
	$(OCAMLOPT) -linkpkg -package unix -output-obj $^ -o $@

test.byte: $(OBJS:cmx=cmo) test.ml
	$(OCAMLC) -linkpkg -package unix $^ -o $@

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: $(OBJS:cmx=mli)
	$(OCAMLC) -c $(@:cmi=mli)

.PRECIOUS: $(OBJS:cmx=cmi)
.PHONY: install uninstall js mysql test clean
