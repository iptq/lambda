.PHONY: all clean

types.cmo: types.ml
	ocamlc -o $@ -c $^

parser: parser.mly
	ocamlyacc $^

parser.cmo: parser	
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer.cmo: lexer.ml
	ocamlc -o $@ -c $^

lexer.ml: parser.cmo
	ocamllex lexer.mll

lambda.cmo: lambda.ml
	ocamlc -o $@ -c $^

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.cmo *.cmi

all: types.cmo lexer.cmo parser.cmo lambda.cmo
	ocaml types.cmo parser.cmo lexer.cmo lambda.cmo

