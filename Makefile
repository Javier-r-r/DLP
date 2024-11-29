
all: lambda prettyprinter execute parser lexer main 
	ocamlc -o top lambda.cmo prettyPrinter.cmo execute.cmo parser.cmo lexer.cmo main.cmo 

lambda: lambda.ml lambda.mli 
	ocamlc -c lambda.mli lambda.ml 

prettyprinter: prettyPrinter.ml 
	ocamlc -c prettyPrinter.ml

execute: execute.ml 
	ocamlc -c execute.ml

parser: parser.mly
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

main: main.ml
	ocamlc -c main.ml

clean:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~
