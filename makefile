lexer: 
	ocamllex src/lexer.mll

parser:
	menhir src/parser.mly

clean: 
	rm -rf _build