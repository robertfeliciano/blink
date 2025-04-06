frontend:
	dune build
	@dune exec ./_build/default/src/main.exe

clean: 
	rm -rf _build