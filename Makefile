all:
	dune build @install @runtest @doc --profile release
	rm -r docs/* && cp -r _build/default/_doc/_html/* docs/
clean:
	rm -rf _build


.PHONY : coverage
coverage : clean
	BISECT_ENABLE=YES jbuilder runtest
	bisect-ppx-report -I _build/default/ -html _coverage/ `find . -name 'bisect*.out'`


.PHONY : pin
pin: 
	opam pin add bitcoinml . -n --working-dir && opam remove bitcoinml && opam install bitcoinml --working-dir
