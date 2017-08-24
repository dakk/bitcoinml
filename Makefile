all:
	jbuilder build @install @runtest @doc
	cp -r _build/default/_doc/* docs/
clean:
	rm -rf _build

#rm -f `find . -name 'bisect*.out'`

.PHONY : coverage
coverage : clean
	BISECT_ENABLE=YES jbuilder runtest
	bisect-ppx-report -I _build/default/ -html _coverage/ `find . -name 'bisect*.out'`

