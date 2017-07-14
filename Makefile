all:
	jbuilder build @install @runtest @doc
	cp -r _build/default/_doc/* docs/
clean:
	rm -rf _build



