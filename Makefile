.PHONY: build doc install uninstall test clean

build:
	dune build @install
	ln -sf _build/install/default/bin .
	ln -sf _build/install/default/lib .

doc:
	dune build @doc
	ln -s _build/default/_doc/_html doc

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean
	rm -f bin lib doc
