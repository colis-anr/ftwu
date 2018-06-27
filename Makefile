.PHONY: build doc install uninstall test clean

build:
	jbuilder build @install
	ln -s _build/install/default/bin bin
	ln -s _build/install/default/lib lib

doc:
	jbuilder build @doc
	ln -s _build/default/_doc/_html doc

install:
	jbuilder install

uninstall:
	jbuilder uninstall

test:
	jbuilder runtest

clean:
	jbuilder clean
	rm -f bin lib doc
