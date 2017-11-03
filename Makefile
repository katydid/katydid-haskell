.PHONY: run test setup

test: 
	stack test

test-trace:
	stack test --trace	

run:
	stack build
	stack exec katydid-exe

setup:
	stack setup

ide-setup:
	stack build intero

install-haddock:
	stack install haddock-2.17.2

doc:
	stack haddock
	rm -rf docs || true
	mkdir docs
	cp -R .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/doc/html/katydid/* ./docs/

lint:
	# -XNoPatternSynonyms is a temporary workaround for https://github.com/ndmitchell/hlint/issues/216
	hlint -XNoPatternSynonyms .
