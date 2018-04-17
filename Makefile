.PHONY: run test setup bench doc pkg

test: 
	stack test

test-trace:
	stack test --trace

bench:
	stack bench

run:
	stack build
	stack exec katydid-exe

setup:
	stack setup

ide-setup:
	stack build intero

doc:
	stack haddock
	rm -rf docs || true
	mkdir docs
	cp -R .stack-work/dist/x86_64-osx/Cabal-2.0.0.2/doc/html/katydid/* ./docs/

lint:
	# -XNoPatternSynonyms is a temporary workaround for https://github.com/ndmitchell/hlint/issues/216
	hlint -XNoPatternSynonyms .

pkg: doc
	stack sdist
	echo "Now upload the created file to: https://hackage.haskell.org/upload"
