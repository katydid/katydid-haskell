.PHONY: run test setup bench doc pkg

build:
	stack build --bench --no-run-benchmarks

test: build
	stack test

singletest: build
	stack test --ta '-p "Derive"'

test-trace: build
	stack test --trace

bench:
	stack bench

run: build
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
