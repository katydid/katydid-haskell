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
	stack haddock --haddock-arguments "--odir=./docs"

lint:
	# -XNoPatternSynonyms is a temporary workaround for https://github.com/ndmitchell/hlint/issues/216
	hlint -XNoPatternSynonyms .

pkg: doc
	stack sdist
	echo "Now upload the created file to: https://hackage.haskell.org/upload"
