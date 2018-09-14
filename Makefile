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

fmt:
	brittany --write-mode=inplace *.hs
	brittany --write-mode=inplace **/*.hs
	brittany --write-mode=inplace ./src/Data/Katydid/**/*.hs
	brittany --write-mode=inplace ./src/Data/Katydid/Relapse/**/*.hs

setup:
	stack setup
	stack install brittany

doc:
	stack haddock --haddock-arguments "--odir=./docs"

lint:
	# -XNoPatternSynonyms is a temporary workaround for https://github.com/ndmitchell/hlint/issues/216
	hlint -XNoPatternSynonyms .

pkg: doc
	stack sdist
	echo "Now upload the created file to: https://hackage.haskell.org/upload"
