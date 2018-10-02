.PHONY: run test setup bench doc pkg

build:
	stack build --bench --no-run-benchmarks

test: build
	stack test

singletest: build
	stack test --ta '-p "Data.Katydid.Parser.Protobuf.Protobuf"'

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

regenerate:
	# If this does not work, then try stack install proto-lens-setup
	stack install proto-lens-protoc
	# If you don't have protoc then install it https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md
	(cd src/Data/Katydid/Parser/Protobuf/Testdata && protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` --haskell_out=. *.proto)
	(cd src/Data/Katydid/Parser/Protobuf/Testdata \
		&& sed -e 's/module Proto.Phone/module Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone/g' ./Proto/Phone_Fields.hs > ./Proto/Phone_Fields.hs.bak \
		; mv ./Proto/Phone_Fields.hs.bak ./Proto/Phone_Fields.hs \
		\
		; sed -e 's/module Proto.Phone/module Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone/g' ./Proto/Phone.hs > ./Proto/Phone.hs.bak \
		; mv ./Proto/Phone.hs.bak ./Proto/Phone.hs \
		\
		; sed -e 's/module Proto.Person/module Data.Katydid.Parser.Protobuf.Testdata.Proto.Person/g' ./Proto/Person_Fields.hs > ./Proto/Person_Fields.hs.bak \
		; mv ./Proto/Person_Fields.hs.bak ./Proto/Person_Fields.hs \
		\
		; sed -e 's/import qualified Proto.Phone/import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone as Proto.Phone/g' ./Proto/Person_Fields.hs > ./Proto/Person_Fields.hs.bak \
		; mv ./Proto/Person_Fields.hs.bak ./Proto/Person_Fields.hs \
		\
		; sed -e 's/module Proto.Person/module Data.Katydid.Parser.Protobuf.Testdata.Proto.Person/g' ./Proto/Person.hs > ./Proto/Person.hs.bak \
		; mv ./Proto/Person.hs.bak ./Proto/Person.hs \
		\
		; sed -e 's/import qualified Proto.Phone/import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone as Proto.Phone/g' ./Proto/Person.hs > ./Proto/Person.hs.bak \
		; mv ./Proto/Person.hs.bak ./Proto/Person.hs)
		

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
