.PHONY: run test setup

test: 
	stack test

test-trace:
	stack test --trace	

run:
	stack build
	stack exec haslapse-exe

setup:
	stack setup
