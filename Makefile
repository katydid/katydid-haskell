.PHONY: run test setup

test: 
	stack test

run:
	stack build
	stack exec haslapse-exe

setup:
	stack setup
