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
