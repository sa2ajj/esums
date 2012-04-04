.PHONY: deps

all: compile test

compile: deps
	rebar compile

test:
	rebar eunit ct

deps:
	rebar get-deps
