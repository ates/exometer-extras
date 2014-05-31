REBAR = $(shell which rebar 2>/dev/null || echo $(PWD)/rebar)

.PHONY: deps

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
