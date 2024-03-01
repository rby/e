.PHONY: watchtest

watchtest::
	ls src/*.erl | entr -s 'rebar3 eunit'
