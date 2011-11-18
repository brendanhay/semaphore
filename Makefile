REBAR=`which rebar`

all:
	@$(REBAR) get-deps compile

run: all
	exec erl -pa ../semaphore/ebin -sname semaphore_dev -s semaphore

edoc: all
	@$(REBAR) doc

test: all
	@rm -rf .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

