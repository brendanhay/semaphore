REBAR=`which rebar`

.PHONY: all deps build doc test

all: build test

build:
	$(REBAR) get-deps compile
	$(MAKE) xref

console: build
	exec erl -pa ../semaphore/ebin -sname semaphore_dev -s semaphore

doc: build
	$(REBAR) skip_deps=true doc

test: build
	rm -rf .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean

PLT=./plt/R14B04.plt

WARNINGS=-Werror_handling \
  -Wrace_conditions \
  -Wunderspecs \
  -Wunmatched_returns

APPS=kernel stdlib sasl erts ssl \
  tools os_mon runtime_tools crypto \
  inets xmerl webtool snmp public_key \
  mnesia eunit syntax_tools compiler

build-plt: deps build
	dialyzer --build_plt --output_plt $(PLT) \
	  --apps $(APPS) $(DEPS)

dialyzer: build
	dialyzer ebin --plt $(PLT) $(WARNINGS)

xref:
	$(REBAR) skip_deps=true xref



