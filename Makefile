#
# Targets
#

REBAR=`which rebar`

.PHONY: build

all: build

clean:
	rm -rf ebin log doc erl_crash.dump
	$(REBAR) clean

build: deps
	$(REBAR) compile
	$(REBAR) skip_deps=true xref

deps:
	$(REBAR) get-deps

test: build
	rm -rf .eunit
	$(REBAR) skip_deps=true eunit

boot:
	exec erl -pa ebin -sname semaphore -s semaphore

#
# Analysis
#

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

