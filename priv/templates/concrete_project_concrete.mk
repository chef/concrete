# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

# If building on travis, use the rebar in the current directory
ifeq ($(TRAVIS),true)
REBAR=$(CURDIR)/rebar
endif

ifeq ($(REBAR),)
REBAR=$(CURDIR)/rebar
endif

# =============================================================================
# Handle version discovery
# =============================================================================

# We have a problem that we only have 10 minutes to build on travis
# and those travis boxes are quite small. This is ok for the fast
# dialyzer on R15 and above. However on R14 and below we have the
# problem that travis times out. The code below lets us not run
# dialyzer on R14
OTP_VSN=$(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
TRAVIS_SLOW=$(shell expr $(OTP_VSN) \<= 15 )

ifeq ($(TRAVIS_SLOW), 0)
DIALYZER=$(shell which dialyzer)
else
DIALYZER=: not running dialyzer on R14 or R15
endif

REBAR_URL=https://github.com/rebar/rebar/wiki/rebar

DEPS ?= $(CURDIR)/deps

DIALYZER_OPTS ?= -Wunderspecs

# Find all the deps the project has by searching the deps dir
ALL_DEPS = $(notdir $(wildcard deps/*))
# Create a list of deps that should be used by dialyzer by doing a
# complement on the sets
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS), $(ALL_DEPS))
# Create the path structure from the dep names
# so dialyzer can find the .beam files in the ebin dir
# This list is then used by dialyzer in creating the local PLT
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),deps/$(dep)/ebin)

DEPS_PLT = deps.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       erts \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
                       webtool \
                       xmerl

all: all_but_dialyzer dialyzer

all_but_dialyzer: .concrete/DEV_MODE compile eunit $(ALL_HOOK)

$(REBAR):
	curl -Lo rebar $(REBAR_URL) || wget $(REBAR_URL)
	chmod a+x rebar

get-rebar: $(REBAR)

.concrete/DEV_MODE:
	@mkdir -p .concrete
	@touch $@

# Clean ebin and .eunit of this project
clean:
	@$(REBAR) clean skip_deps=true

# Clean this project and all deps
allclean:
	@$(REBAR) clean

compile: $(DEPS)
	@$(REBAR) compile

$(DEPS):
	@$(REBAR) get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@$(REBAR) clean

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@$(REBAR) doc skip_deps=true

shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

tags:
	find src deps -name "*.[he]rl" -print | etags -

# Releases via relx. we will install a local relx, as we do for rebar,
# if we don't find one on PATH.
RELX_CONFIG ?= $(CURDIR)/relx.config
RELX = $(shell which relx)
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel
ifeq ($(RELX),)
RELX = $(CURDIR)/relx
endif
RELX_URL = https://github.com/erlware/relx/releases/download/v0.6.0/relx

$(RELX):
	curl -Lo relx $(RELX_URL) || wget $(RELX_URL)
	chmod a+x relx

rel: relclean all_but_dialyzer $(RELX)
	@$(RELX) -c $(RELX_CONFIG) -o $(RELX_OUTPUT_DIR) $(RELX_OPTS)

relclean:
	rm -rf $(RELX_OUTPUT_DIR)

.PHONY: all all_but_dialyzer compile eunit test dialyzer clean allclean relclean distclean doc tags get-rebar
