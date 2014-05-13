# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

# If building on travis, use the rebar in the current directory
ifeq ($(TRAVIS),true)
REBAR = $(CURDIR)/rebar
endif

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar),rebar)
REBAR = $(CURDIR)/rebar
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar
endif

# For use on Travis CI, skip dialyzer for R14 and R15. Newer versions
# have a faster dialyzer that is less likely to cause a build timeout.
DIALYZER = dialyzer
R14 = $(findstring R14,$(TRAVIS_OTP_RELEASE))
R15 = $(findstring R15,$(TRAVIS_OTP_RELEASE))
ifneq ($(R14),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(R15),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(SKIP_DIALYZER),)
DIALYZER = echo "SKIPPING dialyzer"
endif

REBAR_URL=https://github.com/rebar/rebar/wiki/rebar

DEPS ?= $(CURDIR)/deps

DIALYZER_OPTS ?=

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
                       inets \
                       kernel \
                       mnesia \
                       public_key \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
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
	@$(DIALYZER) $(DIALYZER_OPTS) -r ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@$(DIALYZER) --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS)
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

devrel: rel
devrel: lib_dir=$(wildcard $(RELX_OUTPUT_DIR)/lib/delivery-* )
devrel:
	@/bin/echo -n Symlinking deps and apps into release
	@rm -rf $(lib_dir); mkdir -p $(lib_dir)
	@ln -sf `pwd`/ebin $(lib_dir)
	@ln -sf `pwd`/priv $(lib_dir)
	@ln -sf `pwd`/src $(lib_dir)

relclean:
	rm -rf $(RELX_OUTPUT_DIR)


.PHONY: all all_but_dialyzer compile eunit test dialyzer clean allclean relclean distclean doc tags get-rebar rel devrel
