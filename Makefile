# This Makefile written by concrete
#
# {concrete_makefile_version, 1}
#
# Use this to override concrete's default dialyzer options of
# -Wunderspecs
# DIALYZER_OPTS = ...

# List dependencies that you do NOT want to be included in the
# dialyzer PLT for the project here.  Typically, you would list a
# dependency here if it isn't spec'd well and doesn't play nice with
# dialyzer or otherwise mucks things up.
#
# DIALYZER_SKIP_DEPS = bad_dep_1 \
#                      bad_dep_2

# Use this if you want to wire a dependency into the default "all"
# target provided by concrete
ALL_HOOK = concrete

# Do we have a cached copy of the rebar.mk rules file available?
concrete_rules_file = $(wildcard .concrete/rebar.mk)
ifeq ($(concrete_rules_file),.concrete/rebar.mk)
    include .concrete/rebar.mk
else
    all:
	@echo "ERROR: missing .concrete/rebar.mk"
	@echo "  run: concrete update"
endif

concrete: escript

escript:
	rebar escriptize
