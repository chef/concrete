# concrete: use with rebar for a better building experience #


```
# This is a minimal 'Makefile' for use with concrete.

# define dialyzer deps here. then verify that it gets picked up by
# included rules.

# Do we have a cached copy of the rebar.mk rules file available?
concrete_rules_file = $(wildcard .concrete/rebar.mk)
ifeq ($(concrete_rules_file),.concrete/rebar.mk)
    include .concrete/rebar.mk
else
    all: concrete
endif

# bootstrap time!
# Make sure we get our deps, which should make concrete available as a
# rebar plugin (assumes correct rebar.config). Then call ourselves to
# build the project for reals.
concrete:
	rebar get-deps
	rebar init-makefile
	$(MAKE) all

clean_concrete:
	-rm -rf .concrete
```
