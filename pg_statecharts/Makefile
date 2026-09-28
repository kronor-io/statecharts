EXTENSION   = pg_statecharts
# The build order lives in sql/parts.txt so that install.sh, which has to work
# without make, reads the very same list.
SQL_PARTS   = $(shell cat sql/parts.txt)
EXTVERSION  = $(shell sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" $(EXTENSION).control)

# Upgrade and adoption scripts are handwritten rather than generated
UPGRADE_SCRIPTS = $(wildcard sql/upgrade/$(EXTENSION)--*.sql)

BUILT     = $(EXTENSION)--$(EXTVERSION).sql
DATA      = $(BUILT) $(notdir $(UPGRADE_SCRIPTS))
EXTRA_CLEAN = $(BUILT) $(notdir $(UPGRADE_SCRIPTS))

all: $(BUILT) $(notdir $(UPGRADE_SCRIPTS))

# printf rather than echo: sh's builtin echo eats the backslashes in \echo and \quit
$(BUILT): $(SQL_PARTS)
	@printf '%s\n' '\echo Use "CREATE EXTENSION $(EXTENSION)" to load this file. \quit' > $@
	@printf '%s\n' '-- $(EXTENSION) $(EXTVERSION)' >> $@
	@printf '%s\n' '-- GENERATED FILE, DO NOT EDIT. Concatenated from the files in sql/parts.txt.' >> $@
	@for f in $(SQL_PARTS); do \
		printf '\n-- %s\n' "$$f" >> $@; \
		cat $$f                  >> $@; \
	done

# PGXS wants DATA files next to the Makefile
$(notdir $(UPGRADE_SCRIPTS)): $(UPGRADE_SCRIPTS)
	cp sql/upgrade/$@ $@

# make installcheck  (needs a running server you can connect to)
REGRESS = semver runtime
REGRESS_OPTS = --inputdir=test --outputdir=test

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
