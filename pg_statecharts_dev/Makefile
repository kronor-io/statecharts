EXTENSION   = pg_statecharts_dev
# The build order lives in sql/parts.txt so that install.sh, which has to work
# without make, reads the very same list.
SQL_PARTS   = $(shell cat sql/parts.txt)
EXTVERSION  = $(shell sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" $(EXTENSION).control)

BUILT       = $(EXTENSION)--$(EXTVERSION).sql
DATA        = $(BUILT)
EXTRA_CLEAN = $(BUILT)

all: $(BUILT)

# printf rather than echo: sh's builtin echo eats the backslashes in \echo and \quit
$(BUILT): $(SQL_PARTS)
	@printf '%s\n' '\echo Use "CREATE EXTENSION $(EXTENSION)" to load this file. \quit' > $@
	@printf '%s\n' '-- $(EXTENSION) $(EXTVERSION)' >> $@
	@printf '%s\n' '-- GENERATED FILE, DO NOT EDIT. Concatenated from the files in sql/parts.txt.' >> $@
	@for f in $(SQL_PARTS); do \
		printf '\n-- %s\n' "$$f" >> $@; \
		cat $$f                  >> $@; \
	done

# make installcheck  (needs a running server you can connect to)
REGRESS = parse codegen files
REGRESS_OPTS = --inputdir=test --outputdir=test

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
