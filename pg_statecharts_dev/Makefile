EXTENSION   = pg_statecharts_dev
# The build order lives in sql/parts.txt so that install.sh, which has to work
# without make, reads the very same list.
SQL_PARTS   = $(shell cat sql/parts.txt)
EXTVERSION  = $(shell sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" $(EXTENSION).control)

BUILT       = $(EXTENSION)--$(EXTVERSION).sql
DATA        = $(BUILT)
EXTRA_CLEAN = $(BUILT) $(DISTDIR) $(DIST)

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

# ------------------------------------------------------------------ PGXN ---
# A PGXN distribution is a zip holding a single directory named
# <distribution>-<version>, with META.json at its root. Everything that goes
# into it is committed except the assembled $(BUILT), which is built here and
# copied in so that the file META.json names under "provides" is really there.
DISTDIR = $(EXTENSION)-$(EXTVERSION)
DIST    = $(DISTDIR).zip

.PHONY: dist
dist: $(DIST)

$(DIST): all META.json
# The control file is the one place the version is written by hand, so refuse
# to build an archive named after a version META.json disagrees with. "1.0.0"
# is the meta-spec's own version, not ours.
	@stale=$$(grep -oE '"version" *: *"[^"]*"' META.json \
		| sed 's/.*"\([^"]*\)"$$/\1/' \
		| grep -vxF '$(EXTVERSION)' | grep -vxF '1.0.0' | sort -u | paste -sd' ' -); \
	if [ -n "$$stale" ]; then \
		echo "META.json is at $$stale but $(EXTENSION).control says $(EXTVERSION)" >&2; \
		exit 1; \
	fi
	rm -rf $(DISTDIR) $@
	mkdir -p $(DISTDIR)
	cp -r META.json $(EXTENSION).control $(BUILT) Makefile install.sh README.md sql test $(DISTDIR)/
	cp ../LICENSE $(DISTDIR)/
	rm -rf $(DISTDIR)/test/results
# Whatever META.json points at has to be in the archive, or PGXN indexes a
# distribution whose extension or documentation is missing.
	@for f in $$(grep -oE '"(doc)?file" *: *"[^"]*"' META.json | sed 's/.*: *"//; s/"$$//'); do \
		test -f "$(DISTDIR)/$$f" || { \
			echo "META.json names $$f, which is not in the distribution" >&2; exit 1; }; \
	done
	zip -qr $@ $(DISTDIR)
	rm -rf $(DISTDIR)
	@echo "$@"

# make installcheck  (needs a running server you can connect to)
REGRESS = parse codegen files
REGRESS_OPTS = --inputdir=test --outputdir=test

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
