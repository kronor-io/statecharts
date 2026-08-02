SQITCH_DIR := sqitch

# docker compose does not expand $(id -u) itself, so the values go in .env.
# The database container runs as you rather than as its own postgres user, so
# that the migration files it generates end up owned by you rather than by
# uid 999. That is what replaced the old chmod 666 dance.
.env:
	printf 'UID=%s\nGID=%s\n' "$$(id -u)" "$$(id -g)" > $@

# SQL cannot create directories, so the ones the generated migrations go into
# have to exist up front. sqitch init does not make the statechart/ subfolders.
.PHONY: setup
setup: .env
	mkdir -p $(SQITCH_DIR)/deploy/statechart \
	         $(SQITCH_DIR)/revert/statechart \
	         $(SQITCH_DIR)/verify/statechart

.PHONY: up
up: setup
	docker compose up -d db

.PHONY: down
down:
	docker compose down

.PHONY: access-database
access-database:
	docker compose exec db psql

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir sqitch deploy --verify

# pg_statecharts_dev is deliberately not created by any migration: it can read
# and write files on the database host and has no business being in
# production. It gets created here, by hand, on development machines only.
.PHONY: dev-extension
dev-extension:
	docker compose exec db psql -c "create extension if not exists pg_statecharts_dev cascade"

# Generates a sqitch migration per .scxml file. The paths are resolved by the
# database server, so they are paths inside the container.
define GEN_CHARTS_QUERY
	select fsm.gen_statechart_sqitch_migrations(
		source_path => '/repo/statecharts',
		sqitch_plan_file_path => '/repo/sqitch/sqitch.plan',
		recursive => false
	);
endef

export GEN_CHARTS_QUERY
.PHONY: gen-charts
gen-charts: setup dev-extension
	docker compose exec db psql -c "$$GEN_CHARTS_QUERY"

# Loads the .scxml files straight into the database without going through
# sqitch. Handy while iterating on a chart, but nothing records that it
# happened, so use gen-charts for anything you want to keep.
define IMPORT_CHARTS_QUERY
	select * from fsm.import_scxml_files(
		source_path => '/repo/statecharts',
		recursive => false,
		on_conflict_do_nothing => false
	);
endef

export IMPORT_CHARTS_QUERY
.PHONY: import-charts
import-charts: dev-extension
	docker compose exec db psql -c "$$IMPORT_CHARTS_QUERY"
