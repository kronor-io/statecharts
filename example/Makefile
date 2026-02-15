.PHONY: access-database
access-database:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir sqitch deploy --verify

# The file_permission_666 flag gives the created files the rw-rw-rw-
# permissions. This is useful because the docker user will be the owner of the
# files since the database is running inside docker. By default you won't be
# allowed to write to the generated files.
#
# We need to chmod 666 the sqitch.plan for the same reason so that the docker
# user can write to the file.

define GEN_CHARTS_QUERY
	select fsm.gen_statechart_sqitch_migrations(
		source_path => '/repo/statecharts',
		sqitch_plan_file_path => '/repo/sqitch/sqitch.plan',
		recursive => false,
		file_permission_666 => true
	);
endef

export GEN_CHARTS_QUERY
.PHONY: gen-charts
gen-charts:
	sudo chmod 666 sqitch/sqitch.plan
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres -c "$$GEN_CHARTS_QUERY"

define IMPORT_CHARTS_QUERY
	select * from fsm.import_scxml_files(
		source_path => '/repo/statecharts',
		recursive => false,
		on_conflict_do_nothing => false
	);
endef

export IMPORT_CHARTS_QUERY
.PHONY: import-charts
import-charts:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres -c "$$IMPORT_CHARTS_QUERY"

