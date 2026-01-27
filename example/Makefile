.PHONY: access-database
access-database:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir sqitch deploy

define GEN_CHARTS_QUERY
	select fsm.gen_statechart_sqitch_migrations(
		source_path => '/repo/statecharts',
		sqitch_plan_file_path => '/repo/sqitch/sqitch.plan'
	);
endef

# The generated files will be owned by the postgres user, we need to chmod
# them so that they can edited by you
export GEN_CHARTS_QUERY
.PHONY: gen-charts
gen-charts:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres -c "$$GEN_CHARTS_QUERY"
	sudo chmod -R 777 sqitch/**/statechart

