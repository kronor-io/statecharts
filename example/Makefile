.PHONY: access-database
access-database:
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres

.PHONY: deploy-migrations
deploy-migrations:
	docker compose exec db sqitch --chdir sqitch deploy --verify

define GEN_CHARTS_QUERY
	select fsm.gen_statechart_sqitch_migrations(
		source_path => '/repo/statecharts',
		sqitch_plan_file_path => '/repo/sqitch/sqitch.plan',
		file_permission_666 => true
	);
endef

# The generated files will be owned by the docker user, we need to chmod them
# so that they can read by us
export GEN_CHARTS_QUERY
.PHONY: gen-charts
gen-charts:
	sudo chmod -R 666 sqitch/sqitch.plan
	docker compose exec db psql postgres://postgres:postgres@localhost:5432/postgres -c "$$GEN_CHARTS_QUERY"

