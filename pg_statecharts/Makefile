# Meant to be run in a Postgres Docker container, currently only works on pg18
.PHONY: install
install:
	cargo install cargo-pgrx --locked
	cargo pgrx init --pg18 /usr/bin/pg_config
	cargo pgrx install --features pg18 --no-default-features --release --pg-config /usr/bin/pg_config
