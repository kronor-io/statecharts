#!/bin/bash

for cmd in sed pg_config; do
    if ! command -v $cmd &> /dev/null; then
        echo "Error: $cmd is not installed or not in PATH"
        exit 1
    fi
done

PG_VERSION=$(pg_config --version | sed -n 's/^PostgreSQL \([0-9]*\).*/\1/p')
PG_CONFIG_PATH=$(which pg_config)

# Replacing the postgresql version in Cargo.toml feels hacky but I can't get it
# to work any other way. Cargo.toml needs to have the correct PostgreSQL
# version listed.
sed -i "s/pg18/pg$PG_VERSION/g" Cargo.toml

cargo install cargo-pgrx --locked
cargo pgrx init --pg$PG_VERSION $PG_CONFIG_PATH
cargo pgrx install --features pg$PG_VERSION --no-default-features --release --pg-config $PG_CONFIG_PATH
