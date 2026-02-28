#!/bin/bash

set -e

# this will install the world, go get a cup of tea
cargo install --locked cargo-pgrx
cargo pgrx init
