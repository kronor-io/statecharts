#!/bin/bash
set -e

pgxn load -d statecharts semver
psql -c 'create extension pgtap'
psql -c 'create extension if not exists plpgsql_check'

sqitch --verbose deploy
