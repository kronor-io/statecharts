#!/bin/bash
set -e

pgxn load -d statecharts semver
psql -c 'create extension pgtap'

sqitch --verbose deploy
