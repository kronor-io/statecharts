#! /usr/bin/env bash

set -e

TIMEOUT=60
wait_for_port() {
    local PORT=$1
    for i in `seq 1 $TIMEOUT`;
    do
        nc -z localhost $PORT 2>&1 && return
        sleep 1
    done
    echo "failed waiting for $PORT"
}

wait_for_port $@

