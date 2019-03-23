#!/usr/bin/env bash
#
# Build all Docker images.
# With some luck you don't have to do this, `docker pull` should get
# you all the images you need.

set -eu

PATHS=("frontend" "lemmingpants.conf" "init.sql" "sql" "pg_tests" "static")

clean_up() {
    for p in ${PATHS[@]}; do
        rm -rf "$p"
    done
}

# Always run the clean-up-code.
trap "clean_up" ERR

for p in ${PATHS[@]}; do
    cp -r "../$p" .
done

docker build --tag="lemmingpants/backend-full:latest" .
clean_up
