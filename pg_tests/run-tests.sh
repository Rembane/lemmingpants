#!/bin/bash

set -eu

DBNAME="lp_tests"

dropdb --if-exists $DBNAME
createdb $DBNAME

# Prepare by creating the extensions we need.
psql -d $DBNAME <<< 'CREATE EXTENSION IF NOT EXISTS pgcrypto; CREATE EXTENSION IF NOT EXISTS pgjwt; CREATE EXTENSION IF NOT EXISTS pgtap;'
(
    cd ..
    psql -d $DBNAME < init.sql
)

# Run the tests.
pg_prove -d $DBNAME --pset tuples_only=1 sql/*.sql -v
