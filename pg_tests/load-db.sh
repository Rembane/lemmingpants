#!/bin/bash
#
# $1 is the name of the database.

set -eu

dropdb --if-exists $1
createdb $1
cd ..
# Prepare by creating the extensions we need.
psql -d $1 <<< 'CREATE EXTENSION IF NOT EXISTS pgcrypto; CREATE EXTENSION IF NOT EXISTS pgjwt; CREATE EXTENSION IF NOT EXISTS pgtap;'
psql -d $1 < init.sql
