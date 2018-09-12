#!/bin/bash
#
# $1 is the name of the database.

set -eu

dropdb --if-exists $1
createdb $1
cd ..
psql -d $1 < init.sql
