#!/bin/bash
# set -eu
#
# $1 is the name of the database.

dropdb $1
createdb $1
cd ..
psql -d $1 < init.sql
