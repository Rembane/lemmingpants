#!/bin/bash
# Inspiration: https://spin.atomicobject.com/2017/08/24/start-stop-bash-background-process/

set -eu

trap "exit" INT TERM ERR
trap "kill 0" EXIT

export PGRST_DB_URI="postgres://lemmingpants:lemmingpants@localhost/lemmingpants"
export PGRST_DB_SCHEMA="api"
export PGRST_DB_ANON_ROLE="web_anon"
export PGRST_DB_POOL=10
export PGRST_SERVER_HOST="*4"
export PGRST_SERVER_PORT=3000
export PGRST_JWT_SECRET="feBU1ykZ4icKs2nKam9l8CD84qhgeOl6QQakrUJBiRTUu4dKTLVoH8o"

export PGWS_DB_URI=$PGRST_DB_URI
export PGWS_HOST=127.0.0.1
export PGWS_PORT=8000
export PGWS_ROOT_PATH="./static"
export PGWS_LISTEN_CHANNEL="postgres-websockets-listener"
export PGWS_JWT_SECRET=$PGRST_JWT_SECRET

postgrest lemmingpants.conf &
postgres-websockets lemmingpants.conf &

wait
