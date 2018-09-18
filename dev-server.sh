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
export PGRST_WS_ROOT="./static"
export PGRST_WS_LISTEN="postgres-websockets-listener"

postgrest lemmingpants.conf &

{
    export PGRST_SERVER_PORT=8000
    postgres-websockets lemmingpants.conf &
}

wait
