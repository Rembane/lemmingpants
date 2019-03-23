#!/usr/bin/dumb-init bash
#
# This script is run as the standard command when running the backend image.
#
# Inspiration: https://spin.atomicobject.com/2017/08/24/start-stop-bash-background-process/

set -eu

trap "exit" INT TERM ERR
trap "kill 0" EXIT

export PGRST_JWT_SECRET=`cat /usr/local/etc/secret_key.txt`
export PGRST_DB_URI="postgres://lemmingpants:lemmingpants@localhost/lemmingpants"
export PGRST_DB_POOL=10
export PGRST_DB_SCHEMA="api"
export PGRST_DB_ANON_ROLE="web_anon"
export PGRST_SERVER_HOST="*4"
export PGRST_SERVER_PORT=3000

/etc/init.d/postgresql start
postgrest /usr/local/etc/lemmingpants.conf &

export PGWS_DB_URI=$PGRST_DB_URI
export PGWS_PORT=8000
export PGWS_ROOT_PATH="/srv/static"
export PGWS_LISTEN_CHANNEL="postgres-websockets-listener"
export PGWS_JWT_SECRET=$PGRST_JWT_SECRET
postgres-websockets &

wait
