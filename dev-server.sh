#!/bin/bash
# Inspiration: https://spin.atomicobject.com/2017/08/24/start-stop-bash-background-process/

set -e

trap "exit" INT TERM ERR
trap "kill 0" EXIT

postgrest lemmingpants.conf &
postgres-websockets websockets.conf &

wait
