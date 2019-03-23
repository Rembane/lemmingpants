#!/usr/bin/env sh
#
# The installation script for the backend part of Lemmingpants.
# This script creates an image if it has to and then installs
# all the needed parts to it. Tests will also be run. The result
# should be a working and running Docker container.
#
# This script should be idempotent, if it isn't please file an issue.

set -eu

SETUP_PATH="/setup"

# Make sure the installation doesn't ask us questions we can't answer.
export DEBIAN_FRONTEND=noninteractive

cd $SETUP_PATH

echo "Create the lemmingpants user."
if ! grep lemmingpants /etc/passwd; then
    useradd -mU lemmingpants
fi

# Set selections.
echo locales locales/default_environment_locale select sv_SE.UTF-8 | debconf-set-selections
echo locales locales/locales_to_be_generated select "sv_SE.UTF-8 UTF-8" | debconf-set-selections

echo "Install Ubuntu packages."
apt-get -y update
apt-get -y install curl dumb-init jq locales make libpq-dev pgtap postgresql postgresql-server-dev-10 sudo

echo "Provision pgjwt."
if find -name 'pgjwt' | wc -l; then
    curl -sSL https://github.com/michelp/pgjwt/archive/master.tar.gz | tar -zx
    cd pgjwt-master/
    make install
fi

echo "Provision Postgrest."
if [ ! -e /usr/local/bin/postgrest ]; then
    cd $SETUP_PATH
    curl -sSL $(curl -sS https://api.github.com/repos/postgrest/postgrest/releases/latest | jq -Mrc '.assets | map(select(.name | contains("ubuntu.")) | .browser_download_url)[0]') | tar -Jx
    mv postgrest /usr/local/bin
fi

echo "Provision Postgres-websockets."
if [ ! -e /usr/local/bin/postgres-websockets ]; then
    cd $SETUP_PATH
    for url in $(curl -sS https://api.github.com/repos/diogob/postgres-websockets/releases/latest | jq -Mrc '.assets | map(.browser_download_url) | join ("\n")'); do
        curl -sSLO $url &
    done
    wait
    # sha256sum postgres-websockets.sha256
    chmod +x postgres-websockets
    mv postgres-websockets /usr/local/bin/postgres-websockets
fi

echo "Starting Postgresql."
/etc/init.d/postgresql start

echo "Preparing database... creating Lemmingpants user"

# The lemmingpants user has a really unsecure password and is
# allowed to create databases.
# TODO: Generate a random password for some security.
sudo -u postgres createuser --createdb lemmingpants
sudo -u postgres psql <<< "ALTER USER lemmingpants WITH PASSWORD 'lemmingpants';"

# Create the lemmingpants database which lemmingpants owns.
sudo -u postgres createdb -O lemmingpants lemmingpants

# Create a secret and save it.
FP=/usr/local/etc/secret_key.txt
if [ ! -f $FP ]; then
  CHARS=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
  RESULT=""
  for _ in {1..50}; do
    RESULT=$RESULT${CHARS:RANDOM%${#CHARS}:1}
  done
  echo $RESULT > $FP
  chmod 0644 $FP
fi;
SECRET_KEY=`cat $FP`
sudo -u postgres psql -d lemmingpants <<< 'ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "'$SECRET_KEY'"'

# Create all the extensions we need.
sudo -u postgres psql -d lemmingpants <<< 'CREATE EXTENSION IF NOT EXISTS pgcrypto; CREATE EXTENSION IF NOT EXISTS pgjwt;'

echo "Initialize database"
cd $SETUP_PATH
sudo -u postgres psql -d lemmingpants < init.sql

echo "Run tests"
cd $SETUP_PATH/pg_tests
sudo -u postgres /bin/bash run-tests.sh

ls /setup

echo "Cleaning up..."
cd / && rm -rf $SETUP_PATH
apt-get -y clean
echo "Done."
