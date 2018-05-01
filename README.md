# Lemmingpants

The purpose of this project is to develop a system to manage the speaker list of Datateknologsektionens meetings.

Feel free to fork, do pull requests and post issues!

The name is much inspired by DHack's totem animal, the [lemming](https://www.youtube.com/watch?v=9A6vm92R9oU).

## Installation

For development you should be good to go if you install the programs below:

- Postgresql >= 9.6
- Postgrest: https://github.com/begriffs/postgrest
- PGJWT: https://github.com/michelp/pgjwt
- postgres-websockets: https://github.com/diogob/postgres-websockets
- Some SASS binary on your `$PATH`. There are a couple to choose between. libsass and SassC are used in development, so if you use them it should just work. https://sass-lang.com/libsass
- A decent Purescript distribution

### Initialize the database

```bash
createuser -P lemmingpants
createdb lemmingpants -O lemmingpants
echo 'ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "feBU1ykZ4icKs2nKam9l8CD84qhgeOl6QQakrUJBiRTUu4dKTLVoH8o";' | psql -d lemmingpants
echo 'CREATE EXTENSION pgcrypto; CREATE EXTENSION pgjwt;' | psql -d lemmingpants
psql -d lemmingpants < init.sql
```

## Running in dev-mode

For development, the local web-server that serves the static resources is run with this command `./dev-server.sh`

## Building the frontend

Build the frontend by running `make dev` in the `frontend/` directory.

## Running with docker

Run `docker-build.sh` and wait for everything to build, then run `docker-compose up` and things should work.
