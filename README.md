# Lemming pants

The purpose of this project is to develop a system to manage the speaker list of Datateknologsektionens meetings. Preferably in the simplest possible way.

Feel free to fork, do pull requests and post issues!

The name is much inspired by DHack's totem animal, the [lemming](https://www.youtube.com/watch?v=9A6vm92R9oU).

## Installation

Install the programs below and rock on!

- Postgrest https://github.com/begriffs/postgrest
- Postgresql
- A web server of your choice

### Initialize the database

```bash
createuser -P lemmingpants
createdb lemmingpants -O lemmingpants
psql -d lemmingpants < init.sql
```

## Running in dev-mode

For development, the local web-server that serves the static resources is run by with this command `./de-server.sh`
