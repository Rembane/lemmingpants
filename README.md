# Lemmingpants

The purpose of this project is to develop a system to manage the speaker list of
Datateknologsektionens meetings.

Feel free to fork, do pull requests and post issues!

The name is much inspired by DHack's totem animal, the
[lemming](https://www.youtube.com/watch?v=9A6vm92R9oU).

## How to run a meeting

Lemmingpants has a Docker image to make deployment easier. To run a meeting you
need the `lemmingpants/backend-full` image. It exposes ports `8000` for
websockets and static files and `3000` for the API. One of the static files is
the frontend application where you need to set the domain and ports you want to
use.

Decide which combination of domains and ports you want to use and run
`deployment/boot_full_image.sh` which will start the backend Docker image with
the correct ports and URLs setup for you.

To terminate your SSL connection you can use Nginx. Example configuration below.
Adjust ports, domains and so on to suit your needs.

```
server {
    server_name lemmingpants.example.com
    include ssl.configs/tls-ssl-server.conf;

    # The Postgrest API
    location /pg {
        # Remove the pg-prefix.
        rewrite ^/pg(/.*)$ $1 break;
        proxy_pass http://localhost:3000/;
    }

    # The websockets
    location /ws {
        rewrite ^/ws(/.*)$ $1 break;
        proxy_pass http://localhost:8000/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }

    # The static files
    location / {
        proxy_pass http://localhost:8000/;
    }
}

server {
    listen 80;
    listen [::]:80;

    server_name lemmingpants.example.com;

    location / {
        return 301 https://$host$request_uri;
    }
}
```

## Development

There are a couple of different ways to install all prerequisites for
development, the most arduous one is below. Simple ones are coming, they will
involve Docker.

For development you should be good to go if you install the programs below:

- Postgresql >= 9.6
- Postgrest: https://github.com/begriffs/postgrest
- PGJWT: https://github.com/michelp/pgjwt
- postgres-websockets: https://github.com/diogob/postgres-websockets
- pgTAP: https://github.com/theory/pgtap for testing the backend
- pg_prove: https://pgtap.org/pg_prove.html also for testing the backend
- Some SASS binary on your `$PATH`. There are a couple to choose between.
  libsass and SassC are used in development, so if you use them it should just
  work. https://sass-lang.com/libsass
- Purescript >= 0.12.0
- psc-package: https://github.com/purescript/psc-package is used for
  handling dependencies.
- node is used for running the tests.
- Docker

### Initialize the database

```bash
createuser -P lemmingpants
createdb lemmingpants -O lemmingpants
echo 'ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "thisisnotaverygoodsecret";' | psql -d lemmingpants
echo 'CREATE EXTENSION pgcrypto; CREATE EXTENSION pgjwt; CREATE EXTENSION pgtap;' | psql -d lemmingpants
psql -d lemmingpants < init.sql
```

## Running in dev-mode

For development, the local web-server that serves the static resources is run with this
command `./dev-server.sh`

## Building the frontend

Build the frontend by running `make dev` in the `frontend/` directory.

## Architecture

Postgrest is used for the backend REST API, while postgrest-websockets handles the
websocket part. The backend code is written in SQL and PL/pgSQL. You can find it in
`init.sql` and `sql/` with subdirectories.

The frontend is written in Purescript using the library Purescript Halogen. The pretty
stuff is made with Bourbon, Bitters and Neat on SASS.

### Some references to tools used in this project:

- https://github.com/postgrest/postgrest
- https://github.com/slamdata/purescript-halogen
- https://www.bourbon.io
- https://neat.bourbon.io
- https://bitters.bourbon.io
