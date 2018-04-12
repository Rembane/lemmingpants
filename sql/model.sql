SET SCHEMA 'model';

GRANT USAGE ON SCHEMA model TO web_anon, admin_user, lemmingpants;

-- Auth ---------------------------------------------------------------------

CREATE TABLE role (
    id   SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

CREATE TABLE users (
    id       SERIAL PRIMARY KEY,
    username TEXT NOT NULL,
    pwhash   TEXT NOT NULL,
    role_id  INTEGER REFERENCES role NOT NULL
);
CREATE UNIQUE INDEX ON users (lower(username));

-- Useful functions ---------------------------------------------------------
CREATE FUNCTION send_websocket_notification(event TEXT, message json) RETURNS void
    LANGUAGE sql
    AS $$
        SELECT pg_notify(
          'postgres-websockets-listener',
          json_build_object('channel', 'state_updates', 'event', event, 'payload', message)::text
        );
    $$;

REVOKE ALL ON FUNCTION send_websocket_notification(event TEXT, message json) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION send_websocket_notification(event TEXT, message json)
    TO web_anon, admin_user, lemmingpants;
