-- This file contains private stuff. If you want to use it publicly, create
-- a view or function or something in the api and use these things qualified.

-- Roles / Groups --------------------------------------------------------------
-- And their permissions                                                      --

-- TODO: Create the authenticator role. A non-privileged one with NOINHERIT.

-- This role has read access to everything that everybody should have read
-- access to. Saves quite a bit of code and clears up the concepts.
DROP ROLE IF EXISTS read_access;
CREATE ROLE read_access NOLOGIN;
GRANT USAGE ON SCHEMA api TO read_access;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA api TO read_access;
GRANT SELECT ON ALL TABLES IN SCHEMA api TO read_access;

-- Create the anonymous role that all users that do not authorize use.
DROP ROLE IF EXISTS web_anon;
CREATE ROLE web_anon NOLOGIN;
GRANT web_anon TO lemmingpants;
GRANT read_access TO web_anon;

-- The admin user can do a lot of fun things.
DROP ROLE IF EXISTS admin_user;
CREATE ROLE admin_user NOLOGIN;
GRANT admin_user TO lemmingpants;
GRANT read_access TO admin_user;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA api TO admin_user;

-- Infrastructure for user roles ----------------------------------------------

SET SCHEMA 'model';

-- Saves usernames and passwords and maps users to roles.
CREATE TABLE users (
    username text PRIMARY KEY,
    password text NOT NULL,
    role     name NOT NULL
);

CREATE UNIQUE INDEX ON users (LOWER(username));

CREATE FUNCTION check_role_exists() RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF NOT EXISTS (SELECT 1 FROM pg_roles AS r WHERE r.rolname = NEW.role) THEN
            RAISE FOREIGN_KEY_VIOLATION USING message = 'Unknown database role: ' || new.role;
            RETURN NULL;
        END IF;
        RETURN NEW;
    END
    $$;

CREATE CONSTRAINT TRIGGER ensure_user_role_exists
    AFTER INSERT OR UPDATE ON users
    FOR EACH ROW
        EXECUTE PROCEDURE check_role_exists();

CREATE FUNCTION encrypt_password() RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF tg_op = 'INSERT' OR NEW.password <> OLD.password THEN
            NEW.password = crypt(NEW.password, gen_salt('bf', 8));
        END IF;
        RETURN NEW;
    end
    $$;

CREATE TRIGGER encrypt_password
    BEFORE INSERT OR UPDATE ON users
    FOR EACH ROW
        EXECUTE PROCEDURE encrypt_password();

-- Useful functions ---------------------------------------------------------
CREATE FUNCTION model.send_websocket_notification(event TEXT, message json) RETURNS void
    LANGUAGE sql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
        SELECT pg_notify(
          'postgres-websockets-listener',
          json_build_object('channel', 'state_updates', 'event', event, 'payload', message)::text
        );
    $$;

REVOKE ALL ON FUNCTION model.send_websocket_notification(event TEXT, message json) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION model.send_websocket_notification(event TEXT, message json) TO read_access;

CREATE FUNCTION model.websocket_news() RETURNS TRIGGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
    BEGIN
        PERFORM model.send_websocket_notification(LOWER(TG_TABLE_NAME) || '_' || LOWER(TG_OP), row_to_json(NEW));
        RETURN NEW;
    END
    $$;

REVOKE ALL ON FUNCTION model.websocket_news() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION model.websocket_news() TO read_access;
