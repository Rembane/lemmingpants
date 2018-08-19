SET SCHEMA 'api';

-- Public facing auth functions ---------------------------------------------

CREATE TYPE jwt_token AS (token TEXT);

CREATE FUNCTION api.login(username TEXT, password TEXT) RETURNS jwt_token
    LANGUAGE sql SECURITY DEFINER SET search_path = api, model, public, pg_temp
    AS $$
        SELECT sign(row_to_json(r), current_setting('app.jwt_secret')) AS token
        FROM (
            SELECT
                role::TEXT AS role,
                extract(EPOCH FROM NOW())::INTEGER + 86400 AS exp,
                'r'::TEXT as mode
            FROM model.users
            WHERE (username=username AND password=crypt(login.password, users.password))
        ) r;
    $$;

REVOKE ALL ON FUNCTION login(username TEXT, password TEXT) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION login(username TEXT, password TEXT) TO read_access;

-- Get token with permissions of web_anon.
-- Useful for websockets.
CREATE FUNCTION api.get_token() RETURNS jwt_token
  LANGUAGE sql SECURITY DEFINER SET search_path = model, public, pg_temp
  AS $$
    SELECT sign(json_build_object('role', 'web_anon', 'exp',
            extract(EPOCH FROM NOW())::INTEGER + 86400, 'mode', 'r'),
        current_setting('app.jwt_secret')) AS token;
  $$;

REVOKE ALL ON FUNCTION get_token() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION get_token() TO read_access;
