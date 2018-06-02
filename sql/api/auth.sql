SET SCHEMA 'api';

-- Public facing auth functions ---------------------------------------------

CREATE FUNCTION api.login(username TEXT, password TEXT) RETURNS jwt_token
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = api, model, public, pg_temp
    AS $$
    DECLARE
        t jwt_token;
        r record;
    BEGIN
        SELECT
            role::TEXT AS role,
            extract(EPOCH FROM NOW())::INTEGER + 86400 AS exp,
            'r'::TEXT as mode
        FROM model.users
        WHERE users.username=login.username
        AND users.password=crypt(login.password, users.password)
        INTO r;
        IF r IS NULL THEN
            RAISE sqlstate 'PT404' USING
                message = 'Cannot find user.',
                detail = 'There is no user with that username + password combination',
                hint = 'Try again!';
        ELSE
            SELECT sign(row_to_json(r), current_setting('app.jwt_secret')) INTO t;
            RETURN t;
        END IF;
    END
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
