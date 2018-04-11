SET SCHEMA 'api';

CREATE TABLE attendee (
    id      SERIAL PRIMARY KEY,
    cid     TEXT NOT NULL UNIQUE,
    name    TEXT NOT NULL,
    nick    TEXT,
    created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
GRANT SELECT ON attendee TO insert_attendee_user, admin_user, web_anon;

CREATE TABLE attendee_number (
    id          SERIAL PRIMARY KEY,
    attendee_id INTEGER REFERENCES attendee NOT NULL,
    created     TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
GRANT SELECT ON attendee_number TO insert_attendee_user, admin_user, web_anon;

CREATE FUNCTION attendee_number_news() RETURNS TRIGGER
  LANGUAGE plpgsql SECURITY DEFINER SET search_path = api, model, public, pg_temp
  AS $$
  DECLARE
    j json;
  BEGIN
    SELECT row_to_json(r) INTO j
    FROM (
      SELECT a.id, a.cid, a.name, a.nick, json_agg(json_build_object('id', an.id)) AS numbers
      FROM attendee AS a
      LEFT JOIN attendee_number AS an ON an.attendee_id = a.id
      WHERE a.id = NEW.attendee_id
      GROUP BY a.id
    ) AS r;
    PERFORM model.send_websocket_notification('attendee' || '_' || LOWER(TG_OP), j);
    RETURN NEW;
  END
  $$;

CREATE TRIGGER attendee_number_news
    AFTER INSERT ON attendee_number
    FOR EACH ROW
    EXECUTE PROCEDURE attendee_number_news();

CREATE FUNCTION create_attendee(id INTEGER, cid TEXT, name TEXT, nick TEXT DEFAULT NULL) RETURNS VOID
  LANGUAGE plpgsql SECURITY DEFINER SET search_path = api, model, public, pg_temp
  AS $$
  DECLARE
    aid INTEGER;
  BEGIN
    SELECT attendee.id INTO aid FROM attendee WHERE attendee.cid = create_attendee.cid;
    IF aid IS NULL
      THEN INSERT INTO attendee(cid, name, nick) VALUES (create_attendee.cid, create_attendee.name, create_attendee.nick) RETURNING attendee.id INTO aid;
    END IF;
    INSERT INTO attendee_number(id, attendee_id) VALUES (create_attendee.id, aid);
  END
  $$;
REVOKE ALL ON FUNCTION create_attendee(INTEGER, TEXT, TEXT, TEXT) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION create_attendee(INTEGER, TEXT, TEXT, TEXT) TO web_anon, insert_attendee_user, admin_user;
