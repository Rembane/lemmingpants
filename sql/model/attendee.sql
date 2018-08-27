SET SCHEMA 'model';

-- Note: That if you ever let people update their cids or insert new cids
--       from another vector than create_attendee then you _need_ to
--       lowercase the cid.
CREATE TABLE attendee (
    id      SERIAL PRIMARY KEY,
    cid     TEXT NOT NULL UNIQUE,
    name    TEXT NOT NULL,
    nick    TEXT,
    created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE attendee_number (
    id          INTEGER PRIMARY KEY,
    attendee_id INTEGER REFERENCES attendee NOT NULL,
    created     TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

-- Whenever an attendee number is added, we send a message to the world
-- containing the original attendee and its new number.
CREATE FUNCTION attendee_number_news() RETURNS TRIGGER
  LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
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
