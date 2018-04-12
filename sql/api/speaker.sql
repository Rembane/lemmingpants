SET SCHEMA 'api';

CREATE TABLE speaker (
    id               SERIAL PRIMARY KEY,
    speaker_queue_id INTEGER REFERENCES speaker_queue NOT NULL,
    attendee_id      INTEGER REFERENCES attendee NOT NULL,
    state            state DEFAULT 'init' NOT NULL,
    created          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
-- At most one speaker per queue may be active at the time.
CREATE UNIQUE INDEX ON speaker (speaker_queue_id, state) where state='active';

REVOKE ALL ON TABLE speaker FROM admin_user, web_anon, PUBLIC;
GRANT INSERT (speaker_queue_id, attendee_id, state) ON speaker TO admin_user;
GRANT USAGE ON SEQUENCE speaker_id_seq TO admin_user;
GRANT UPDATE (state) ON speaker TO admin_user;
GRANT REFERENCES ON speaker TO admin_user;
GRANT SELECT ON speaker TO admin_user, web_anon;

-- This is used to determine the order of the speakers.
-- It lets us have an infinite number of speakers queues.
CREATE VIEW speaker_count AS
    SELECT speaker_queue_id, attendee_id, COUNT(*) AS times_spoken
    FROM speaker
    WHERE state = 'done'
    GROUP BY (speaker_queue_id, attendee_id);

GRANT SELECT ON speaker_count TO admin_user, web_anon;

-- Note that if a speaker hasn't spoken, this view will simply give you
-- times_spoken = 0. You can even in that case use this table without
-- fear as long as you give a speaker id as a ritual sacrifice.
CREATE VIEW active_speakers AS
  SELECT s.id, s.speaker_queue_id, s.attendee_id, s.state, COALESCE(sc.times_spoken, 0) AS times_spoken
  FROM speaker AS s
  LEFT JOIN speaker_count AS sc
  ON    s.attendee_id      = sc.attendee_id
  AND   s.speaker_queue_id = sc.speaker_queue_id
  WHERE s.state = 'init' OR s.state = 'active'
  ORDER BY times_spoken, s.id;

GRANT SELECT ON active_speakers TO admin_user, web_anon;

-- This is websocket_news() on steroids.
-- It does a join with active_speakers to give us the data we want
-- instead of the data we have, thus breaking the pattern the other
-- triggers follow.
CREATE FUNCTION speaker_news() RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    DECLARE
      j json;
    BEGIN
        SELECT row_to_json(r) INTO j
        FROM (
            SELECT s.id, s.speaker_queue_id, s.attendee_id, s.state, a.times_spoken, sq.agenda_item_id
            FROM speaker AS s
            LEFT JOIN active_speakers AS a
            ON s.id = a.id
            LEFT JOIN speaker_queue AS sq
            ON s.speaker_queue_id = sq.id
            WHERE s.id = NEW.id
        ) AS r;
        PERFORM model.send_websocket_notification(LOWER(TG_TABLE_NAME) || '_' || LOWER(TG_OP), j);
        RETURN NEW;
    END
    $$;

CREATE TRIGGER speaker_news
    AFTER INSERT OR UPDATE ON speaker
    FOR EACH ROW
    EXECUTE PROCEDURE speaker_news();

-- Returns the id of the new current speaker if things worked out well, 0 otherwise.
CREATE FUNCTION set_current_speaker(id INTEGER) RETURNS INTEGER
    LANGUAGE plpgsql
    AS $$
    DECLARE
        n    INTEGER = 0;
        sqid INTEGER = 0;
    BEGIN
        IF EXISTS(SELECT 1 FROM speaker WHERE speaker.id=set_current_speaker.id) THEN
            SELECT speaker_queue_id INTO sqid FROM speaker WHERE speaker.id = set_current_speaker.id;
            UPDATE speaker SET state='done'
                WHERE speaker.speaker_queue_id = sqid
                AND state='active';
            UPDATE speaker SET state='active' WHERE speaker.id=set_current_speaker.id
            RETURNING speaker.id INTO n;
            RETURN n;
        ELSE
            RETURN 0;
        END IF;
    END
    $$;

REVOKE ALL ON FUNCTION set_current_speaker(id INTEGER) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION set_current_speaker(id INTEGER) TO admin_user;
