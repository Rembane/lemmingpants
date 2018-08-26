SET SCHEMA 'api';

CREATE VIEW speaker AS
    SELECT * FROM model.speaker;

GRANT SELECT ON speaker TO read_access;
-- GRANT SELECT, REFERENCES ON speaker TO read_access;
-- GRANT INSERT (speaker_queue_id, attendee_id, state) ON speaker TO admin_user;
-- GRANT UPDATE (state) ON speaker TO admin_user;

-- This is used to determine the order of the speakers.
-- It lets us have an infinite number of speakers queues.
CREATE VIEW speaker_count AS
    SELECT speaker_queue_id, attendee_id, COUNT(*) AS times_spoken
    FROM speaker
    WHERE state = 'done'
    GROUP BY (speaker_queue_id, attendee_id);

-- Note that if a speaker hasn't spoken, this view will simply give you
-- times_spoken = 0. You can even in that case use this table without
-- fear as long as you give a speaker id as a ritual sacrifice.
CREATE VIEW active_speakers AS
  SELECT s.id, s.speaker_queue_id, s.attendee_id, s.state, COALESCE(sc.times_spoken, 0) AS times_spoken
  FROM model.speaker AS s
  LEFT JOIN speaker_count AS sc
  ON    s.attendee_id      = sc.attendee_id
  AND   s.speaker_queue_id = sc.speaker_queue_id
  WHERE s.state = 'init' OR s.state = 'active'
  ORDER BY times_spoken, s.id;

GRANT SELECT, REFERENCES ON active_speakers TO read_access;

-- Returns the id of the new current speaker if things worked out well, 0 otherwise.
CREATE FUNCTION set_current_speaker(id INTEGER) RETURNS INTEGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = api, model, public, pg_temp
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
