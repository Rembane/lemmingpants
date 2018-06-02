SET SCHEMA 'api';

CREATE VIEW speaker AS
    SELECT * FROM model.speaker
    WITH CASCADED CHECK OPTION;

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

-- Returns the id of the new current speaker if things worked out well.
CREATE FUNCTION set_current_speaker(id INTEGER) RETURNS INTEGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = api, model, public, pg_temp
    AS $$
    DECLARE
        s speaker;
    BEGIN
        SELECT * INTO s
            FROM speaker WHERE speaker.id = set_current_speaker.id;
        IF s IS NULL THEN
            RAISE sqlstate 'PT404' USING
                message = 'Cannot find speaker.',
                detail = 'There is no speaker with that id.',
                hint = 'Try an id that exists.';
        ELSE
            UPDATE speaker SET state='done'
                WHERE speaker.speaker_queue_id = s.speaker_queue_id
                AND state='active';
            UPDATE speaker SET state='active' WHERE speaker.id=set_current_speaker.id;
            RETURN s.id;
        END IF;
    END
    $$;

REVOKE ALL ON FUNCTION set_current_speaker(id INTEGER) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION set_current_speaker(id INTEGER) TO admin_user;
