SET SCHEMA 'api';

CREATE VIEW speaker AS
    SELECT * FROM model.speaker;

GRANT SELECT ON speaker TO read_access;
GRANT INSERT (speaker_queue_id, attendee_id, state) ON speaker TO admin_user;
GRANT UPDATE (state) ON speaker TO admin_user;
GRANT REFERENCES ON speaker TO admin_user;

CREATE VIEW active_speakers AS
    SELECT * FROM model.active_speakers;

GRANT SELECT ON active_speakers TO read_access;

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
