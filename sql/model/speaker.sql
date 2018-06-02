SET SCHEMA 'model';

-- A speaker can be deleted if it doesn't want to speak or has been added
-- by mistake. I'm not really clear on why I don't just let speakers be
-- deleted for real, but I do know that I really want to keep my
-- consistensy badge.
CREATE TYPE speaker_state AS ENUM ('init', 'active', 'done', 'deleted');

CREATE TABLE speaker (
    id               SERIAL PRIMARY KEY,
    speaker_queue_id INTEGER REFERENCES speaker_queue NOT NULL,
    attendee_id      INTEGER REFERENCES attendee NOT NULL,
    state            speaker_state DEFAULT 'init' NOT NULL,
    created          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

-- At most one speaker per queue may be active at the same time.
CREATE UNIQUE INDEX ON speaker (speaker_queue_id) WHERE state='active';

-- At most one speaker per queue and attendee may be init or active at the same time.
-- That is, you must speak before you add yourself to the queue again.
CREATE UNIQUE INDEX ON speaker (speaker_queue_id, attendee_id) WHERE state IN ('active', 'init');

GRANT SELECT ON speaker TO read_access;
GRANT INSERT (speaker_queue_id, attendee_id) ON speaker TO admin_user, authorized_attendee;
GRANT UPDATE (state) ON speaker TO admin_user;
GRANT REFERENCES ON speaker TO admin_user;
GRANT USAGE ON SEQUENCE speaker_id_seq TO admin_user, authorized_attendee;

CREATE FUNCTION speaker_only_insert_myself() RETURNS TRIGGER
    LANGUAGE plpgsql SET search_path = model, api, public, pg_temp
    AS $$
    BEGIN
        IF current_user <> 'authorized_attendee'
            THEN RETURN NEW;
        ELSIF current_setting('request.jwt.claim.lp_aid')::INTEGER = NEW.attendee_id
            THEN RETURN NEW;
        ELSE
            RAISE sqlstate 'PT403' USING
                message = 'You are not allowed to insert speakers.',
                detail = '',
                hint = '';
        END IF;
    END
    $$;

CREATE TRIGGER speaker_only_insert_myself
    BEFORE INSERT ON speaker
    FOR EACH ROW
    EXECUTE PROCEDURE speaker_only_insert_myself();

-- This is websocket_news() on steroids.
-- It does a join with active_speakers to give us the data we want
-- instead of the data we have, thus breaking the pattern the other
-- triggers follow.
CREATE FUNCTION speaker_news() RETURNS TRIGGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, api, public, pg_temp
    AS $$
    DECLARE
      j json;
    BEGIN
        SELECT row_to_json(r) INTO j
        FROM (
            SELECT s.id, s.speaker_queue_id, s.attendee_id, s.state, a.times_spoken, sq.agenda_item_id
            FROM speaker AS s
            LEFT JOIN api.active_speakers AS a
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

