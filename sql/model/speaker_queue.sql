SET SCHEMA 'model';

CREATE TABLE speaker_queue (
    id             SERIAL PRIMARY KEY,
    agenda_item_id INTEGER REFERENCES agenda_item NOT NULL,
    state          state DEFAULT 'init' NOT NULL
);

REVOKE ALL ON speaker_queue FROM read_access, PUBLIC;
GRANT INSERT (agenda_item_id, state) ON speaker_queue TO admin_user;
GRANT UPDATE (state) ON speaker_queue TO admin_user;
GRANT SELECT, REFERENCES ON speaker_queue TO read_access;
GRANT USAGE ON speaker_queue_id_seq TO admin_user;

-- Triggers -----------------------------------------------------------------
CREATE FUNCTION create_speaker_queue() RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    BEGIN
      INSERT INTO speaker_queue(agenda_item_id) VALUES(NEW.id);
      RETURN NULL;
    END
    $$;

CREATE TRIGGER create_speakerqueue_when_agenda_item_is_created
    AFTER INSERT ON agenda_item
    FOR EACH ROW
    EXECUTE PROCEDURE create_speaker_queue();

CREATE FUNCTION at_least_one_speaker_queue() RETURNS TRIGGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
    DECLARE
        active_speaker_queues INTEGER;
    BEGIN
        IF NEW.state = 'done'
            THEN
                SELECT COUNT(id)
                INTO active_speaker_queues
                FROM model.speaker_queue
                WHERE agenda_item_id = NEW.agenda_item_id
                AND (state = 'init' OR state = 'active');

                IF active_speaker_queues < 2
                    THEN
                        RAISE EXCEPTION 'Not enough speaker queues left. I cannot let you do that Dave.';
                        RETURN OLD;
                    ELSE
                        RETURN NEW;
                    END IF;
            ELSE
                RETURN NEW;
            END IF;
    END
    $$;

CREATE TRIGGER at_least_one_speaker_queue
    BEFORE UPDATE ON model.speaker_queue
    FOR EACH ROW
    EXECUTE PROCEDURE at_least_one_speaker_queue();

CREATE FUNCTION check_if_top_speakerqueue() RETURNS TRIGGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
    DECLARE
        has_greater_friends BOOLEAN;
    BEGIN
        SELECT COUNT(id) > 0 INTO has_greater_friends
            FROM speaker_queue
            WHERE id > OLD.id
            AND agenda_item_id = OLD.agenda_item_id
            AND state = 'active';

        IF has_greater_friends
            THEN
                RAISE EXCEPTION 'This speaker queue is not on top of the stack!';
                RETURN OLD;
            ELSE
                RETURN NEW;
            END IF;
    END
    $$;

CREATE TRIGGER only_update_top_speakerqueue
    BEFORE UPDATE OF state ON speaker_queue
    FOR EACH ROW
    EXECUTE PROCEDURE check_if_top_speakerqueue();

CREATE TRIGGER speaker_queue_news
    AFTER UPDATE OR INSERT ON speaker_queue
    FOR EACH ROW
    EXECUTE PROCEDURE model.websocket_news();

