SET SCHEMA 'model';

CREATE TABLE agenda_item (
    id         SERIAL PRIMARY KEY,
    title      TEXT NOT NULL,
    order_     SERIAL,
    state      state DEFAULT 'init' NOT NULL,
    parent     INTEGER REFERENCES agenda_item
);
-- Only one agenda item may be active at the time.
CREATE UNIQUE INDEX ON agenda_item (state) WHERE state='active';

GRANT INSERT, UPDATE, REFERENCES ON agenda_item TO admin_user;
GRANT USAGE ON agenda_item_id_seq, agenda_item_order__seq TO admin_user;

CREATE FUNCTION model.agenda_item_trim_whitespace() RETURNS TRIGGER
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
    BEGIN
        NEW.title = trim(NEW.title);
        RETURN NEW;
    END
    $$;

REVOKE ALL ON FUNCTION model.agenda_item_trim_whitespace() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION model.agenda_item_trim_whitespace() TO read_access;


CREATE TRIGGER agenda_item_trim_whitespace
    BEFORE INSERT OR UPDATE ON agenda_item
    FOR EACH ROW
    EXECUTE PROCEDURE agenda_item_trim_whitespace();

CREATE TRIGGER agenda_item_news
    AFTER INSERT OR UPDATE ON agenda_item
    FOR EACH ROW
    EXECUTE PROCEDURE websocket_news();

-- This table is only used for parsing.
-- You can of course put things in it, but what good will that do?
CREATE TABLE some_json (
    title TEXT NOT NULL,
    children JSON NOT NULL
);
