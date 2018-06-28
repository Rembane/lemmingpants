SET SCHEMA 'model';

CREATE TABLE agenda_item (
    id         SERIAL PRIMARY KEY,
    supertitle TEXT,
    title      TEXT NOT NULL,
    order_     SERIAL,
    state      state DEFAULT 'init' NOT NULL
);
-- Only one agenda item may be active at the time.
CREATE UNIQUE INDEX ON agenda_item (state) WHERE state='active';

GRANT INSERT, UPDATE, REFERENCES ON agenda_item TO admin_user;
GRANT USAGE ON agenda_item_id_seq, agenda_item_order__seq TO admin_user;

CREATE TRIGGER agenda_item_news
    AFTER INSERT OR UPDATE ON agenda_item
    FOR EACH ROW
    EXECUTE PROCEDURE websocket_news();
