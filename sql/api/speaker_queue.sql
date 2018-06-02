SET SCHEMA 'api';

CREATE VIEW speaker_queue AS
    SELECT * FROM model.speaker_queue;

REVOKE ALL ON speaker_queue FROM read_access, PUBLIC;
GRANT INSERT (agenda_item_id, state) ON speaker_queue TO admin_user;
GRANT UPDATE (state) ON speaker_queue TO admin_user;
GRANT REFERENCES ON speaker_queue TO admin_user;
GRANT SELECT ON speaker_queue TO read_access;
