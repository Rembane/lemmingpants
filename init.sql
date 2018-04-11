-- Initialization ------------------------------------------------------------
--
-- The role lemmingpants and the database lemmingpants must exist!

DROP SCHEMA IF EXISTS model CASCADE;
CREATE SCHEMA model;

DROP SCHEMA IF EXISTS api CASCADE;
CREATE SCHEMA api AUTHORIZATION lemmingpants;
GRANT ALL PRIVILEGES ON SCHEMA api TO lemmingpants;

CREATE TYPE api.state AS ENUM ('init', 'active', 'done');

\i sql/role.sql
\i sql/model.sql
-- Public tables, views and functions
\i sql/api/websocket.sql
\i sql/api/attendee.sql
\i sql/api/agenda_item.sql
\i sql/api/speaker_queue.sql
\i sql/api/speaker.sql
\i sql/api/auth.sql
-- Example data
\i sql/example.sql
