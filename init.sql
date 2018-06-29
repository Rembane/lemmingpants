-- Initialization ------------------------------------------------------------
--
-- NOTE: The role lemmingpants and the database lemmingpants must exist!
--
-- Drop all schemas, recreate them and make sure that Lemmingpants own them.
ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "feBU1ykZ4icKs2nKam9l8CD84qhgeOl6QQakrUJBiRTUu4dKTLVoH8o";
CREATE EXTENSION pgcrypto;
CREATE EXTENSION pgjwt;
CREATE EXTENSION pgtap;

DROP SCHEMA IF EXISTS model CASCADE;
CREATE SCHEMA model AUTHORIZATION lemmingpants;

DROP SCHEMA IF EXISTS api CASCADE;
CREATE SCHEMA api AUTHORIZATION lemmingpants;

CREATE TYPE model.state AS ENUM ('init', 'active', 'done');

-- The data model for Lemmingpants
\i /sql/model.sql
\i /sql/model/agenda_item.sql
\i /sql/model/speaker_queue.sql
\i /sql/model/attendee.sql
\i /sql/model/speaker.sql

-- Public tables, views and functions
\i /sql/api/attendee.sql
\i /sql/api/agenda_item.sql
\i /sql/api/speaker_queue.sql
\i /sql/api/speaker.sql
\i /sql/api/auth.sql

-- Example data
\i /sql/example.sql
