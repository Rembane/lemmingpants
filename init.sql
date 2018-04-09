-- Initialization ------------------------------------------------------------
--
-- The role lemmingpants and the database lemmingpants must exist!

DROP SCHEMA IF EXISTS model CASCADE;
CREATE SCHEMA model;

DROP SCHEMA IF EXISTS api CASCADE;
CREATE SCHEMA api AUTHORIZATION lemmingpants;
GRANT ALL PRIVILEGES ON SCHEMA api TO lemmingpants;

DROP EXTENSION IF EXISTS pgcrypto CASCADE;
CREATE EXTENSION pgcrypto WITH SCHEMA model;
DROP EXTENSION IF EXISTS pgjwt CASCADE;
CREATE EXTENSION pgjwt;

\i sql/role.sql
\i sql/model.sql
\i sql/api.sql
\i sql/example.sql
