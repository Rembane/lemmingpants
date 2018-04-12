-- Roles --------------------------------------------------------------------

GRANT ALL PRIVILEGES ON SCHEMA api TO lemmingpants;

DROP ROLE IF EXISTS web_anon;
CREATE ROLE web_anon NOLOGIN;
GRANT web_anon TO lemmingpants;

GRANT USAGE ON SCHEMA api TO web_anon;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA api TO web_anon;
GRANT SELECT ON ALL TABLES IN SCHEMA api TO web_anon;

DROP ROLE IF EXISTS admin_user;
CREATE ROLE admin_user NOLOGIN;
GRANT admin_user TO lemmingpants;

GRANT USAGE ON SCHEMA api TO admin_user;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA api TO admin_user;
GRANT ALL ON ALL TABLES IN SCHEMA api TO admin_user;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA api TO admin_user;
