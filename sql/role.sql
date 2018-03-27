-- Roles --------------------------------------------------------------------

GRANT ALL PRIVILEGES ON SCHEMA api TO lemmingpants;

DROP ROLE IF EXISTS web_anon;
CREATE ROLE web_anon NOLOGIN;
GRANT web_anon TO lemmingpants;

GRANT USAGE ON SCHEMA api TO web_anon;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA api TO web_anon;
GRANT SELECT ON ALL TABLES IN SCHEMA api TO web_anon;

-- This user is used in the terminal interface.
DROP ROLE IF EXISTS insert_attendee_user;
CREATE ROLE insert_attendee_user NOLOGIN;
GRANT insert_attendee_user TO lemmingpants;
GRANT USAGE ON SCHEMA api TO insert_attendee_user;

DROP ROLE IF EXISTS admin_user;
CREATE ROLE admin_user NOLOGIN;
GRANT admin_user TO lemmingpants;

GRANT USAGE ON SCHEMA api TO admin_user;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA api TO admin_user;
GRANT ALL ON ALL TABLES IN SCHEMA api TO admin_user;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA api TO admin_user;


