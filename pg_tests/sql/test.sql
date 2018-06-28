-- \unset ECHO
\i test_setup.sql

SELECT plan(2);
SELECT schemas_are(ARRAY['api', 'model', 'public']);
SELECT roles_are(ARRAY['web_anon', 'read_access', 'admin_user', 'lemmingpants', 'pg_signal_backend', 'postgres']);
SELECT * FROM finish();
ROLLBACK;
