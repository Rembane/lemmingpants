\unset ECHO
\i test_setup.sql

SELECT plan(1);
SELECT has_table('speaker_queue');
SELECT * FROM finish();
ROLLBACK;
