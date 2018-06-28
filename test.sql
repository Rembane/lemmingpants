SET SCHEMA 'model';
SET ROLE 'admin_user';

BEGIN;
    SELECT plan(1);
    SELECT has_table('speaker_queue');
    SELECT * FROM finish();
ROLLBACK;

