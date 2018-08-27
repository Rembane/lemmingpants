-- \unset ECHO
\i test_setup.sql

CREATE SCHEMA testing AUTHORIZATION lemmingpants;

CREATE OR REPLACE FUNCTION testing.test_db_structure()
RETURNS SETOF TEXT LANGUAGE plpgsql AS $$
BEGIN
    RETURN NEXT schemas_are(ARRAY['api', 'model', 'public', 'testing'],
        'These are the schemas we need.');
    RETURN NEXT has_role('web_anon');
    RETURN NEXT has_role('read_access');
    RETURN NEXT has_role('admin_user');
    RETURN NEXT has_role('lemmingpants');
    RETURN NEXT has_role('pg_signal_backend');
END;
$$;

-- Agenda item tests
CREATE OR REPLACE FUNCTION testing.test_agenda_item_structure()
RETURNS SETOF TEXT LANGUAGE plpgsql AS $$
BEGIN
    RETURN NEXT has_view('api', 'agenda_item',
        'The view api.agenda_item should exist.');
    RETURN NEXT columns_are('api', 'agenda_item',
        ARRAY['id', 'supertitle', 'title', 'order_', 'state']);
    RETURN NEXT has_function('api', 'set_current_agenda_item',
        ARRAY['integer']);
    RETURN NEXT function_privs_are('api', 'set_current_agenda_item',
        ARRAY['integer'], 'admin_user', ARRAY['EXECUTE']);
    RETURN NEXT function_privs_are('api', 'set_current_agenda_item',
        ARRAY['integer'], 'web_anon', '{}');
    RETURN NEXT function_privs_are('api', 'set_current_agenda_item',
        ARRAY['integer'], 'read_access', '{}');
    RETURN NEXT function_privs_are('api', 'set_current_agenda_item',
        ARRAY['integer'], 'lemmingpants', ARRAY['EXECUTE']);
    RETURN NEXT function_privs_are('api', 'set_current_agenda_item',
        ARRAY['integer'], 'pg_signal_backend', '{}');
END;
$$;

CREATE OR REPLACE FUNCTION testing.test_set_current_agenda_item()
RETURNS SETOF TEXT LANGUAGE plpgsql SET search_path = api, model, public AS $$
BEGIN
    PREPARE init_plan AS SELECT 'init'::state;
    PREPARE active_plan AS SELECT 'active'::state;
    PREPARE done_plan AS SELECT 'done'::state;

    INSERT INTO model.agenda_item(id, supertitle, title, order_, state)
        VALUES(1, 'SUPER!', 'Just a title', 1, 'init');
    INSERT INTO model.agenda_item(id, supertitle, title, order_, state)
        VALUES(2, 'SUPER!', 'Just a title', 2, 'init');

    RETURN NEXT row_eq('SELECT * FROM api.speaker_queue WHERE agenda_item_id=1',
        ROW(1, 1, 'init')::speaker_queue,
        'There is a speaker queue attached to the agenda item.');
    RETURN NEXT row_eq('SELECT * FROM api.speaker_queue WHERE agenda_item_id=2',
        ROW(2, 2, 'init')::speaker_queue,
        'There is a speaker queue attached to the agenda item.');

    PERFORM api.set_current_agenda_item(1);
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=1', 'active_plan',
        'The current agenda item is active.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=2', 'init_plan',
        'The next agenda item is init.');

    PERFORM api.set_current_agenda_item(2);
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=2', 'active_plan',
        'The current agenda item is active.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=1', 'done_plan',
        'The previous agenda item is done.');

    RETURN NEXT throws_ok('SELECT api.set_current_agenda_item(3)', 'PT404',
        'Cannot find agenda item.');
END;
$$;

-- Attendees
CREATE OR REPLACE FUNCTION testing.test_attendee_structure()
RETURNS SETOF TEXT LANGUAGE plpgsql SET search_path = api, model, public AS $$
BEGIN
    RETURN NEXT has_view('api', 'attendee',
        'The view api.attendee should exist.');
    RETURN NEXT columns_are('api', 'attendee',
        ARRAY['id', 'cid', 'name', 'nick', 'created']);
    RETURN NEXT table_privs_are('api', 'attendee',
        'read_access', ARRAY['SELECT']);
    RETURN NEXT table_privs_are('api', 'attendee',
        'admin_user', ARRAY['SELECT']);
    RETURN NEXT has_view('api', 'attendee_number',
        'The view api.attendee_number should exist.');
    RETURN NEXT columns_are('api', 'attendee_number',
        ARRAY['id', 'attendee_id', 'created']);
    RETURN NEXT table_privs_are('api', 'attendee_number',
        'read_access', ARRAY['SELECT']);
    RETURN NEXT table_privs_are('api', 'attendee_number',
        'admin_user', ARRAY['SELECT']);

    RETURN NEXT function_privs_are('api', 'create_attendee',
        ARRAY['integer', 'text', 'text', 'text'], 'read_access', ARRAY['EXECUTE']);

    RETURN NEXT table_privs_are('model', 'attendee',
        'admin_user', '{}', 'Not even an admin user should have access to the model table.');
    RETURN NEXT table_privs_are('model', 'attendee_number',
        'admin_user', '{}', 'Not even an admin user should have access to the model table.');
END;
$$;

CREATE OR REPLACE FUNCTION testing.test_attendee_functions()
RETURNS SETOF TEXT LANGUAGE plpgsql SET search_path = api, model, public AS $$
BEGIN
    RETURN NEXT lives_ok('SELECT api.create_attendee(1, ''CID'', ''name'', ''witty nickname'')',
        'We can create an attendee.');
    RETURN NEXT results_eq('SELECT cid FROM api.attendee', 'SELECT ''cid''',
        'create_attendee lowercases the CIDs.');
    RETURN NEXT results_eq('SELECT id FROM api.attendee_number WHERE attendee_id=1',
        Array[1],
        'Exactly one attendee_number is created for our attendee.');
    RETURN NEXT lives_ok('SELECT api.create_attendee(2, ''CID'', ''name'', ''witty nickname'')',
        'We can create the same attendee again, using the same CID.');
    RETURN NEXT results_eq('SELECT id FROM api.attendee_number WHERE attendee_id=1',
        Array[1, 2],
        'Exactly two attendee_numbers is created for our attendee.');
END;
$$;

-- Auth
CREATE OR REPLACE FUNCTION testing.test_auth()
RETURNS SETOF TEXT LANGUAGE plpgsql SET search_path = api, model, public AS $$
BEGIN
    RETURN NEXT has_type('api', 'jwt_token', 'Is jwt_token there as it should be?');
    RETURN NEXT has_function('api', 'login', ARRAY['text', 'text']);
    RETURN NEXT function_privs_are('api', 'login',
        ARRAY['text', 'text'], 'read_access', ARRAY['EXECUTE']);
    RETURN NEXT has_function('api', 'get_token', '{}');
    RETURN NEXT function_privs_are('api', 'get_token',
        '{}', 'read_access', ARRAY['EXECUTE']);

    RETURN NEXT has_table('model'::name, 'users'::name);
    RETURN NEXT columns_are('model', 'users', ARRAY['username', 'password', 'role']);
    RETURN NEXT col_is_pk('model', 'users', 'username',
        'The username column is primary key.');
    RETURN NEXT table_privs_are('model', 'users',
        'read_access', '{}', 'Noone should have access to the users table.');
    RETURN NEXT table_privs_are('model', 'users',
        'admin_user', '{}', 'Noone should have access to the users table.');
    RETURN NEXT table_privs_are('model', 'users',
        'lemmingpants', '{}', 'Noone should have access to the users table.');

    RETURN NEXT lives_ok('INSERT INTO model.users(username, password, role) VALUES(''testbob'', ''bestpassword'', ''read_access'')',
        'We should be able to create a user.');
    RETURN NEXT results_eq('SELECT username FROM model.users WHERE username=''testbob''',
        ARRAY['testbob'],
        'We should be able to get a user by username.'
        );
    RETURN NEXT results_ne('SELECT password FROM model.users WHERE username=''testbob''',
        ARRAY['bestpassword'],
        'The password should be scrambled.'
        );
    RETURN NEXT throws_ok('INSERT INTO model.users(username, password, role) VALUES(''bob2'', ''bestpassword'', ''norol'')',
        '23503',
        'Unknown database role: norol',
        'We should get an error message if the database role doesnt exist.');
    SET SESSION app.jwt_secret = 'secret';
    RETURN NEXT lives_ok('SELECT api.login(''testbob'', ''bestpassword'')',
        'We should be able to login.');
    RETURN NEXT throws_ok('SELECT api.login(''testbob'', ''worstpassword'')', 'PT404',
        'Cannot find user.',
        'Wrong password should not give us access.');
    RETURN NEXT lives_ok('SELECT api.get_token()',
        'We should be able to get an anonymous token.');
END;
$$;

-- Speakers
CREATE OR REPLACE FUNCTION testing.test_speaker()
RETURNS SETOF TEXT LANGUAGE plpgsql SET search_path = api, model, public AS $$
DECLARE
    aid INTEGER;
BEGIN
    INSERT INTO model.agenda_item(title, order_) VALUES('THIS IS A TITLE', 1);
    PERFORM api.create_attendee(1, 'CID', 'name', 'witty nickname');
    INSERT INTO model.speaker_queue(id, agenda_item_id) VALUES(1, 1);
    SELECT api.attendee.id INTO aid
        FROM api.attendee
        JOIN api.attendee_number
        ON api.attendee.id = api.attendee_number.attendee_id
        WHERE api.attendee_number.id = 1;
    INSERT INTO model.speaker(speaker_queue_id, attendee_id) VALUES(1, aid);
    RETURN NEXT lives_ok('SELECT api.set_current_speaker(1)',
        'We should be able to set the current speaker.');
    RETURN NEXT results_eq('SELECT state FROM api.speaker WHERE id=1',
        ARRAY['active'::speaker_state],
        'The state should now be active.');
END;
$$;

SELECT * FROM runtests('testing'::name);
