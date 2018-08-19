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

-- API: Agenda item tests
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

    RETURN NEXT results_eq('SELECT api.set_current_agenda_item(1)', 'SELECT 1',
        'We can set current agenda item.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=1', 'active_plan',
        'The current agenda item is active.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=2', 'init_plan',
        'The next agenda item is init.');

    RETURN NEXT results_eq('SELECT api.set_current_agenda_item(2)', 'SELECT 2',
        'We can set current agenda item.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=2', 'active_plan',
        'The current agenda item is active.');
    RETURN NEXT results_eq('SELECT state FROM api.agenda_item WHERE id=1', 'done_plan',
        'The previous agenda item is done.');

    RETURN NEXT throws_ok('SELECT api.set_current_agenda_item(3)', 'PT404',
        'Cannot find agenda item.');
END;
$$;

-- API: Attendees
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

SELECT * FROM runtests('testing'::name);
