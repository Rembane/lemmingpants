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
        'We throw error when we cant find an agenda item.');
END;
$$;

SELECT * FROM runtests('testing'::name);
