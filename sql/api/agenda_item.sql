SET SCHEMA 'api';

CREATE VIEW agenda_item AS
    SELECT * FROM model.agenda_item;

GRANT SELECT ON agenda_item TO read_access;
GRANT INSERT ON agenda_item TO admin_user;

CREATE FUNCTION set_current_agenda_item(id INTEGER) RETURNS VOID
    LANGUAGE plpgsql SECURITY DEFINER SET search_path = model, public, pg_temp
    AS $$
    DECLARE
        aid INTEGER;
    BEGIN
        SELECT agenda_item.id INTO aid FROM agenda_item
            WHERE agenda_item.id=set_current_agenda_item.id;
        IF aid IS NULL THEN
            RAISE sqlstate 'PT404' USING
                message = 'Cannot find agenda item.',
                detail = 'Agenda item with id' || (set_current_agenda_item.id :: text),
                hint = 'Use an id for an agenda item that exists.';
        ELSE
            UPDATE agenda_item SET state='done' WHERE state='active';
            UPDATE agenda_item SET state='active'
                WHERE agenda_item.id=aid;
        END IF;
    END
    $$;

REVOKE ALL ON FUNCTION set_current_agenda_item(INTEGER) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION set_current_agenda_item(id INTEGER) TO admin_user;

-- This: https://stackoverflow.com/questions/16174046/pre-order-depth-first-traversal-of-a-closure-table
-- And this: https://www.postgresql.org/docs/11/queries-with.html
-- are lifesavers.
CREATE FUNCTION create_agenda(agenda_json JSON) RETURNS VOID
  LANGUAGE sql SECURITY DEFINER SET search_path = model, public, pg_temp
  AS $$
    WITH RECURSIVE parse_json(id, title, children, parent, dpth, pth) AS (
            SELECT nextval('agenda_item_id_seq') AS id, title, children,
                   NULL :: BIGINT, 0 AS dpth, ARRAY[ordinality :: INTEGER]
            FROM json_populate_recordset(NULL :: model.some_json, agenda_json)
                 WITH ORDINALITY
        UNION ALL
            SELECT nextval('agenda_item_id_seq') AS id, result.title,
                   result.children, pj.id, pj.dpth + 1,
                   pth || (result.ordinality :: INTEGER)
            FROM parse_json AS pj,
                 json_populate_recordset(NULL :: model.some_json, pj.children)
                 WITH ORDINALITY AS result
    )
    INSERT INTO model.agenda_item(id, title, parent, order_)
    SELECT id, title, parent, nextval('agenda_item_order__seq') AS order_
        FROM parse_json ORDER BY pth;
  $$;

COMMENT ON FUNCTION create_agenda IS
  $$Create an agenda by supplying JSON in a specific format.

  The items of the agenda will get the order they occur in in the input data.

  Understood format:
    [{ "title": "Title of the agenda item",
       "children": [{"title": "Title of a child", "children": [] }]
    }]
  $$;

REVOKE ALL ON FUNCTION create_agenda(JSON) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION create_agenda(JSON) TO admin_user;
