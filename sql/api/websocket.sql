SET SCHEMA 'api';

CREATE FUNCTION websocket_news() RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    BEGIN
        PERFORM model.send_websocket_notification(LOWER(TG_TABLE_NAME) || '_' || LOWER(TG_OP), row_to_json(NEW));
        RETURN NEW;
    END
    $$;
