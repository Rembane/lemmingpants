-- Example data  -------------------------------------------------------------

SET SCHEMA 'model';
SET search_path TO model, public;

INSERT INTO users(username, password, role)
    VALUES('hen', 'grawr', 'admin_user');
