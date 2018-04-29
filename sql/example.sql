-- Example data  -------------------------------------------------------------

SET SCHEMA 'model';
SET search_path TO model, public;

INSERT INTO role(name) VALUES('web_anon');
INSERT INTO role(name) VALUES('admin_user');

INSERT INTO users(username, pwhash, role_id)
SELECT 'hen', crypt('grawr', gen_salt('bf', 8)), id FROM role WHERE name='admin_user';
