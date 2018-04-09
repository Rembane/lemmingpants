-- Example data  -------------------------------------------------------------

SET SCHEMA 'model';

INSERT INTO role(name) VALUES('web_anon');
INSERT INTO role(name) VALUES('insert_attendee_user');
INSERT INTO role(name) VALUES('admin_user');

INSERT INTO users(username, pwhash, role_id)
SELECT 'terminal', crypt('I will indeed be back.', gen_salt('bf', 8)), id FROM role WHERE name='insert_attendee_user';

INSERT INTO users(username, pwhash, role_id)
SELECT 'hen', crypt('grawr', gen_salt('bf', 8)), id FROM role WHERE name='admin_user';

SET SCHEMA 'api';

INSERT INTO attendee(id, cid, name) VALUES(1, 'ekeroot', 'A. Ekeroot');
INSERT INTO attendee(id, cid, name) VALUES(2, 'snelob', 'Snel Bob');
INSERT INTO attendee(id, cid, name) VALUES(3, 'bobbobson', 'Bob Bobsson');
INSERT INTO attendee(id, cid, name) VALUES(4, 'testson', 'Test Testson');
INSERT INTO attendee(id, cid, name) VALUES(5, 'doedsbengt', 'Döds Bengt');

INSERT INTO agenda_item(title, content, order_)
    VALUES('Mötets öppnande', 'Mötet bör öpnas med trumpetstötar och godis.', 1);
INSERT INTO agenda_item(title, content, order_)
    VALUES('Val av mötesordförande', 'Förslag: sittande.', 2);
INSERT INTO agenda_item(title, content, order_)
    VALUES('Val av reservmötesordförande ifall den första går sönder', 'Förslag: stående.', 3);
INSERT INTO agenda_item(title, content, order_)
    VALUES('Mer glass=?', 'JA! MER GLASS!!!', 4);
INSERT INTO agenda_item(title, content, order_)
    VALUES('Mer bäsk?', 'Mmmmmbäsk...', 5);
INSERT INTO agenda_item(title, content, order_)
    VALUES('Mötets stängande', 'Vi tänkte gå hem nu.', 6);

