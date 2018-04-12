-- Example data  -------------------------------------------------------------

SET SCHEMA 'model';
SET search_path TO model, public;

INSERT INTO role(name) VALUES('web_anon');
INSERT INTO role(name) VALUES('insert_attendee_user');
INSERT INTO role(name) VALUES('admin_user');

INSERT INTO users(username, pwhash, role_id)
SELECT 'terminal', crypt('I will indeed be back.', gen_salt('bf', 8)), id FROM role WHERE name='insert_attendee_user';

INSERT INTO users(username, pwhash, role_id)
SELECT 'hen', crypt('grawr', gen_salt('bf', 8)), id FROM role WHERE name='admin_user';

SET SCHEMA 'api';

-- The agenda items are from a real meeting. Cool, eh?
INSERT INTO agenda_item(title)
    VALUES('1. Preliminärer');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.1  Mötets öppnande');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.2  Val av mötesordförande');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.3  Val av vice mötesordförande');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.4  Val av mötessekreterare');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.5  Val av två justeringsmän och tillika rösträknare');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.6  Adjungeringar');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.7  Fastställande av mötesordning');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.8  Mötets behörighet och beslutsmässighet');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.9  Godkännande av föredragningslistan');
INSERT INTO agenda_item(supertitle, title)
VALUES('1. Preliminärer', '1.10 Föregående mötesprotokoll');

INSERT INTO agenda_item(title)
    VALUES('2. Meddelanden');

INSERT INTO agenda_item(title)
    VALUES('3. Bordlagda ärenden');

INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.1 Ansvarsfrihet för verksamhetsåret 2009/2010');
INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.2 Ansvarsfrihet för verksamhetsåret 2011/2012');
INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.3 Ansvarsfrihet för verksamhetsåret 2013/2014');
INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.4 Ansvarsfrihet för verksamhetsåret 2014/2015');
INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.5 Ansvarsfrihet för verksamhetsåret 2015/2016');
INSERT INTO agenda_item(supertitle, title)
VALUES('3. Bordlagda ärenden', '3.6 Ansvarsfrihet för verksamhetsåret 2016/2017');

INSERT INTO agenda_item(title)
    VALUES('4. Val till Ståthållarämbetet');

INSERT INTO agenda_item(supertitle, title)
VALUES('4. Val till Ståthållarämbetet', '4.1 Fanbärare');
INSERT INTO agenda_item(supertitle, title)
VALUES('4. Val till Ståthållarämbetet', '4.2 Ceremonimästare');
INSERT INTO agenda_item(supertitle, title)
VALUES('4. Val till Ståthållarämbetet', '4.3 0-2 vapendragare');

INSERT INTO agenda_item(title)
    VALUES('5. Val till DKock');

INSERT INTO agenda_item(supertitle, title)
VALUES('5. Val till DKock', '5.1 Ordförande');
INSERT INTO agenda_item(supertitle, title)
VALUES('5. Val till DKock', '5.2 Kassör');
INSERT INTO agenda_item(supertitle, title)
VALUES('5. Val till DKock', '5.3 0-5 övriga');

INSERT INTO agenda_item(title)
    VALUES('6. Propositioner');

INSERT INTO agenda_item(supertitle, title)
VALUES('6. Propositioner', '6.1 Preliminär verksamhetsplan för verksamhetsåret 2018/2019');
INSERT INTO agenda_item(supertitle, title)
VALUES('6. Propositioner', '6.2 Flytta pengar till lokalfonden');
INSERT INTO agenda_item(supertitle, title)
VALUES('6. Propositioner', '6.3 Preliminär budget för verksamhetsåret 2018/2019');

INSERT INTO agenda_item(title)
    VALUES('7. Motioner');

INSERT INTO agenda_item(supertitle, title)
VALUES('7. Motioner', '7.1 Ett lite mer autonomt talhenspresidium (andra läsningen)');

INSERT INTO agenda_item(title)
    VALUES('8. Val till Sektionsstyrelsen');
INSERT INTO agenda_item(supertitle, title)
VALUES('8. Val till Sektionsstyrelsen', '8.1 Ordförande');
INSERT INTO agenda_item(supertitle, title)
VALUES('8. Val till Sektionsstyrelsen', '8.2 Vice ordförande');
INSERT INTO agenda_item(supertitle, title)
VALUES('8. Val till Sektionsstyrelsen', '8.3 Kassör');
INSERT INTO agenda_item(supertitle, title)
VALUES('8. Val till Sektionsstyrelsen', '8.4 Sekreterare');
INSERT INTO agenda_item(supertitle, title)
VALUES('8. Val till Sektionsstyrelsen', '8.5 SAMO');

INSERT INTO agenda_item(title) VALUES('9. Val till Talhenspresidiet');
INSERT INTO agenda_item(supertitle, title) VALUES('9. Val till Talhenspresidiet', '9.1 Talhen');
INSERT INTO agenda_item(supertitle, title) VALUES('9. Val till Talhenspresidiet', '9.2 Vice talhen');
INSERT INTO agenda_item(supertitle, title) VALUES('9. Val till Talhenspresidiet', '9.3 Sekreterare');

INSERT INTO agenda_item(title)
    VALUES('10. Val till Datatekniks Nämnd för Studier');
INSERT INTO agenda_item(supertitle, title) VALUES('10. Val till Datatekniks Nämnd för Studier', '10.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('10. Val till Datatekniks Nämnd för Studier', '10.2 Vice ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('10. Val till Datatekniks Nämnd för Studier', '10.3 0-4 övriga medlemmar');

INSERT INTO agenda_item(title) VALUES('11. Val till Lekmannarevisorer');
INSERT INTO agenda_item(supertitle, title) VALUES('11. Val till Lekmannarevisorer', '11.1 2 lekmannarevisorer');

INSERT INTO agenda_item(title) VALUES('12. Val till D-lirium');
INSERT INTO agenda_item(supertitle, title) VALUES('12. Val till D-lirium', '12.1 4 redaktörer');

INSERT INTO agenda_item(title)
    VALUES('13. Val till iDrott');
INSERT INTO agenda_item(supertitle, title) VALUES('13. Val till iDrott', '13.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('13. Val till iDrott', '13.2 0-4 övriga medhjälpare');

INSERT INTO agenda_item(title)
    VALUES('14. Val till dHack');
INSERT INTO agenda_item(supertitle, title) VALUES('14. Val till dHack', '14.1 Superdatordriftsystemman');
INSERT INTO agenda_item(supertitle, title) VALUES('14. Val till dHack', '14.2 0-3 Mikrodatordriftsystemmän');

INSERT INTO agenda_item(title)
    VALUES('15. Val till Datas Ludologer');
INSERT INTO agenda_item(supertitle, title) VALUES('15. Val till Datas Ludologer', '15.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('15. Val till Datas Ludologer', '15.2 0-4 övriga');

INSERT INTO agenda_item(title)
    VALUES('16. Val till Dbrus');
INSERT INTO agenda_item(supertitle, title) VALUES('16. Val till Dbrus', '16.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('16. Val till Dbrus', '16.2 0-2 övriga');

INSERT INTO agenda_item(title)
    VALUES('17. Val till DAG');
INSERT INTO agenda_item(supertitle, title) VALUES('17. Val till DAG', '17.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('17. Val till DAG', '17.2 Kassör');
INSERT INTO agenda_item(supertitle, title) VALUES('17. Val till DAG', '17.3 4-5 övriga medlemmar');

INSERT INTO agenda_item(title)
    VALUES('18. Val till DBus');
INSERT INTO agenda_item(supertitle, title) VALUES('18. Val till DBus', '18.1 Bilansvarig');
INSERT INTO agenda_item(supertitle, title) VALUES('18. Val till DBus', '18.2 Vice Bilansvarig');

INSERT INTO agenda_item(title)
    VALUES('19. Val till D-foto');
INSERT INTO agenda_item(supertitle, title) VALUES('19. Val till D-foto', '19.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('19. Val till D-foto', '19.2 Kassör');
INSERT INTO agenda_item(supertitle, title) VALUES('19. Val till D-foto', '19.3 0-4 övriga medlemmar');

INSERT INTO agenda_item(title)
    VALUES('20. Val till DRUST');
INSERT INTO agenda_item(supertitle, title) VALUES('20. Val till DRUST', '20.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('20. Val till DRUST', '20.2 Kassör');
INSERT INTO agenda_item(supertitle, title) VALUES('20. Val till DRUST', '20.3 4 övriga medlemmar');

INSERT INTO agenda_item(title)
    VALUES('21. Val till D6');
INSERT INTO agenda_item(supertitle, title) VALUES('21. Val till D6', '21.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('21. Val till D6', '21.2 Kassör');
INSERT INTO agenda_item(supertitle, title) VALUES('21. Val till D6', '21.3 4-6 övriga medlemmar');

INSERT INTO agenda_item(title)
    VALUES('22. Val till Delta');
INSERT INTO agenda_item(supertitle, title) VALUES('22. Val till Delta', '22.1 Ordförande');
INSERT INTO agenda_item(supertitle, title) VALUES('22. Val till Delta', '22.2 Kassör');
INSERT INTO agenda_item(supertitle, title) VALUES('22. Val till Delta', '22.3 4 övriga medlemmar');

INSERT INTO agenda_item(title)
    VALUES('23. Övriga frågor');

INSERT INTO agenda_item(title)
    VALUES('24. Mötets avslutande');

