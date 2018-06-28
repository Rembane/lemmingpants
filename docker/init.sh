# Install pgtap:
cd /
git clone https://github.com/theory/pgtap/ /pgtap
cd /pgtap
RUN make
RUN make installcheck
RUN make install

cd /
echo 'ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "feBU1ykZ4icKs2nKam9l8CD84qhgeOl6QQakrUJBiRTUu4dKTLVoH8o";' | psql -d lemmingpants
echo 'CREATE EXTENSION pgcrypto; CREATE EXTENSION pgjwt; CREATE EXTENSION pgtap;' | psql -d lemmingpants
psql -d lemmingpants < /init.sql

