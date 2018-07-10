cd / && psql -d lemmingpants < /init.sql
# Run pgtap tests
cd /pgtap && make installcheck
# Run application tests
cd /pg_tests && make test
