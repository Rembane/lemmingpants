TESTS = $(wildcard sql/*.sql)
REGRESS = $(patsubst sql/%.sql,%,$(TESTS))
REGRESS_OPTS = --load-language plpgsql
DBNAME = lp-tests

test: preload
	pg_prove -d $(DBNAME) --pset tuples_only=1 $(TESTS) -v

preload:
	./load-db.sh $(DBNAME)
