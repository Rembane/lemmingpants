TESTS = $(wildcard sql/*.sql)
REGRESS = $(patsubst sql/%.sql,%,$(TESTS))
REGRESS_OPTS = --load-language plpgsql

test:
	pg_prove --pset tuples_only=1 $(TESTS)
