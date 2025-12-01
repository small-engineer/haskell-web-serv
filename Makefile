STACK = stack
DBDIR = db
DB    = $(DBDIR)/mnist-web.db

.PHONY: run db-init clean

run:
	$(STACK) build
	$(STACK) run

db-init:
	mkdir -p $(DBDIR)
	rm -f $(DB)
	sqlite3 $(DB) 'CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, password TEXT NOT NULL);'

clean:
	$(STACK) clean
	rm -f $(DB)
