Unreleased
----------

- Add `Rel_sqlite3.Strict`, a table parameter for SQLite STRICT tables.
  Their bool, int and int64 columns are declared INTEGER; `schema_of_db`
  marks strict tables and reads those columns back as int.
- Add `Rel.Table.Check`, SQL CHECK constraints on tables: `Table.make
  ~checks`, `Table.checks`, the `Add_check` and `Drop_check` table changes.
  `Rel_sqlite3` writes them in `CREATE TABLE` and `schema_of_db` reads
  them back from the table's SQL.
- Add [Rel_sqlite3.Extension.apply] for per-connection initialization of
  statically linked SQLite extensions, with a C header for extension authors.

v0.0.1 YYYY-MM-DD Location
--------------------------

First release.
