/* ISC license; see LICENSE.md. */
#ifndef REL_SQLITE3_EXTENSION_H
#define REL_SQLITE3_EXTENSION_H

#include <sqlite3.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* Initializers must be statically linked and usable without SQLite's dynamic
   extension API table (for SQLite extensions, compile with SQLITE_CORE).
   The pointer and its code must remain valid throughout the process. */
typedef int (*rel_sqlite3_extension_init)
  (sqlite3 *, char **, const sqlite3_api_routines *);

#define Rel_sqlite3_extension_val(v) \
  (*((rel_sqlite3_extension_init *) Data_abstract_val(v)))

/* Return a Rel_sqlite3.Extension.t from an extension's OCaml primitive. */
static inline value rel_sqlite3_extension_make (rel_sqlite3_extension_init init)
{
  value extension = caml_alloc
    ((sizeof (init) + sizeof (value) - 1) / sizeof (value), Abstract_tag);
  Rel_sqlite3_extension_val (extension) = init;
  return extension;
}
#endif
