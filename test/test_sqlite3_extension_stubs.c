#include "../src/sqlite3/rel_sqlite3_extension.h"
#include <caml/mlvalues.h>

static void test_function (sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
  (void) argc; (void) argv;
  sqlite3_result_int (ctx, 42);
}

static int test_init (sqlite3 *db, char **error, const sqlite3_api_routines *api)
{
  (void) error; (void) api;
  return sqlite3_create_function_v2 (db, "rel_test_extension", 0,
                                     SQLITE_UTF8, NULL, test_function,
                                     NULL, NULL, NULL);
}

static int fail_init (sqlite3 *db, char **error, const sqlite3_api_routines *api)
{
  (void) db; (void) api;
  *error = sqlite3_mprintf ("test initializer failed");
  return SQLITE_ERROR;
}

CAMLprim value ocaml_test_extension (value unit)
{ (void) unit; return rel_sqlite3_extension_make (test_init); }

CAMLprim value ocaml_test_failed_extension (value unit)
{ (void) unit; return rel_sqlite3_extension_make (fail_init); }
