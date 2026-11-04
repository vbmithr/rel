(* Tests for statically linked, connection-local extensions. *)
open B0_testing

external extension : unit -> Rel_sqlite3.Extension.t = "ocaml_test_extension"
external failed_extension : unit -> Rel_sqlite3.Extension.t =
  "ocaml_test_failed_extension"

let ok = function
| Ok v -> v | Error e -> Test.failstop "%s" (Rel_sqlite3.Error.message e)

let mem () = ok (Rel_sqlite3.open' ~mode:Memory ":memory:")

let test_extension =
  Test.test "static extensions are connection-local" @@ fun () ->
  let first = mem () and second = mem () in
  let sql = "SELECT rel_test_extension()" in
  Test.holds (Result.is_error (Rel_sqlite3.exec_sql first sql));
  ok (Rel_sqlite3.Extension.apply first (extension ()));
  let query = Rel_sql.Stmt.v sql ~rev_args:[] ~result:Rel.Row.(t1 (int "x")) in
  Test.(list T.int) (ok (Rel_sqlite3.fold first query List.cons [])) [42];
  Test.holds (Result.is_error (Rel_sqlite3.exec_sql second sql));
  ok (Rel_sqlite3.Extension.apply second (extension ()));
  ok (Rel_sqlite3.exec_sql second sql);
  ok (Rel_sqlite3.close first);
  Test.invalid_arg (fun () -> Rel_sqlite3.Extension.apply first (extension ()));
  ok (Rel_sqlite3.close second)

let test_error =
  Test.test "extension errors retain their message" @@ fun () ->
  let db = mem () in
  (match Rel_sqlite3.Extension.apply db (failed_extension ()) with
   | Ok () -> Test.failstop "expected initializer failure"
   | Error e -> Test.(string) (Rel_sqlite3.Error.message e) "test initializer failed");
  ok (Rel_sqlite3.exec_sql db "SELECT 1");
  ok (Rel_sqlite3.close db)

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
