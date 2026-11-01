(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_version =
  Test.test "Rel_sqlite3.version" @@ fun () ->
  Test.Log.msg "SQLite version: %s" (Rel_sqlite3.version ());
  ()

(* Handle lifetime *)

let mem () = match Rel_sqlite3.open' ~mode:Memory ":memory:" with
| Ok db -> db | Error e -> Test.failstop "%s" (Rel_sqlite3.Error.message e)

let ok = function
| Ok v -> v | Error e -> Test.failstop "%s" (Rel_sqlite3.Error.message e)

let test_close_idempotent =
  Test.test "Rel_sqlite3.close: use after close is refused" @@ fun () ->
  let db = mem () in
  ok (Rel_sqlite3.close db);
  Test.invalid_arg (fun () -> Rel_sqlite3.close db);
  Test.invalid_arg (fun () -> Rel_sqlite3.exec_sql db "SELECT 1");
  ()

let test_close_live_stmt =
  Test.test "Rel_sqlite3.close: with a live user statement" @@ fun () ->
  let db = mem () in
  let st = ok (Rel_sqlite3.Stmt.create db "SELECT 1") in
  ok (Rel_sqlite3.close db);
  ok (Rel_sqlite3.Stmt.finalize st);
  ()

let test_finalize_twice =
  Test.test "Rel_sqlite3.Stmt.finalize: twice" @@ fun () ->
  let db = mem () in
  let st = ok (Rel_sqlite3.Stmt.create db "SELECT 1") in
  ok (Rel_sqlite3.Stmt.finalize st);
  ok (Rel_sqlite3.Stmt.finalize st);
  ok (Rel_sqlite3.close db);
  ()

let test_empty_sql =
  Test.test "Rel_sqlite3.Stmt.create: empty SQL is an error" @@ fun () ->
  let db = mem () in
  Test.holds (Result.is_error (Rel_sqlite3.Stmt.create db "-- nothing"));
  ()

let test_backup_finish_twice =
  Test.test "Rel_sqlite3.Backup.finish: twice" @@ fun () ->
  let src = mem () and dst = mem () in
  let b = ok (Rel_sqlite3.Backup.init ~dst ~src ()) in
  ok (Rel_sqlite3.Backup.finish b);
  Test.invalid_arg (fun () -> Rel_sqlite3.Backup.finish b);
  ()

let test_fold_raise_resets =
  Test.test "Rel_sqlite3.fold: raising callback resets the statement" @@
  fun () ->
  let db = mem () in
  let st =
    Rel_sql.Stmt.v "SELECT 4294967296" ~rev_args:[]
      ~result:Rel.Row.(t1 (bool "b"))
  in
  Test.raises Exit (fun () -> Rel_sqlite3.fold db st (fun _ _ -> raise Exit) ());
  (* A pending statement would make VACUUM fail. *)
  ok (Rel_sqlite3.exec_sql db "VACUUM");
  (* Bool columns are not truncated to 32 bits. *)
  Test.(list T.bool) (ok (Rel_sqlite3.fold db st List.cons [])) [true];
  ()

let test_fold_nested =
  Test.test "Rel_sqlite3.fold: nested fold on the same SQL" @@ fun () ->
  let db = mem () in
  let st =
    Rel_sql.Stmt.v "WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL \
                    SELECT x + 1 FROM c WHERE x < 3) SELECT x FROM c"
      ~rev_args:[] ~result:Rel.Row.(t1 (int "x"))
  in
  let inner () = ok (Rel_sqlite3.fold db st (fun x acc -> x :: acc) []) in
  let all =
    ok (Rel_sqlite3.fold db st (fun x acc -> (x, List.rev (inner ())) :: acc) [])
  in
  let one = [1; 2; 3] in
  Test.(list (T.pair T.int (T.list T.int))) (List.rev all)
    [1, one; 2, one; 3, one];
  (* The cached statement is usable again afterwards. *)
  Test.(list T.int) (List.rev (inner ())) one;
  ok (Rel_sqlite3.close db);
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else (exit (main ()))
