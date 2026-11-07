(*---------------------------------------------------------------------------
   Copyright (c) 2026 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Rel

let get r = r |> Rel_sqlite3.string_error |> Result.get_ok'

let amount = Col.make "amount" Type.int fst
let kind = Col.make "kind" Type.text snd
let row amount kind = amount, kind
let row = Row.(unit row * amount * kind)

let checks =
  [ Table.Check.make {|"amount" > 0|};
    Table.Check.make ~name:"kind_known" {|"kind" IN ('a', 'b,c', ')')|};
    Table.Check.make ~name:{|odd "name"|} {|length("kind") < (2 * 3)|} ]

let table = Table.make "entry" row ~checks

let with_db f =
  let db = Rel_sqlite3.open' ~mode:Memory "" |> get in
  Fun.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) @@ fun () ->
  f db

let create db =
  Rel_sqlite3.exec db (Rel_sql.create_table Rel_sqlite3.dialect table) |> get

let pairs cs = List.map (fun c -> Table.Check.(name c, expr c)) cs
let of_pairs ps = List.map (fun (name, e) -> Table.Check.make ?name e) ps

let check_list ps =
  List.map (fun (n, e) -> Option.value ~default:"-" n ^ ": " ^ e) ps

let live_table db name =
  let schema, issues = Rel_sqlite3.schema_of_db db |> get in
  Test.(list T.string) issues [];
  match List.find_opt (fun t -> Table.name' t = name) (Schema.tables schema)
  with
  | Some (Table.Def t) -> pairs (Table.checks t)
  | None -> Test.failstop "no table %s" name

let test_create =
  Test.test "Check constraints in CREATE TABLE" @@ fun () ->
  let stmt = Rel_sql.create_table Rel_sqlite3.dialect table in
  Snap.string (Rel_sql.Stmt.src stmt) @> __POS_OF__
    "CREATE TABLE \"entry\" (\n\
    \  \"amount\" INTEGER NOT NULL,\n\
    \  \"kind\" TEXT NOT NULL,\n\
    \  CHECK (\"amount\" > 0),\n\
    \  CONSTRAINT \"kind_known\" CHECK (\"kind\" IN ('a', 'b,c', ')')),\n\
    \  CONSTRAINT \"odd \"\"name\"\"\" CHECK (length(\"kind\") < (2 * 3))\n\
     );";
  ()

let test_enforced =
  Test.test "Check constraints are enforced" @@ fun () ->
  with_db @@ fun db ->
  create db;
  let insert v =
    Rel_sqlite3.exec db (Rel_sql.insert_into Rel_sqlite3.dialect table v)
  in
  Test.bool (Result.is_ok (insert (1, "a"))) true;
  Test.bool (Result.is_error (insert (0, "a"))) true;
  Test.bool (Result.is_error (insert (1, "b"))) true;
  ()

let test_round_trip =
  Test.test "schema_of_db reads check constraints back" @@ fun () ->
  with_db @@ fun db ->
  create db;
  let live = live_table db "entry" in
  Test.(list T.string) (check_list live) (check_list (pairs checks));
  let live = Table.make "entry" row ~checks:(of_pairs live) in
  Test.int (List.length (Table.changes ~src:live ~dst:table)) 0;
  ()

let test_written_by_hand =
  Test.test "schema_of_db reads hand-written check constraints" @@ fun () ->
  with_db @@ fun db ->
  let sql =
    "CREATE TABLE t (\n\
    \  a INTEGER CHECK (a <> 1), -- a column constraint, ignored\n\
    \  b TEXT, /* (, */\n\
    \  check(a < 10),\n\
    \  constraint [b ok] CHECK ( b <> '--)' )\n\
     )"
  in
  Rel_sqlite3.exec_sql db sql |> get;
  Test.(list T.string) (check_list (live_table db "t"))
    ["-: a < 10"; "b ok:  b <> '--)' "];
  ()

let test_changes =
  Test.test "Table.changes sees check changes" @@ fun () ->
  let changes src dst =
    let src = Table.make "entry" row ~checks:src in
    let dst = Table.make "entry" row ~checks:dst in
    List.sort String.compare
      (List.map (Fmt.str "%a" Table.pp_change) (Table.changes ~src ~dst))
  in
  let a = Table.Check.make "a" and b = Table.Check.make "b" in
  let n1 = Table.Check.make ~name:"n" "1" in
  let n2 = Table.Check.make ~name:"n" "2" in
  Test.(list T.string) (changes [a; n1] [a; n1]) [];
  Test.(list T.string) (changes [a; n1] [n2; b])
    ["ADD CHECK (b)"; "ADD CHECK n"; "DROP CHECK (a)"; "DROP CHECK n"];
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
