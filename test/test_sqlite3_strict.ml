(*---------------------------------------------------------------------------
   Copyright (c) 2026 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Rel

let get r = r |> Rel_sqlite3.string_error |> Result.get_ok'

type r = { flag : bool; count : int; big : int64; opt : int64 option;
           ratio : float; label : string }

let flag = Col.make "flag" Type.bool (fun r -> r.flag)
let count = Col.make "count" Type.int (fun r -> r.count)
let big = Col.make "big" Type.int64 (fun r -> r.big)
let opt = Col.make "opt" Type.(option int64) (fun r -> r.opt)
let ratio = Col.make "ratio" Type.float (fun r -> r.ratio)
let label = Col.make "label" Type.text (fun r -> r.label)
let row flag count big opt ratio label =
  { flag; count; big; opt; ratio; label }

let row = Row.(unit row * flag * count * big * opt * ratio * label)
let strict = Table.make "strict_t" row ~params:[Rel_sqlite3.Strict]
let plain = Table.make "plain_t" row

let with_db f =
  let db = Rel_sqlite3.open' ~mode:Memory "" |> get in
  Fun.protect ~finally:(fun () -> ignore (Rel_sqlite3.close db)) @@ fun () ->
  f db

let create db t =
  Rel_sqlite3.exec db (Rel_sql.create_table Rel_sqlite3.dialect t) |> get

let test_create =
  Test.test "STRICT tables declare INTEGER for every integer" @@ fun () ->
  let src t = Rel_sql.Stmt.src (Rel_sql.create_table Rel_sqlite3.dialect t) in
  Snap.string (src strict) @> __POS_OF__
    "CREATE TABLE \"strict_t\" (\n\
    \  \"flag\" INTEGER NOT NULL,\n\
    \  \"count\" INTEGER NOT NULL,\n\
    \  \"big\" INTEGER NOT NULL,\n\
    \  \"opt\" INTEGER,\n\
    \  \"ratio\" REAL NOT NULL,\n\
    \  \"label\" TEXT NOT NULL\n\
     ) STRICT;";
  Snap.string (src plain) @> __POS_OF__
    "CREATE TABLE \"plain_t\" (\n\
    \  \"flag\" BOOL NOT NULL,\n\
    \  \"count\" INTEGER NOT NULL,\n\
    \  \"big\" BIGINT NOT NULL,\n\
    \  \"opt\" BIGINT,\n\
    \  \"ratio\" REAL NOT NULL,\n\
    \  \"label\" TEXT NOT NULL\n\
     );";
  ()

let test_values =
  Test.test "STRICT tables store and enforce typed values" @@ fun () ->
  with_db @@ fun db ->
  create db strict;
  let v = { flag = true; count = 3; big = Int64.max_int; opt = Some (-1L);
            ratio = 0.5; label = "x" } in
  Rel_sqlite3.exec db (Rel_sql.insert_into Rel_sqlite3.dialect strict v)
  |> get;
  let all = Rel_query.(Sql.of_bag' strict (Bag.table strict)) in
  let vs = Rel_sqlite3.fold db all List.cons [] |> get in
  Test.bool (vs = [v]) true;
  let bad = "INSERT INTO strict_t VALUES ('yes', 1, 1, NULL, 1.0, 'x')" in
  Test.bool (Result.is_error (Rel_sqlite3.exec_sql db bad)) true;
  ()

let test_schema_of_db =
  Test.test "schema_of_db marks STRICT tables" @@ fun () ->
  with_db @@ fun db ->
  create db strict; create db plain;
  let schema, issues = Rel_sqlite3.schema_of_db db |> get in
  Test.(list T.string) issues [];
  let table name =
    match
      List.find_opt (fun t -> Table.name' t = name) (Schema.tables schema)
    with
    | Some t -> t | None -> Test.failstop "no table %s" name
  in
  let is_strict (Table.Def t) =
    List.exists (function Rel_sqlite3.Strict -> true | _ -> false)
      (Table.params t)
  in
  let big_type (Table.Def t) =
    match List.find_opt (fun c -> Col.name' c = "big") (Table.cols t) with
    | Some (Col.Def c) -> Fmt.str "%a" Type.pp (Col.type' c)
    | None -> Test.failstop "no column big"
  in
  Test.bool (is_strict (table "strict_t")) true;
  Test.bool (is_strict (table "plain_t")) false;
  Test.string (big_type (table "strict_t")) (Fmt.str "%a" Type.pp Type.int);
  Test.string (big_type (table "plain_t")) (Fmt.str "%a" Type.pp Type.int64);
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
