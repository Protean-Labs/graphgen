open OUnit2;
open Rresult;

open Solidity_parsing.Ast;

open Gg_script.Parsetree_util;

let printer = (doc) =>
  switch (doc) {
  | Ok(doc) => string_of_ast(doc)
  | Error(`Msg(msg)) => msg
  };

let test_cases = 
  List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected))) @@ [
  (
    Test_util.read_file([%string {|%{Sys.getenv "TEST_DIR"}/Gravity.sol|}]),
    [
      Definitions([
        mk_entity("Gravatar", [
          ("id", GQLNonNull(GQLId), None),
          ("owner", GQLNonNull(GQLBytes), None),
          ("displayName", GQLNonNull(GQLString), None),
          ("imageUrl", GQLNonNull(GQLString), None)
        ]),
        mk_data_source(
          "Gravity", 
          "GravatarRegistry",
          "0x2E645469f354BB4F5c8a05B3b30A929361cf77eC", 
          1000923
        ),
      ]),
      EventHandler("NewGravatar", [
        mk_new_entity("Gravatar", mk_var(~path=["event", "params"], "id"), [
          ("owner", mk_var(~path=["event", "params"], "owner")),
          ("displayName", mk_var(~path=["event", "params"], "displayName")),
          ("imageUrl", mk_var(~path=["event", "params"], "imageUrl")),
        ])
      ]),
      EventHandler("UpdatedGravatar", [
        mk_update_entity("Gravatar", mk_var(~path=["event", "params"], "id"), [
          Assign("owner", mk_var(~path=["event", "params"], "owner")),
          Assign("displayName", mk_var(~path=["event", "params"], "displayName")),
          Assign("imageUrl", mk_var(~path=["event", "params"], "imageUrl"))
        ])
      ])
    ]
  )
];

let make_single_test = ((document, expected)) =>
  String.escaped(document) >:: (_) => Inline.parse(document) |> (ast) => assert_equal(~printer=printer, expected, ast);

let suite = 
  "test_parser_inline" >::: List.map(make_single_test, test_cases);