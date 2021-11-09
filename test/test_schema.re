open OUnit2;
open Rresult;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

let test_cases = 
  List.map(((document, expected)) => (document, String.trim(expected))) @@ [
  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId)),
      ("owner", GQLNonNull(GQLBytes)),
      ("displayName", GQLNonNull(GQLString)),
      ("imageUrl", GQLNonNull(GQLString)),
    ])
  ], {|
type Gravatar @entity {
  id: ID!
  owner: Bytes!
  displayName: String!
  imageUrl: String!
}
  |}),

  ([
    mk_entity("Transaction", [
      ("id", GQLNonNull(GQLId)),
      ("txIndex", GQLNonNull(GQLBigInt)),
      ("from", GQLNonNull(GQLBytes)),
      ("to", GQLBytes),
      ("blockNumber", GQLNonNull(GQLBigInt)),
      ("blockTimestamp", GQLNonNull(GQLBigInt)),
      ("events", GQLNonNull(GQLList(GQLNonNull(GQLObject("Event")))))
    ]),
    mk_interface("Event", [
      ("id", GQLNonNull(GQLId)),
      ("logIndex", GQLNonNull(GQLBigInt)),
      ("tx", GQLNonNull(GQLObject("Transaction"))),
    ]),
    mk_entity(~interface="Event", "Swap", [
      ("id", GQLNonNull(GQLId)),
      ("logIndex", GQLNonNull(GQLBigInt)),
      ("tx", GQLNonNull(GQLObject("Transaction"))),
      ("sender", GQLNonNull(GQLBytes)),
      ("amount0In", GQLNonNull(GQLBigInt)),
      ("amount1In", GQLNonNull(GQLBigInt)),
      ("amount0Out", GQLNonNull(GQLBigInt)),
      ("amount1Out", GQLNonNull(GQLBigInt)),
      ("to", GQLNonNull(GQLBytes))
    ])
  ], {|
type Transaction @entity {
  id: ID!
  txIndex: BigInt!
  from: Bytes!
  to: Bytes
  blockNumber: BigInt!
  blockTimestamp: BigInt!
  events: [Event!]!
}

type Swap implements Event @entity {
  id: ID!
  logIndex: BigInt!
  tx: Transaction!
  sender: Bytes!
  amount0In: BigInt!
  amount1In: BigInt!
  amount0Out: BigInt!
  amount1Out: BigInt!
  to: Bytes!
}

interface Event {
  id: ID!
  logIndex: BigInt!
  tx: Transaction!
}
  |}),

  (
    R.get_ok @@ Gg_script.parse_file([%string {|%{Sys.getenv "TEST_DIR"}/gravatar.gg|}])
    |> Test_util.fix_abi_paths,
    {|
type Gravatar @entity {
  id: ID!
  owner: Bytes!
  displayName: String!
  imageUrl: String!
}
    |}
  )
];

let make_single_test = ((document, expected)) =>
  string_of_document(document) >:: (_) => 
    document
    |> Gg_script.Validate.tcheck
    |> Transpiler.Schema.transpile
    |> (document) => assert_equal(~printer=s => s, expected, String.trim(document));

let suite = 
  "test_schema" >::: List.map(make_single_test, test_cases);