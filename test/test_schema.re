open OUnit2;
open Rresult;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

let test_cases = 
  List.map(((document, expected)) => (document, String.trim(expected))) @@ [
  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId), None),
      ("owner", GQLNonNull(GQLBytes), None),
      ("displayName", GQLNonNull(GQLString), None),
      ("imageUrl", GQLNonNull(GQLString), None),
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
      ("id", GQLNonNull(GQLId), None),
      ("txIndex", GQLNonNull(GQLBigInt), None),
      ("from", GQLNonNull(GQLBytes), None),
      ("to", GQLBytes, None),
      ("blockNumber", GQLNonNull(GQLBigInt), None),
      ("blockTimestamp", GQLNonNull(GQLBigInt), None),
      ("events", GQLNonNull(GQLList(GQLNonNull(GQLObject("Event")))), Some({name: "derivedFrom", args: [("field", String("transaction"))]}))
    ]),
    mk_interface("Event", [
      ("id", GQLNonNull(GQLId), None),
      ("logIndex", GQLNonNull(GQLBigInt), None),
      ("transaction", GQLNonNull(GQLObject("Transaction")), None),
    ]),
    mk_entity(~interface="Event", "Swap", [
      ("id", GQLNonNull(GQLId), None),
      ("logIndex", GQLNonNull(GQLBigInt), None),
      ("transaction", GQLNonNull(GQLObject("Transaction")), None),
      ("sender", GQLNonNull(GQLBytes), None),
      ("amount0In", GQLNonNull(GQLBigInt), None),
      ("amount1In", GQLNonNull(GQLBigInt), None),
      ("amount0Out", GQLNonNull(GQLBigInt), None),
      ("amount1Out", GQLNonNull(GQLBigInt), None),
      ("to", GQLNonNull(GQLBytes), None)
    ])
  ], {|
type Transaction @entity {
  id: ID!
  txIndex: BigInt!
  from: Bytes!
  to: Bytes
  blockNumber: BigInt!
  blockTimestamp: BigInt!
  events: [Event!]! @derivedFrom(field: "transaction")
}

type Swap implements Event @entity {
  id: ID!
  logIndex: BigInt!
  transaction: Transaction!
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
  transaction: Transaction!
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