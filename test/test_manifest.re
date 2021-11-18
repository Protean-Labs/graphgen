open OUnit2;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

let test_cases = 
  List.map(((document, manifest)) => (document, String.trim(manifest))) @@ [
  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId), None),
      ("owner", GQLNonNull(GQLBytes), None),
      ("displayName", GQLNonNull(GQLString), None),
      ("imageUrl", GQLNonNull(GQLString), None),
    ]),
    mk_abi(
      "GravatarRegistry",
      [%string {|%{Sys.getenv "TEST_DIR"}/GravatarRegistry.json|}]
    ),
    mk_data_source(
      "Gravity", 
      "GravatarRegistry",
      "0x2E645469f354BB4F5c8a05B3b30A929361cf77eC", 
      1000923
    ),
    mk_event_handler("NewGravatar", "Gravity", [
      mk_new_entity("Gravatar", mk_var(~path=["event_", "params"], "id"), [
        ("owner", mk_var(~path=["event_", "params"], "owner")),
        ("displayName", mk_var(~path=["event_", "params"], "displayName")),
        ("imageUrl", mk_var(~path=["event_", "params"], "imageUrl")),
      ])
    ]),
    mk_event_handler("UpdatedGravatar", "Gravity", [
      mk_update_entity("Gravatar", mk_var(~path=["event_", "params"], "id"), [
        Assign("owner", mk_var(~path=["event_", "params"], "owner")),
        Assign("displayName", mk_var(~path=["event_", "params"], "displayName")),
        Assign("imageUrl", mk_var(~path=["event_", "params"], "imageUrl")),
      ])
    ])
  ], {|
dataSources:
  - kind: ethereum/contract
    name: Gravity
    network: mainnet
    source:
      address: '0x2E645469f354BB4F5c8a05B3b30A929361cf77eC'
      abi: GravatarRegistry
      startBlock: 1000923
    mapping:
      kind: ethereum/events
      apiVersion: 0.0.5
      language: wasm/assemblyscript
      file: ./src/mappings/Gravity.ts
      entities:
        - Gravatar
      abis:
        - name: GravatarRegistry
          file: ./abis/GravatarRegistry.json
      eventHandlers:
        - event: NewGravatar(uint256,address,string,string)
          handler: handleNewGravatar
        - event: UpdatedGravatar(uint256,address,string,string)
          handler: handleUpdatedGravatar
  |})
];

// let pp = (mappings) =>
//   String.concat("\n") @@ List.map(((source, mapping)) => [%string "%{source}:\n%{mapping}"], mappings);

let make_single_test = ((document, expected)) =>
  string_of_document(document) >:: (_) => 
    document
    |> Gg_script.Validate.tcheck
    |> Transpiler.Manifest.transpile
    |> (manifest) => assert_equal(~printer=s => s , expected, manifest);

let suite = 
  "test_mainfest" >::: List.map(make_single_test, test_cases);