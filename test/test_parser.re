open OUnit2;
open Rresult;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

let cmp = (doc, doc') => {
  let cmp_toplevel = (toplevel, toplevel') =>
    switch (toplevel, toplevel') {
    | (ABI({name, _}), ABI({name: name', _})) => name == name'
    | (toplevel, toplevel') => toplevel == toplevel'
    };

  List.fold_left2((acc, toplevel, toplevel') => acc && cmp_toplevel(toplevel, toplevel'), true, R.get_ok(doc), R.get_ok(doc'));
};

let pp_document = (doc) =>
  switch (doc) {
  | Ok(doc) => string_of_document(doc)
  | Error(`Msg(msg)) => msg
  };

let test_cases = 
  List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected))) @@ [
  ({|
entity Transaction {
  id: ID!
  index: BigInt!
  from: Bytes!
  to: Bytes
  blockNumber: BigInt!
  blockTimestamp: BigInt!
}
  |}, [
    Entity({
      name: "Transaction",
      interface: None,
      fields: [
        ("id", GQLNonNull(GQLId)),
        ("index", GQLNonNull(GQLBigInt)),
        ("from", GQLNonNull(GQLBytes)),
        ("to", GQLBytes),
        ("blockNumber", GQLNonNull(GQLBigInt)),
        ("blockTimestamp", GQLNonNull(GQLBigInt)),
      ]
    })
  ]),

  ({|
data_source Gravity is GravatarRegistry {
  addr = 0x2E645469f354BB4F5c8a05B3b30A929361cf77eC
  start_block = 1000923
}
  |}, [
    DataSource({
      name: "Gravity",
      abi: "GravatarRegistry",
      address: Literal(Address("0x2E645469f354BB4F5c8a05B3b30A929361cf77eC")),
      start_block: Literal(Int(1000923)),
    })
  ]),

  ({|
event_handler GravatarRegistry.NewGravatar {
  new_entity Gravatar[event_.params.id] {
    owner = event_.params.owner
    displayName = event_.params.displayName
    imageUrl = event_.params.imageUrl
    creationTx = Transaction[event_.transaction.hash]
  }
}
  |}, [
    EventHandler({
      event: "NewGravatar",
      source: "GravatarRegistry",
      actions: [
        NewEntity({
          name: "Gravatar",
          id: Variable(["event_", "params"], "id"),
          values: [
            ("owner", Variable(["event_", "params"], "owner")),
            ("displayName", Variable(["event_", "params"], "displayName")),
            ("imageUrl", Variable(["event_", "params"], "imageUrl")),
            ("creationTx", Index(Variable([], "Transaction"), Variable(["event_", "transaction"], "hash"))),
          ]
        })
      ]
    })
  ]),

  ({|
event_handler GravatarRegistry.UpdatedGravatar {
  update Gravatar[event_.params.id] {
    owner = event_.params.owner
    displayName = event_.params.displayName
    imageUrl = event_.params.imageUrl
  }
}
  |}, [
    EventHandler({
      event: "UpdatedGravatar",
      source: "GravatarRegistry",
      actions: [
        UpdateEntity({
          name: "Gravatar",
          id: Variable(["event_", "params"], "id"),
          values: [
            Assign("owner", Variable(["event_", "params"], "owner")),
            Assign("displayName", Variable(["event_", "params"], "displayName")),
            Assign("imageUrl", Variable(["event_", "params"], "imageUrl")),
          ]
        })
      ]
    })
  ]),
  (
    Test_util.read_file([%string {|%{Sys.getenv "TEST_DIR"}/gravatar.gg|}]),
    [
      mk_entity("Gravatar", [
        ("id", GQLNonNull(GQLId)),
        ("owner", GQLNonNull(GQLBytes)),
        ("displayName", GQLNonNull(GQLString)),
        ("imageUrl", GQLNonNull(GQLString))
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
          Assign("imageUrl", mk_var(~path=["event_", "params"], "imageUrl"))
        ])
      ])
    ]
  )
];

let make_single_test = ((document, expected)) =>
  String.escaped(document) >:: (_) => Gg_script.parse(document) |> (ast) => assert_equal(~printer=pp_document, ~cmp=cmp, expected, ast);

let suite = 
  "test_parser" >::: List.map(make_single_test, test_cases);