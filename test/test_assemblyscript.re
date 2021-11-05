open OUnit2;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

let test_cases = 
  List.map(((document, mappings)) => (document, List.map(((source, mapping)) => (source, String.trim(mapping)), mappings))) @@ [
  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId)),
      ("owner", GQLNonNull(GQLBytes)),
      ("displayName", GQLNonNull(GQLString)),
      ("imageUrl", GQLNonNull(GQLString)),
    ]),
    mk_data_source(
      "GravatarRegistry", 
      "0x2E645469f354BB4F5c8a05B3b30A929361cf77eC", 
      1000923, 
      [%string {|%{Sys.getenv "TEST_DIR"}/Gravity.json|}]
    ),
    mk_event_handler("NewGravatar", "GravatarRegistry", [
      mk_new_entity("Gravatar", mk_var(~path=["event_", "params"], "id"), [
        ("owner", mk_var(~path=["event_", "params"], "owner")),
        ("displayName", mk_var(~path=["event_", "params"], "displayName")),
        ("imageUrl", mk_var(~path=["event_", "params"], "imageUrl")),
      ])
    ])
  ], [("GravatarRegistry", {|
import { Bytes, BigInt, Address } from '@graphprotocol/graph-ts'
import { BigIntZero, BigIntOne, BigDecimalZero, BigDecimalOne } from '../util'
import * as Schema from '../types/schema'
import * as Templates from '../types/templates'
import * as Source from '../types/GravatarRegistry/GravatarRegistry'

export function handleNewGravatar(event: Source.NewGravatar): void {
  let entity = new Gravatar(event.params.id)
  entity.owner = event.params.owner
  entity.displayName = event.params.displayName
  entity.imageUrl = event.params.imageUrl
  entity.save()
}
  |})]),

  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId)),
      ("owner", GQLNonNull(GQLBytes)),
      ("displayName", GQLNonNull(GQLString)),
      ("imageUrl", GQLNonNull(GQLString)),
    ]),
    mk_data_source(
      "GravatarRegistry", 
      "0x2E645469f354BB4F5c8a05B3b30A929361cf77eC", 
      1000923, 
      [%string {|%{Sys.getenv "TEST_DIR"}/Gravity.json|}]
    ),
    mk_event_handler("UpdatedGravatar", "GravatarRegistry", [
      mk_update_entity("Gravatar", mk_var(~path=["event_", "params"], "id"), [
        Assign("owner", mk_var(~path=["event_", "params"], "owner")),
        Assign("displayName", mk_var(~path=["event_", "params"], "displayName")),
        Assign("imageUrl", mk_var(~path=["event_", "params"], "imageUrl")),
      ])
    ])
  ], [("GravatarRegistry", {|
import { Bytes, BigInt, Address } from '@graphprotocol/graph-ts'
import { BigIntZero, BigIntOne, BigDecimalZero, BigDecimalOne } from '../util'
import * as Schema from '../types/schema'
import * as Templates from '../types/templates'
import * as Source from '../types/GravatarRegistry/GravatarRegistry'

export function handleUpdatedGravatar(event: Source.UpdatedGravatar): void {
  let entity = Gravatar.load(event.params.id)
  entity.owner = event.params.owner
  entity.displayName = event.params.displayName
  entity.imageUrl = event.params.imageUrl
  entity.save()
}
  |})]),

  ([
    mk_entity("Gravatar", [
      ("id", GQLNonNull(GQLId)),
      ("owner", GQLNonNull(GQLBytes)),
      ("displayName", GQLNonNull(GQLString)),
      ("imageUrl", GQLNonNull(GQLString)),
      ("numUpdates", GQLNonNull(GQLBigInt))
    ]),
    mk_data_source(
      "GravatarRegistry", 
      "0x2E645469f354BB4F5c8a05B3b30A929361cf77eC", 
      1000923, 
      [%string {|%{Sys.getenv "TEST_DIR"}/Gravity.json|}]
    ),
    mk_event_handler("UpdatedGravatar", "GravatarRegistry", [
      mk_update_entity("Gravatar", mk_var(~path=["event_", "params"], "id"), [
        Assign("owner", mk_var(~path=["event_", "params"], "owner")),
        Assign("displayName", mk_var(~path=["event_", "params"], "displayName")),
        Assign("imageUrl", mk_var(~path=["event_", "params"], "imageUrl")),
        Increment("numUpdates")
      ])
    ])
  ], [("GravatarRegistry", {|
import { Bytes, BigInt, Address } from '@graphprotocol/graph-ts'
import { BigIntZero, BigIntOne, BigDecimalZero, BigDecimalOne } from '../util'
import * as Schema from '../types/schema'
import * as Templates from '../types/templates'
import * as Source from '../types/GravatarRegistry/GravatarRegistry'

export function handleUpdatedGravatar(event: Source.UpdatedGravatar): void {
  let entity = Gravatar.load(event.params.id)
  entity.owner = event.params.owner
  entity.displayName = event.params.displayName
  entity.imageUrl = event.params.imageUrl
  entity.numUpdates = entity.numUpdates + BigIntOne
  entity.save()
}
  |})])
];

let pp = (mappings) =>
  String.concat("\n") @@ List.map(((source, mapping)) => [%string "%{source}:\n%{mapping}"], mappings);

let make_single_test = ((document, expected)) =>
  string_of_document(document) >:: (_) => 
    document
    |> Gg_script.Validate.tcheck
    |> Transpiler.Assemblyscript.transpile
    |> (mappings) => assert_equal(~printer=pp, expected, List.map(((source, mapping)) => (source, String.trim(mapping)), mappings));

let suite = 
  "test_assemblyscript" >::: List.map(make_single_test, test_cases);