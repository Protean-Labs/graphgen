open Jingoo;

open Gg_script;
open Parsetree;
open Parsetree_util;
open Typetree;
open Database;

exception Runtime_error(string);

module Syntax = {
  type literal = 
    | String(string)
    | Int(int)
    | BigInt(int)
    | Float(float)
    | BigDecimal(float)
    | Bool(bool);

  type typ = 
    | TInt
    | TFloat
    | TString
    | TBool
    | TNull
    | TCustom(list(string), string)
    | TUnion(list(typ));

  let rec string_of_typ = (typ) =>
    switch (typ) {
    | TInt          => "int"
    | TFloat        => "float"
    | TString       => "string"
    | TBool         => "bool"
    | TNull         => "null"
    | TCustom(path, name) =>
      let path = String.concat(".", path);
      [%string "%{path}.%{name}"]  
    | TUnion(typs) =>
      List.map(string_of_typ, typs)
      |> String.concat(" | ");
    };

  type expr = 
    | Export(expr)
    | Function(string, list((string, typ)), typ, list(expr))
    | Let(string, expr)
    | Assign(expr, expr)
    | Variable(list(string), string)
    | Apply(expr, list(expr))
    | New(expr, list(expr))
    | Literal(literal)
    | Annotated(expr, typ)  // x as t
    | Neg(expr)
    | Add(expr, expr)
    | Sub(expr, expr)
    | Mul(expr, expr)
    | Div(expr, expr);

  let string_of_lit = fun
    | String(v)       => [%string "\"%{v}\""]
    | Int(v)          => string_of_int(v)
    | BigInt(0)       => "BigIntZero"
    | BigInt(1)       => "BigIntOne"
    | BigInt(v)       => [%string "BigInt.fromI32(%{string_of_int v})"]
    | Float(v)        => string_of_float(v)
    | BigDecimal(0.)  => "BigDecimalZero"
    | BigDecimal(1.)  => "BigDecimalOne"
    | BigDecimal(v)   => [%string "BigDecimal.fromString('%{string_of_float v}')"]
    | Bool(v)         => string_of_bool(v);

  let to_string = (document) => {
    let indent = (n) => String.init(2 * n, _ => ' ');

    let rec to_string = (~level=0, expr) => {
      let indent = indent(level);
      switch (expr) {
      | Export(e) => [%string "%{indent}export %{to_string e}"]
      | Function(name, args, typ, body) =>
        let body = 
          List.map(to_string(~level=level+1), body)
          |> String.concat("\n");

        let args = 
          List.map(((name, typ)) => [%string "%{name}: %{string_of_typ typ}"], args)
          |> String.concat(", ");

        [%string "%{indent}function %{name}(%{args}): %{string_of_typ typ}{\n%{body}}"]
      
      | Let(name, e) => 
        [%string "%{indent}let %{name} = %{to_string e}"]

      | Assign(e1, e2) =>
        [%string "%{indent}%{to_string e1} = %{to_string e2}"]

      | Variable(path, name) =>
        let path = String.concat(".", path);
        [%string "%{indent}%{path}.%{name}"]

      | Apply(e, args) =>
        let args = String.concat(", ") @@ List.map(to_string, args);
        [%string "%{indent}%{to_string e}(%{args})"]

      | New(e, args) =>
        let args = String.concat(", ") @@ List.map(to_string, args);
        [%string "%{indent}new %{to_string e}(%{args})"]

      | Literal(lit) => [%string "%{indent}%{string_of_lit lit}"]
      | Annotated(e, typ) => [%string "%{indent}%{to_string e} as %{string_of_typ typ}"]
      | Neg(e) => [%string "%{indent}-%{to_string e}"]
      | Add(e1, e2) => [%string "%{indent}%{to_string e1} + %{to_string e2}"]
      | Sub(e1, e2) => [%string "%{indent}%{to_string e1} - %{to_string e2}"]
      | Mul(e1, e2) => [%string "%{indent}%{to_string e1} * %{to_string e2}"]
      | Div(e1, e2) => [%string "%{indent}%{to_string e1} / %{to_string e2}"]
      }
    };

    List.map(to_string, document)
    |> String.concat("\n")
  };
};

let imports_template = {|
import { Bytes, BigInt, Address } from '@graphprotocol/graph-ts'
import { BigIntZero, BigIntOne, BigDecimalZero, BigDecimalOne } from '../util'
import * as Schema from '../types/schema'
import * as Templates from '../types/templates'
import * as Source from '../types/{% if mapping.is_template %}templates/{% endif %}{{ mapping.source_name }}/{{ mapping.source_name }}'
|};

let model_of_imports = (is_template, name) =>
  Jg_types.Tobj([
    ("is_template", Tbool(is_template)),
    ("source_name", Tstr(name))
  ]);

let event_handler_template = {|
export function handle{{ event_handler.event_name }}(event: Source.{{ event_handler.event_name }}): void {
  {%- for action_block in event_handler.actions -%}
  {{ action_block }}
  {%- endfor -%}
}
|};

let model_of_event_handler = (name, action_blocks) =>
  Jg_types.Tobj([
    ("event_name", Tstr(name)),
    ("actions", Tlist(
      List.map((action) => Jg_types.Tstr(action), action_blocks)
    ))
  ]);

// TODO:
let call_handler_template = {|
export function handle{{ call_handler.call_name }}(call: Source.{{ call_handler.call_name }}): void {
  {%- for action_block in call_handler.actions -%}
  {{ action_block }}
  {%- endfor -%}
}
|};

let model_of_call_handler = (name, action_blocks) =>
  Jg_types.Tobj([
    ("call_name", Tstr(name)),
    ("actions", Tlist(
      List.map((action) => Jg_types.Tstr(action), action_blocks)
    ))
  ]);

let new_entity_template = {|
  let entity = new {{ new_entity.entity_name }}({{ new_entity.id_expr }})
  {%- for (field, expr) in new_entity.values %}
  entity.{{ field }} = {{ expr }}
  {%- endfor %}
  entity.save()
|};

let model_of_new_entity = (name, id_expr, values) =>
  Jg_types.Tobj([
    ("entity_name", Tstr(name)),
    ("id_expr", Tstr(id_expr)),
    ("values", Tlist(
      List.map(((name, expr)) => Jg_types.Tset([Tstr(name), Tstr(expr)]), values)
    ))
  ]);

let update_template = {|
  let entity = {{ update.entity_name }}.load({{ update.id_expr }})
  {%- for (field, expr) in update.values %}
  entity.{{ field }} = {{ expr }}
  {%- endfor %}
  entity.save()
|};

let model_of_update = (name, id_expr, values) =>
  Jg_types.Tobj([
    ("entity_name", Tstr(name)),
    ("id_expr", Tstr(id_expr)),
    ("values", Tlist(
      List.map(((name, expr)) => Jg_types.Tset([Tstr(name), Tstr(expr)]), values)
    ))
  ]);

// TODO:
let new_template_template = {||};

let string_of_literal = fun
  | String(v)     => v
  | Bytes(v)      => v
  | Int(v)        => string_of_int(v)
  | BigInt(v)     => [%string "BigInt.fromI32(%{v})"]
  | Float(v)      => string_of_float(v)
  | BigDecimal(v) => [%string "BigDecimal.fromString(\"%{v}\")"]
  | Address(v)    => [%string "Address.fromString(\"%{v}\")"]
  | Bool(v)       => string_of_bool(v);

type import = {
  name: string,
  alias: option(string)
};

// let import_env = (db) => {
//   let data_sources_env = 
//     List.map(((name, data_source: data_source_t)) => 
//       (
//         [%string "../types/%{name}/%{name}"],
//         List.flatten @@ [
//           List.map(((name, _)) => {name: [%string "%{name}"], alias: Some([%string "%{name}Event"])}, data_source.contract.events),
//           List.map(((name, _)) => {name: [%string "%{name}"], alias: Some([%string "%{name}Call"])}, data_source.contract.calls)
//         ]
//       ), 
//       db.data_sources
//     );

//   let templates_env = 
//     List.cons(
//       ("../types/templates", List.map(((name, _)) => {name, alias: Some([%string "%{name}Contract"])}, db.templates)),
//       List.map(((name, template: template_t)) => 
//         (
//           [%string "../types/templates/%{name}/%{name}"],
//           List.flatten @@ [
//             List.map(((name, _)) => {name: [%string "%{name}"], alias: Some([%string "%{name}Event"])}, template.contract.events),
//             List.map(((name, _)) => {name: [%string "%{name}"], alias: Some([%string "%{name}Call"])}, template.contract.calls)
//           ]
//         ), 
//         db.templates
//       )
//     );

//   [
//     (
//       "@graphprotocol/graph-ts",
//       [
//         {name: "Address", alias: None},
//         {name: "BigInt", alias: None},
//         {name: "BigDecimal", alias: None}
//       ]
//     ),
//     (
//       "../util",
//       [
//         {name: "Transaction", alias: None},
//         {name: "BigIntZero", alias: None},
//         {name: "BigIntOne", alias: None},
//         {name: "BigDecimalZero", alias: None},
//         {name: "BigDecimalOne", alias: None},
//       ]
//     ),
//     (
//       "../types/schema",
//       List.map(((name, _)) => {name, alias: Some([%string "%{name}Entity"])}, db.entities)
//     ),
//     (
//       ""
//     )
//   ]
// };


let translation_map = fun
  | "self"    => "entity"
  | "event_"  => "event"
  | "call_"   => "call"
  | s => s;

let transpile = ((document, db, env)) => {
  let rec transpile = (db, env, toplevel) => 
    switch (toplevel) {
    | Interface(_)
    | Entity(_)
    | DataSource(_)
    | Template(_)
    | Event(_)
    | Call(_) => None

    | EventHandler({event, source, actions}) =>
      switch Database.(data_source_opt(db, source), template_opt(db, source)) {
      | (Some({contract, _}), _)
      | (None, Some({contract})) =>
        let {params} = List.assoc(event, contract.events);

        let model = model_of_event_handler(
          event,
          List.map(transpile_action(db, [("event_", Validate.struct_of_abi_event(params)), ...env]), actions)
        );

        Some((source, Jg_template.from_string(event_handler_template, ~models=[("event_handler", model)])))

      | _ => None
      }

    | CallHandler({call, source, actions}) => 
      switch Database.(data_source_opt(db, source), template_opt(db, source)) {
      | (Some({contract, _}), _)
      | (None, Some({contract})) =>
        let {inputs, outputs} = List.assoc(call, contract.calls);

        let model = model_of_event_handler(
          call,
          List.map(transpile_action(db, [("call_", Validate.struct_of_abi_call(inputs, outputs)), ...env]), actions)
        );

        Some((source, Jg_template.from_string(call_handler_template, ~models=[("call_handler", model)])))

      | _ => None
      }
    }
  and transpile_action = (db, env, action) =>
    switch (action) {
    | NewEntity({name, id, values}) => 
      let model = model_of_new_entity(
        name, 
        transpile_expr(db, env, id),
        List.map(((name, expr)) => (name, transpile_expr(db, env, expr)), values)
      );

      Jg_template.from_string(new_entity_template, ~models=[("new_entity", model)])

    | UpdateEntity({name, id, values}) =>
      let id_expr = transpile_expr(db, env, id);

      let values = List.map((field_mod) =>
        switch (field_mod, Option.get @@ Option.map(Validate.typ_of_gql_type(db)) @@ List.assoc_opt(field_of_field_mod(field_mod), Database.entity(db, name).fields)) {
        | (Increment(fname), TInt | TNonNull(TInt))       => (fname, [%string "entity.%{fname} + 1"])
        | (Increment(fname), TBigInt | TNonNull(TBigInt)) => (fname, [%string "entity.%{fname} + BigIntOne"])
        | (Decrement(fname), TInt | TNonNull(TInt))       => (fname, [%string "entity.%{fname} - 1"])
        | (Decrement(fname), TBigInt | TNonNull(TBigInt)) => (fname, [%string "entity.%{fname} - BigIntOne"])
        | (PlusEq(fname, expr), _)  => (fname, [%string "entity.%{fname} + %{transpile_expr db env expr}"]) 
        | (MinusEq(fname, expr), _) => (fname, [%string "entity.%{fname} - %{transpile_expr db env expr}"])
        | (Assign(fname, expr), _)  => (fname, transpile_expr(db, env, expr))
        | _ => raise(Runtime_error("update_entity: unexpected field_mod type for field %{fname}"))
        },
        values
      );

      let model = model_of_update(
        name,
        id_expr,
        values
      );

      Jg_template.from_string(update_template, ~models=[("update", model)])

    // TODO: NewTemplate
    // | NewTemplate({name, address}) =>
    //   switch (lookup(env, [], name)) {
    //   | 
    //   }
    | _ => raise(Runtime_error("tcheck_action: not implemented"))
    }
  and transpile_expr = (db, env, expr) => {
    let transpile_aritm = (e1, e2, op) => {
      let (e1_str, e2_str) = (
        transpile_expr(db, env, e1), 
        transpile_expr(db, env, e2)
      );

      switch (Validate.tcheck_expr(db, env, e1), Validate.tcheck_expr(db, env, e2)) {
      // Int
      | (TInt, TInt)        => [%string "%{e1_str} %{op} %{e2_str}"]

      // Float
      | (TFloat, TFloat)    => [%string "%{e1_str} %{op} %{e2_str}"]

      // BigInt
      | (TBigInt, TBigInt)  => [%string "%{e1_str} %{op} %{e2_str}"]
      | (TBigInt, TInt)     => [%string "%{e1_str} %{op} BigInt.fromI32(%{e2_str})"]
      | (TInt, TBigInt)     => [%string "BigInt.fromI32(%{e1_str}) %{op} %{e2_str}"]

      // BigDecimal
      | (TBigDecimal, TBigDecimal)  => [%string "%{e1_str} %{op} %{e2_str}"]
      | (TBigDecimal, TFloat)       => [%string "%{e1_str} %{op} BigDecimal.fromString(%{e2_str}.toString())"]
      | (TFloat, TBigDecimal)       => [%string "BigDecimal.fromString(%{e1_str}.toString()) %{op} %{e2_str}"]

      | _ => raise(Runtime_error([%string "transpile_arithm: invalid types for operator %{op}"]))
      }
    };

    switch (expr) {
    | Literal(lit) => string_of_literal(lit);

    | Neg(e) => [%string "-%{transpile_expr db env e}"]

    | Add(e1, e2) => 
      let (e1_str, e2_str) = (
        transpile_expr(db, env, e1),
        transpile_expr(db, env, e2)
      );

      switch (Validate.tcheck_expr(db, env, e1), Validate.tcheck_expr(db, env, e2)) {
      // Convert to string
      | (TString, TString)  => [%string "%{e1_str} + %{e2_str}"]
      | (TString, TAddress) => [%string "%{e1_str} + %{e2_str}.toHexString().toString()"]
      | (TAddress, TString) => [%string "%{e1_str}.toHexString().toString() + %{e2_str}"]
      | (TString, TBytes)   => [%string "%{e1_str} + %{e2_str}.toString()"]
      | (TBytes, TString)   => [%string "%{e1_str}.toString() + %{e2_str}"]

      | _ => transpile_aritm(e1, e2, "+")
      }

    | Sub(e1, e2) => transpile_aritm(e1, e2, "-")
    | Mul(e1, e2) => transpile_aritm(e1, e2, "*")
    | Div(e1, e2) => transpile_aritm(e1, e2, "/")

    | Variable([], name) => [%string "%{name}"]
    | Variable(path, name) => 
      let path_str = String.concat(".") @@ List.map(translation_map, path);
      [%string "%{path_str}.%{name}"]

    | Index(e1, e2) => 
      let (e1_str, e2_str) = (
        transpile_expr(db, env, e1), 
        transpile_expr(db, env, e2)
      );

      switch (Validate.tcheck_expr(db, env, e1)) {
      | TIndexable({typ: `Entity, _}) => [%string "%{e1_str}.load(%{e2_str})"]
      | TIndexable({typ: `Contract, _}) => [%string "%{e1_str}.bind(%{e2_str})"]
      | _ => raise(Runtime_error("transpile_expr: index expression not indexable"))
      }
      
    | Apply(expr, args) =>
      let args_str = String.concat(", ") @@ List.map(transpile_expr(db, env), args);
      [%string "%{transpile_expr db env expr}(%{args_str})"]
    };
  };

  let add_imports = (db, (source, mappings)) => {
    let imports_str = 
      switch (Database.type_of(db, source)) {
      | `DataSource => 
        Jg_template.from_string(imports_template, ~models=[("mapping", model_of_imports(false, source))])
      | `Template => 
        Jg_template.from_string(imports_template, ~models=[("mapping", model_of_imports(true, source))])
      | _ => raise(Runtime_error([%string "transpile_imports: source name %{source} neither template or data_source"]))
      };

    (source, [%string "%{imports_str}%{mappings}"])
  };

  List.filter_map(transpile(db, env), document)
  |> List.fold_left((acc, ((source, block))) => 
    switch (List.assoc_opt(source, acc)) {
    | None => [(source, [block]), ...acc]
    | Some(_) =>
      List.map(((source', blocks) as mapping) => source == source' ? (source', [block, ...blocks]) : mapping, acc)
    },
    []
  )
  |> List.map(((src, blocks)) => (src, String.concat("\n", blocks)))
  |> List.map(add_imports(db))
};