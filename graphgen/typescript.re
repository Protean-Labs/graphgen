// get create
open Ast;
/*
Imports 

*/
let primitive_imports = "
import {
  BigIntOne,
  BigIntZero,
  BigDecimalOne,
  BigDecimalZero
} from \"../utils\";
";
let event_imports = (event_handlers, name) => {
  let events = List.filter_map(fun 
    | Subgraph.(Event(event, _)) => Some(event)
    | _ => None
    , event_handlers);

  let import_alias = (event: Subgraph.event) => [%string "%{event.name} as %{event.name}Event"];

  let contract_types = (events) => [%string "
import {
  %{List.map import_alias events |> String.concat(\",\n \")}
} from \"../types/%{name}/%{name}\";
  "
  ];

  let basic_import = (event: Subgraph.event) => event.name;

  let schema_types = (events) => [%string "
import {
  %{List.map basic_import events |> String.concat(\",\n \")}
} from \"../types/schema\";
  "
  ];

  contract_types(events) ++ "\n" ++ schema_types(events) ++ "\n"
};

let call_imports = (call_handlers, name) => {
  let calls = List.filter_map(fun 
    | Subgraph.(Call(call, _)) => Some(call)
    | _ => None
    , call_handlers);

  let import_alias = (call: Subgraph.call) => [%string "%{call.name} as %{call.name}Call"];

  let contract_types = (calls) => [%string "
import {
  %{List.map import_alias calls |> String.concat(\"\n\")}
} from \"../types/%{name}/%{name}\";
  "
  ];

  let basic_import = (call: Subgraph.call) => call.name;

  let schema_types = (calls) => [%string "
import {
  %{List.map basic_import calls |> String.concat(\"\n\")}
} from \"../types/schema\";
  "
  ];

  contract_types(calls) ++ "\n" ++ schema_types(calls) ++ "\n"
};

type field = {
  name: string,
  field_type: Ast.typ
};

let typescript_type_init_value = fun
  | IntT(256) => "BigIntZero"
  | StringT => "string"
  | _ => "placeholder"
;

let typescript_field_init = (field) => {
  [%string "entity.%{field.name} = %{typescript_type_init_value field.field_type}"];
};

let create_entity = (name, fields) => {
  [%string 
"export function create%{name}(id: String): %{name} {
  let entity = new %{name}(id)
  %{List.map typescript_field_init fields |> String.concat(\"\n\")}
  entity.save();
  return entity
}"
  ];
};

let get_create = (name, fields) => {
  [%string 
"export function getCreate%{name}(id: String): %{name} {
  let entity = %{name}.load(id);
  if (entity == null) {
    entity = create%{name}(id)
  }
  return entity as %{name};
}"
  ];
};

let create_transaction = [%string 
"export function createTransaction(tx: Transaction, block: Block): void {
  let txEntity = new Transaction(tx.hash.toHexString().toString())
  txEntity.txIndex = tx.index
  txEntity.from = tx.from.toHexString().toString()
  txEntity.to = tx.to.toHexString().toString()
  txEntity.blockNumber = block.number
  txEntity.blockTimesatmp = block.timestamp
  txEntity.save()
}"
];

let get_create_transaction = [%string
"export function getCreateTransaction(tx: Transaction, block: Block): void {
  let txEntity = Transaction.load(tx.hash.toHexString().toString())
  if (txEntity == null) {
    createTransaction(tx, block)
  }
}"
];

let create_event = (event: Subgraph.event) => {
  let event_field_assignment = ((name, _)) => {
    [%string "eventEntity.%{name} = event.%{name}"]
  };

  [%string 
"function create%{event.name}(counter: BigInt, event: %{event.name}): %{event.name} {
  let eventEntity = new %{event.name}(event.address.toHexString().toString() + \"-\" counter.toString())
  eventEntity.logIndex = event.logIndex
  eventEntity.tx = event.transaction.hash.toHexString().toString()

  // Event parameters
  %{List.map event_field_assignment event.fields |> String.concat(\"\n  \")}

  eventEntity.save()

  return eventEntity
}"
  ]
};

let create_call = (call: Subgraph.call) => {
  let call_input_field_assignment = ((name, _)) => {
    [%string "callEntity.%{name} = call.inputs.%{name}"]
  };

  let call_output_field_assignment = ((name, _)) => {
    [%string "callEntity.%{name} = call.outputs.%{name}"]
  };

  [%string 
"function create%{call.name}(counter: BigInt, call: %{call.name}Call): void {
  let callEntity = new %{call.name}(call.to.toHexString().toString() + \"-\" counter.toString())
  callEntity.logIndex = call.logIndex
  callEntity.tx = call.transaction.hash.toHexString().toString()

  // Call inputs
  %{List.map call_input_field_assignment call.inputs |> String.concat(\"\n  \")}

  // Call outputs
  %{List.map call_output_field_assignment call.outputs |> String.concat(\"\n  \")}

  callEntity.save()

  return callEntity
}"  
  ]
};

let create_event_handler = (name, event: Subgraph.event, actions) => {
  let has_store_event = List.exists(fun | Subgraph.StoreEvent(_) => true | _ => false, actions);
  let empty = "";

  let update_entity_meta = [%string "  
  let event = create%{event.name}(emitter.num%{event.name}s, event)
  emitter.num%{event.name}s = emitter.num%{event.name}s + BigIntOne
  emitter.latest%{event.name} = event"
  ];

  [%string
"%{if has_store_event then create_event event else \"\"}

export function handle%{event.name}(event: %{event.name}): void {
  let emitter = %{name}.load(event.address.toHexString().toString())
  %{if has_store_event then update_entity_meta else \"\"}
  emitter.save()
}"
  ]
};

let create_call_handler = (name, call, actions) => {
  let store_call_function = {
    actions
    |> List.exists(fun | Subgraph.StoreCall(_) => true | _ => false)
    |> fun 
      | true => create_call(call)
      | false => ""
  };
  
  [%string
"%{store_call_function}

export function handle%{call.name}(call: %{call.name}Call): void {
  let emitter = %{name}.load(call.to.toHexString().toString())
  let call = create%{call.name}(emitter.num%{call.name}s, call)
  emitter.num%{call.name}s = emitter.num%{call.name}s + BigIntOne
  emitter.latest%{call.name} = call
  emitter.save()
}"
  ]
};

let of_subgraph = (subgraph: Subgraph.t) => {
  subgraph
  |> List.map(({name, fields, handlers}: Subgraph.contract) => Subgraph.(
    [%string "%{name}.ts"],
    handlers
    |> List.map(fun 
      | Event(event, actions) => create_event_handler(name, event, actions)
      | Call(call, actions) => create_call_handler(name, call, actions)
    )
    |> String.concat("\n")
    |> x => primitive_imports 
    ++ event_imports(handlers,name)
    ++ call_imports(handlers,name)
    ++ x
  ))
};

let utils_ts = [%string "
import { BigInt, BigDecimal, Address } from '@graphprotocol/graph-ts'

export const BigIntZero =  BigInt.fromI32(0)
export const BigIntOne =  BigInt.fromI32(1)
export const BigDecimalZero = BigDecimal.fromString('0')
export const BigDecimalOne = BigDecimal.fromString('1')

%{create_transaction}

%{get_create_transaction}
"];

// Typescript AST

type unary_op =
  | Neg
;

type binary_op = 
  | Add
  | Sub
  | Mult
  | Div
;

type value = 
  | StringV(string)
  | IntV(int)
  | FloatV(float)
;

type expr = 
  | Var(string)
  | Literal(value)
  | UnaryOp(unary_op, expr)
  | BinaryOp(binary_op, expr, expr)
  | Declare(string, expr)
  | Assign(expr, expr)
  | Apply(expr, list(expr))
  | Cast(expr, string)
  | Member(expr, string)
  | New(expr, list(expr))
  | FunDeclare(string, list((string, string)), string, list(expr))
  | Export(expr)
  | Return(expr)
;

type ts = list(expr);

let generate_literal = (value) => {
  switch (value) {
  | StringV(s) => [%string "\"%{s}\""]
  | IntV(i) => string_of_int(i)
  | FloatV(f) => string_of_float(f)
  }
};

let generate_unary_op = (op, e) => {
  switch (op) {
  | Neg => [%string "-%{e}"]
  }
};

let generate_binary_op = (op, e1, e2) => {
  switch (op) {
  | Add => [%string "%{e1} + %{e2}"]
  | Sub => [%string "%{e1} - %{e2}"]
  | Mult => [%string "%{e1} * %{e2}"]
  | Div => [%string "%{e1} / %{e2}"]
  }
};

let generate_expr = (expr) => {
  let rec generate_expr = ((expr, indent)) => {
    let indent_str = String.make(indent * 2, ' ');
    switch (expr) {
    | Var(name) => name
    | Literal(value) => generate_literal(value)
    | UnaryOp(op, e) => generate_unary_op(op, generate_expr((e, 0)))
    | BinaryOp(op, e1, e2) => generate_binary_op(op, generate_expr((e1, 0)), generate_expr((e2, 0)))
    | Declare(name, e) => [%string "%{indent_str}let %{name} = %{generate_expr((e, 0))}"]
    | Assign(e1, e2) => [%string "%{indent_str}%{generate_expr((e1, 0))} = %{generate_expr((e2, 0))}"]
    | Apply(e1, args) => 
      let args = String.concat(", ", List.map(e => [%string "%{generate_expr((e, 0))}"], args));
      [%string "%{indent_str}%{generate_expr((e1, 0))}(%{args})"]
    | Cast(e, t) => [%string "%{indent_str}%{generate_expr((e, 0))} as %{t};"]
    | Member(e, mem) => [%string "%{indent_str}%{generate_expr((e, 0))}.%{mem}"]
    | New(e, args) => 
      let args = String.concat(", ", List.map(e => [%string "%{generate_expr((e, 0))}"], args));
      [%string "new %{generate_expr((e, 0))}(%{args})"]
    | FunDeclare(name, args, return_typ, body) =>
      let args = String.concat(", ", List.map(((name, typ)) => [%string "%{name}: %{typ}"], args));
      let body = String.concat("\n", List.map(e => generate_expr((e, 1)), body));
      [%string "function %{name}(%{args}): %{return_typ} {\n%{body}\n}"]
    | Export(e) => [%string "export %{generate_expr((e, 0))}"]
    | Return(e) => [%string "%{indent_str}return %{generate_expr((e, 0))}"]
    }
  };

  generate_expr((expr, 0))
};

let generate_exprs = (exprs) => String.concat("\n", List.map(generate_expr, exprs));

// Code generation
let update_field_event_handler = (contract, fields) => {
  let field_update = 
    fields
    |> List.map(field => Assign(Member(Var("emitter"), field), Apply(Member(Var("emitterContract"), field), [])))
  ;

  [
    Declare("emitterContract", Apply(Member(Var(contract), "bind"), [Member(Var("event"), "address")])),
    ...field_update
  ]
};

let update_field_entity_creation = (contract, fields) => {  
  let field_update = 
    fields
    |> List.map(field => Assign(Member(Var("entity"), field), Apply(Member(Var("entityContract"), field), [])))
  ;
  
  [
    Declare("entityContract", Apply(Member(Var(contract), "bind"), [Apply(Member(Var("Address"), "fromString"), [Member(Var("entity"), "id")])])),
    ...field_update
  ]
};

let create_entity_from_event_field = (entity, event_field, update_fields_args) => {
  let entity_decl = [
    Declare("entityAddress", Apply(Member(Apply(Member(Member(Member(Var("event"), "params"), event_field), "toHexString"), []), "toString"), [])),
    Declare("entity", New(Var(entity), [Var("entityAddress")])),
  ];

  let update_fields = 
    update_fields_args
    |> fun 
      | Some((contract, fields)) => update_field_entity_creation(contract, fields)
      | None => []
  ;

  let save_entity = [
    Apply(Member(Var("entity"), "save"), [])
  ];

  List.flatten([entity_decl, update_fields, save_entity])
};