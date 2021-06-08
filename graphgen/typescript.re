/* ================================================================
Typescript AST (ish)
================================================================ */
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
  // Not actual typescript, only used in generation  
  | Empty
  | Block(string, list(expr))
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
  let rec generate_expr = (expr, indent) => {
    let indent_str = String.make(indent * 2, ' ');
    switch (expr) {
    | Var(name) => name
    | Literal(value) => generate_literal(value)
    | UnaryOp(op, e) => generate_unary_op(op, generate_expr(e, 0))
    | BinaryOp(op, e1, e2) => generate_binary_op(op, generate_expr(e1, 0), generate_expr(e2, 0))
    | Declare(name, e) => [%string "%{indent_str}let %{name} = %{generate_expr e 0}"]
    | Assign(e1, e2) => [%string "%{indent_str}%{generate_expr e1 0} = %{generate_expr e2 0}"]
    | Apply(e1, args) => 
      let args = String.concat(", ", List.map(e => [%string "%{generate_expr e 0}"], args));
      [%string "%{indent_str}%{generate_expr e1 0}(%{args})"]
    | Cast(e, t) => [%string "%{indent_str}%{generate_expr e 0} as %{t};"]
    | Member(e, mem) => [%string "%{indent_str}%{generate_expr e 0}.%{mem}"]
    | New(e, args) => 
      let args = String.concat(", ", List.map(e => [%string "%{generate_expr e 0}"], args));
      [%string "new %{generate_expr e 0}(%{args})"]
    | FunDeclare(name, args, return_typ, body) =>
      let args = String.concat(", ", List.map(((name, typ)) => [%string "%{name}: %{typ}"], args));
      let body = String.concat("\n", List.map(e => generate_expr(e, 1), body));
      [%string "function %{name}(%{args}): %{return_typ} {\n%{body}\n}"]
    | Export(e) => [%string "export %{generate_expr e 0}"]
    | Return(e) => [%string "%{indent_str}return %{generate_expr e 0}"]
    | Empty => ""
    | Block("", []) => ""
    | Block("", exprs) =>
      let block = String.concat("\n", List.map(e => generate_expr(e, indent), exprs));
      [%string "%{block}"]
    | Block(comment, exprs) =>
      let block = String.concat("\n", List.map(e => generate_expr(e, indent), exprs));
      [%string "%{indent_str}// %{comment}\n%{block}"]
    }
  };

  generate_expr(expr, 0)
};

let generate_exprs = (exprs) => String.concat("\n", List.map(generate_expr, exprs));

/* ================================================================
AST Generation
================================================================ */
open Ast; // Solidity AST for types definitions

// Shorthands
let to_hex_string = (expr) => Apply(Member(expr, "toHexString"), []);

module Blocks = {
  let wrap = (e1, exprs, e2) => [e1, Block("", exprs), e2];
  
  let update_fields_event_handler = (contract_name, fields) => {
    let field_updates = 
      fields
      |> List.map(field => Assign(Member(Var("emitter"), field), Apply(Member(Var("emitterContract"), field), [])))
    ;

    Block("Update entity fields", [
      Declare("emitterContract", Apply(Member(Var(contract_name), "bind"), [Member(Var("event"), "address")])),
      ...field_updates
    ])
  };

  let init_entity_contract_fields = (contract_name, fields) => {
    let field_update = 
      fields
      |> List.map(field => Assign(Member(Var("entity"), field), Apply(Member(Var("entityContract"), field), [])))
    ;
    
    Block("Initialize entity fields", [
      // Declare("entityContract", Apply(Member(Var(contract_name), "bind"), [Apply(Member(Var("Address"), "fromString"), [Member(Var("entity"), "id")])])),
      Declare("entityContract", Apply(Member(Var(contract_name), "bind"), [Var("address")])),
      ...field_update
    ])
  };

  let init_entity_event_fields = (events: list(Subgraph.event)) => {
    Block("",
      events
      |> List.map((event: Subgraph.event) => Assign(Member(Var("entity"), [%string "num%{event.name}s"]), Var("BigIntZero")))
    )
  };

  let create_entity_from_event_field = (entity_name, contract_name, event_field) => {
    Block("", [
      Declare("entityAddress", Member(Member(Var("event"), "params"), event_field)),
      Apply(Member(Var(contract_name), "create"), [Var("entityAddress")]),
      Apply(Member(Var(entity_name), [%string "create%{entity_name}"]), [to_hex_string(Var("entityAddress"))])
    ])
  };

  let store_event = (event: Subgraph.event) => {
    Block("Create event and update emitter", [
      Declare("event", Apply(Var([%string "create%{event.name}"]), [Member(Var("emitter"), [%string "num%{event.name}s"]), Var("event")])),
      Assign(Member(Var("emitter"), [%string "num%{event.name}s"]), BinaryOp(Add, Member(Var("emitter"), [%string "num%{event.name}s"]), Var("BigIntOne"))),
      Assign(Member(Var("emitter"), [%string "latest%{event.name}"]), Member(Var("event"), "id"))
    ])
  };
};

module Functions = {
  let create_event = (event: Subgraph.event) => {
    let fields_assignment = 
      event.fields
      |> List.map(fun
        | (name, AddressT) => Assign(Member(Var("eventEntity"), name), to_hex_string(Member(Member(Var("event"), "params"), name)))
        | (name, _) => Assign(Member(Var("eventEntity"), name), Member(Member(Var("event"), "params"), name))
      )
    ;

    let event_id = BinaryOp(Add, 
      BinaryOp(Add, 
        to_hex_string(Member(Var("event"), "address")), 
        Literal(StringV("-"))
      ), Apply(Member(Var("counter"), "toString"), [])
    );

    let decl = [
      Declare("eventEntity", New(Var("MyEvent"), [event_id])),
      Assign(Member(Var("eventEntity"), "logIndex"), Member(Var("event"), "logIndex")),
      Assign(Member(Var("eventEntity"), "tx"), to_hex_string(Member(Member(Var("event"), "transaction"), "hash"))),
    ];

    let return = [
      Apply(Member(Var("eventEntity"), "save"), []),
      Return(Var("eventEntity"))
    ];

    let body = List.flatten([decl, fields_assignment, return]);

    FunDeclare([%string "create%{event.name}"], [("counter", "BigInt"), ("event", event.name)], event.name, body)
  };

  let create_entity = (contract: Subgraph.contract) => {
    open Subgraph;

    let events = 
      contract.handlers
      |> List.filter_map(handler => 
        switch (handler) {
        | Event(event, actions) when List.exists(fun | StoreEvent => true | _ => false, actions) => Some(event)
        | _ => None
        }
      )
    ;

    let fields = 
      contract.fields
      |> List.map(((name, typ)) => name)
    ;

    let body = [
      Declare("entity", New(Var(contract.name), [to_hex_string(Var("address"))])),
      // Declare("entityContract", Apply(Member(Var(contract_name), "bind"), [Var("address")])),
      Blocks.init_entity_contract_fields(contract.raw_name, fields),
      Blocks.init_entity_event_fields(events),
      Apply(Member(Var("entity"), "save"), []),
      Return(Var("entity"))
    ];

    Export(FunDeclare([%string "create%{contract.name}"], [("address", "Address")], contract.name, body))
  };

  let event_handler = (name, event: Subgraph.event, actions) => {
    let store_event_block = 
      actions
      |> List.find_map(fun 
        | Subgraph.StoreEvent => Some(Blocks.store_event(event)) 
        | _ => None
      )
      |> Option.value(~default=Empty)
    ;

    let update_fields_block = 
      actions
      |> List.filter_map(fun 
        | Subgraph.UpdateField(field) => Some(field) 
        | _ => None
      )
      |> Blocks.update_fields_event_handler(name)
    ;

    let create_entity_block = 
      actions
      |> List.filter_map(fun 
        | Subgraph.NewEntity(name, raw_name, field) => Some(Blocks.create_entity_from_event_field(name, raw_name, field))
        | _ => None
      )
    ;

    // let body = 
    //   actions
    //   |> List.map(fun
    //     | Subgraph.StoreEvent => Blocks.store_event(event)
    //     // | Subgraph.UpdateField()
    //   )

    // let emitter_decl = Declare("emitter", Apply(Member(Var(name), "load"), [to_hex_string(Member(Var("event"), "address"))]));
    let body = [
      // Get emitter entity
      Declare("emitter", Apply(Member(Var(name), "load"), [to_hex_string(Member(Var("event"), "address"))])),
      store_event_block,
      update_fields_block,
      Block("", create_entity_block),
      // Save emitter entity
      Apply(Member(Var("emitter"), "save"), [])
    ];

    [Export(FunDeclare([%string "handle%{event.name}"], [("event", event.name)], event.name, body))]
  };

  let call_handler = () => {
    Block("", [])
  };
};

/**
  [create_event_handler("MyContract", {"MyEvent", [("addr", AddressT)]}, [StoreEvent({"MyEvent", [("addr", AddressT)]})])] 
  generates the following:

  ```
  export function handleMyEvent(event: MyEvent): void {
    let emitter = MyContract.load(event.address.toHexString())
    let event = createMyEvent(emitter.numMyEvents, event)
    emitter.numMyEvents = emitter.numMyEvents + BigIntOne
    emitter.latestMyEvent = event
    emitter.save()
  }
  ```
 */
let create_event_handler = (name, event: Subgraph.event, actions) => {
  let store_event_block = 
    actions
    |> List.find_map(fun 
      | Subgraph.StoreEvent => Some(Blocks.store_event(event)) 
      | _ => None
    )
    |> Option.value(~default=Empty)
  ;

  let update_fields_block = 
    actions
    |> List.filter_map(fun 
      | Subgraph.UpdateField(field) => Some(field) 
      | _ => None
    )
    |> Blocks.update_fields_event_handler(name)
  ;

  let create_entity_block = 
    actions
    |> List.filter_map(fun 
      | Subgraph.NewEntity(name, raw_name, field) => Some(Blocks.create_entity_from_event_field(name, raw_name, field))
      | _ => None
    )
  ;

  // let body = 
  //   actions
  //   |> List.map(fun
  //     | Subgraph.StoreEvent => Blocks.store_event(event)
  //     // | Subgraph.UpdateField()
  //   )

  // let emitter_decl = Declare("emitter", Apply(Member(Var(name), "load"), [to_hex_string(Member(Var("event"), "address"))]));
  let body = [
    // Get emitter entity
    Declare("emitter", Apply(Member(Var(name), "load"), [to_hex_string(Member(Var("event"), "address"))])),
    store_event_block,
    update_fields_block,
    Block("", create_entity_block),
    // Save emitter entity
    Apply(Member(Var("emitter"), "save"), [])
  ];

  [Export(FunDeclare([%string "handle%{event.name}"], [("event", event.name)], event.name, body))]

//   let update_entity_meta = [%string "  
//   let event = create%{event.name}(emitter.num%{event.name}s, event)
//   emitter.num%{event.name}s = emitter.num%{event.name}s + BigIntOne
//   emitter.latest%{event.name} = event"
//   ];

//   [%string
// "%{if has_store_event then create_event event else \"\"}

// export function handle%{event.name}(event: %{event.name}): void {
//   let emitter = %{name}.load(event.address.toHexString().toString())
//   %{if has_store_event then update_entity_meta else \"\"}
//   emitter.save()
// }"
//   ]


};

// get create

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

let create_event' = (event: Subgraph.event) => {
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

let create_event_handler' = (name, event: Subgraph.event, actions) => {
  let has_store_event = List.exists(fun | Subgraph.StoreEvent => true | _ => false, actions);

  let update_entity_meta = [%string "  
  let event = create%{event.name}(emitter.num%{event.name}s, event)
  emitter.num%{event.name}s = emitter.num%{event.name}s + BigIntOne
  emitter.latest%{event.name} = event"
  ];

  [%string
"%{if has_store_event then create_event' event else \"\"}

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
    |> List.exists(fun | Subgraph.StoreCall => true | _ => false)
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
  |> List.map((({name, fields, handlers}: Subgraph.contract) as contract)  => Subgraph.(
    [%string "%{name}.ts"],
    handlers
    |> List.map(fun 
      | Event(event, actions) => Functions.event_handler(name, event, actions) |> generate_exprs
      | Call(call, actions) => create_call_handler(name, call, actions)
    )
    |> String.concat("\n")
    |> x => primitive_imports 
    ++ event_imports(handlers, name)
    ++ call_imports(handlers, name)
    ++ (Functions.create_entity(contract) |> generate_expr)
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