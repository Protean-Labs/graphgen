// get create
open Ast;

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

let generate = (subgraph: Subgraph.t) => {
  subgraph
  |> List.map(({name, fields, handlers}: Subgraph.contract) => Subgraph.(
    [%string "%{name}.ts"],
    handlers
    |> List.map(fun 
      | Event(event, actions) => create_event_handler(name, event, actions)
      | Call(call, actions) => create_call_handler(name, call, actions)
    )
    |> String.concat("\n")
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