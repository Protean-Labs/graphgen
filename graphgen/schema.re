open Solidity;
open Subgraph;

type graphql_type =
  | BigInt
  | BigDecimal
  | String
  | Address
  | Int
  | Float
  | Bytes
;

let graphqltype_of_soltype = fun
  | FixedSizeBytesT(_) => "Bytes"
  | UintT(n) when n <= 32 => "Int"
  | UintT(_) => "BigInt"
  | IntT(n) when n <= 32 => "Int"
  | IntT(_) => "BigInt"
  | StringT => "String"
  | BoolT => "Bool"
  | AddressT => "Address"
;

let transaction_entity = "
type Transaction @entity {
  id: ID!   # tx hash
  txIndex: Int!
  from: Address!
  to: Address
  blockNumber: BigInt!
  blockTimestamp: BigInt!
  events: [Event!]! @derivedFrom(field: \"tx\")
  calls: [Call!]! @derivedFrom(field: \"tx\")
}
";

let event_interface = "
interface Event {
  id: ID!   # CONTRACT_ADDR-PER_EVENT_COUNTER
  logIndex: BigInt!
  tx: Transaction!
}
";

let call_interface = "
interface Call {
  id: ID!   # CONTRACT_ADDR-PER_CALL_COUNTER
  tx: Transaction!
}
";

let entity_of_event = (contract: Subgraph.contract, {name, fields}: Subgraph.event) => {
  let fields = 
    fields
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, graphqltype_of_soltype(typ)))
    |> String.concat("\n  ")
  ;

  Format.sprintf("
type %s implements Event @entity {
  id: ID!   # CONTRACT_ADDR-PER_EVENT_COUNTER
  logIndex: BigInt!
  tx: Transaction!

  %s
  
  %s: %s!
} 
  ", 
    name, 
    fields,
    String.uncapitalize_ascii(contract.name), 
    contract.name 
  );
};

let entity_of_call = (contract: Subgraph.contract, {name, inputs, outputs}: Subgraph.call) => {
  let inputs = 
    inputs
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, graphqltype_of_soltype(typ)))
    |> String.concat("\n  ")
  ;

  let outputs = 
    outputs
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, graphqltype_of_soltype(typ)))
    |> String.concat("\n  ")
  ;

  Format.sprintf("
type %s @entity implements Call {
  id: ID!   # CONTRACT_ADDR-PER_CALL_COUNTER
  tx: Transaction!

  %s
  %s

  %s: %s!
} 
  ", 
    name, 
    inputs, 
    outputs,
    String.uncapitalize_ascii(contract.name), 
    contract.name
  );
};

let contract_fields_of_event = (contract: Subgraph.contract, event: Subgraph.event) => {
  Format.sprintf("
  num%ss: BigInt!
  %ss: [%s!]! @derivedFrom(field: \"%s\")
  latest%s: %s
    ", 
    event.name, 
    String.uncapitalize_ascii(event.name), 
    event.name, 
    String.uncapitalize_ascii(contract.name), 
    event.name, 
    event.name
  )
};

let contract_fields_of_call = (contract: Subgraph.contract, call: Subgraph.call) => {
  Format.sprintf("
  num%ss: BigInt!
  %ss: [%s!]! @derivedFrom(field: \"%s\")
  latest%s: %s
  ", 
    call.name, 
    String.uncapitalize_ascii(call.name), 
    call.name, 
    String.uncapitalize_ascii(contract.name), 
    call.name, 
    call.name
  );
};

let entity_of_contract = (contract: Subgraph.contract) => {
  let contract_fields = 
    contract.fields
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, graphqltype_of_soltype(typ)))
    |> String.concat("\n  ")
  ;

  let events_contract_fields = 
    contract.triggers
    |> List.map(fun | Event(_, actions) => actions | _ => [])
    |> List.flatten
    |> List.filter_map(fun | StoreEvent(event) => Some(event) | _ => None)
    |> List.map(contract_fields_of_event(contract))
    |> String.concat("")
  ;

  let calls_contract_fields = 
    contract.triggers
    |> List.map(fun | Call(_, actions) => actions | _ => [])
    |> List.flatten
    |> List.filter_map(fun | StoreCall(call) => Some(call) | _ => None)
    |> List.map(contract_fields_of_call(contract))
    |> String.concat("")
  ;

  let all_fields = 
    [
      contract_fields,
      events_contract_fields,
      calls_contract_fields
    ]
    |> String.concat("\n")
  ;

  Format.sprintf("
type %s @entity {
  id: ID!   # Address
  %s
}
  ", contract.name, all_fields)
};

let of_subgraph = (subgraph: Subgraph.t) => {
  let event_entities = 
    subgraph
    |> List.map(contract => 
      contract.triggers 
      |> List.map(t => (contract, t))
    )
    |> List.flatten
    |> List.map(fun 
      | (contract, Event(_, actions)) => 
        actions 
        |> List.map(action => (contract, action)) 
      | _ => []
    )
    |> List.flatten
    |> List.filter_map(fun 
      | (contract, StoreEvent(event)) => Some((contract, event)) 
      | _ => None
    )
    |> List.map(((contract, event)) => entity_of_event(contract, event))
    |> String.concat("")
  ;

  let call_entities = 
    subgraph
    |> List.map(contract => 
      contract.triggers 
      |> List.map(t => (contract, t))
    )
    |> List.flatten
    |> List.map(fun 
      | (contract, Call(_, actions)) => 
        actions 
        |> List.map(action => (contract, action)) 
      | _ => []
    )
    |> List.flatten
    |> List.filter_map(fun 
      | (contract, StoreCall(call)) => Some((contract, call)) 
      | _ => None
    )
    |> List.map(((contract, call)) => entity_of_call(contract, call))
    |> String.concat("")
  ;

  let contract_entities = 
    subgraph
    |> List.map(entity_of_contract)
    |> String.concat("")
  ;

  [
    transaction_entity,
    event_interface,
    call_interface,
    event_entities,
    call_entities,
    contract_entities
  ]
  |> String.concat("")
};