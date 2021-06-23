open Ast;
open Subgraph;

type graphql_type =
  | BigInt
  | BigDecimal
  | String
  | Int
  | Float
  | Bytes
  | Boolean
  | List(graphql_type)
;

let rec graphqltype_of_soltype = fun
  | BytesT => Bytes
  | FbytesT(_) => Bytes
  | UintT(n) when n <= 32 => Int
  | UintT(_) => BigInt
  | IntT(n) when n <= 32 => Int
  | IntT(_) => BigInt
  | StringT => String
  | BoolT => Boolean
  | AddressT => Bytes
  | FixedT => Float
  | UfixedT => Float
  | ArrayT(t') => List(graphqltype_of_soltype(t'))
;

let rec string_of_graphqltype = fun
  | BigInt => "BigInt"
  | BigDecimal => "BigDecimal"
  | String => "String"
  | Int => "Int"
  | Float => "Float"
  | Bytes => "Bytes"
  | Boolean => "Boolean"
  | List(t') => Format.sprintf("[%s!]", string_of_graphqltype(t'))
;


let string_of_soltype_to_graphqltype = (t') => t'|> graphqltype_of_soltype |> string_of_graphqltype;

let transaction_entity = 
  // Do not touch string format!
"type Transaction @entity {
  id: ID!   # tx hash
  txIndex: Int!
  from: Bytes!
  to: Bytes
  blockNumber: BigInt!
  blockTimestamp: BigInt!
  events: [Event!]! @derivedFrom(field: \"tx\")
  calls: [Call!]! @derivedFrom(field: \"tx\")
}";

let event_interface = 
  // Do not touch string format!
"interface Event {
  id: ID!   # CONTRACT_ADDR-PER_EVENT_COUNTER
  logIndex: BigInt!
  tx: Transaction!
}";

let call_interface = 
  // Do not touch string format!
"interface Call {
  id: ID!   # CONTRACT_ADDR-PER_CALL_COUNTER
  tx: Transaction!
}";

let entity_of_event = (contract: Subgraph.contract, {name, fields}: Subgraph.event) => {
  let fields = 
    fields
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, string_of_soltype_to_graphqltype(typ)))
    |> String.concat("\n  ")
  ;

  Format.sprintf(
  // Do not touch string format!
"type %s implements Event @entity {
  id: ID!   # CONTRACT_ADDR-PER_EVENT_COUNTER
  logIndex: BigInt!
  tx: Transaction!

  %s

  %s: %s!
}", 
    name, 
    fields,
    String.uncapitalize_ascii(contract.name), 
    contract.name 
  );
};

let entity_of_call = (contract: Subgraph.contract, {name, inputs, outputs}: Subgraph.call) => {
  let inputs = 
    inputs
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, string_of_soltype_to_graphqltype(typ)))
    |> String.concat("\n  ")
  ;

  let outputs = 
    outputs
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, string_of_soltype_to_graphqltype(typ)))
    |> String.concat("\n  ")
  ;

  Format.sprintf(
  // Do not touch string format!
"type %s @entity implements Call {
  id: ID!   # CONTRACT_ADDR-PER_CALL_COUNTER
  tx: Transaction!

  %s
  %s

  %s: %s!
}", 
    name, 
    inputs, 
    outputs,
    String.uncapitalize_ascii(contract.name), 
    contract.name
  );
};

let contract_fields_of_event = (contract: Subgraph.contract, event: Subgraph.event) => {
  // Do not touch string format!
  Format.sprintf(
"  num%ss: BigInt!
  %ss: [%s!]! @derivedFrom(field: \"%s\")
  latest%s: %s", 
    event.name, 
    String.uncapitalize_ascii(event.name), 
    event.name, 
    String.uncapitalize_ascii(contract.name), 
    event.name, 
    event.name
  )
};

let contract_fields_of_call = (contract: Subgraph.contract, call: Subgraph.call) => {
  Format.sprintf(
  // Do not touch string format!
"  num%ss: BigInt!
  %ss: [%s!]! @derivedFrom(field: \"%s\")
  latest%s: %s", 
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
    |> List.map(((name, typ)) => Format.sprintf("%s: %s!", name, string_of_soltype_to_graphqltype(typ)))
    |> String.concat("\n  ")
  ;

  let events_contract_fields = 
    contract.handlers
    |> List.filter_map((handler) =>
      // Get events for which there is a handler and a StoreEvent action
      switch (handler) {
      | Event(event, actions) when List.exists(fun | StoreEvent => true | _ => false, actions) => Some(event)
      | _ => None
      }
    )
    // Generate contract fields related to event
    |> List.map(contract_fields_of_event(contract))
  ;

  let calls_contract_fields = 
    contract.handlers
    |> List.filter_map((handler) => 
      // Get calls for which there is a handler and a StoreCall action
      switch (handler) {
      | Call(call, actions) when List.exists(fun | StoreCall => true | _ => false, actions) => Some(call)
      | _ => None
      }
    )
    // Generate contract fields related to call
    |> List.map(contract_fields_of_call(contract))

    // |> List.map(fun | Call(_, actions) => actions | _ => [])
    // |> List.flatten
    // |> List.filter_map(fun | StoreCall(call) => Some(call) | _ => None)
  ;

  let all_fields = 
    [
      [contract_fields],
      events_contract_fields,
      calls_contract_fields
    ]
    |> List.flatten
    |> String.concat("\n\n")
  ;

  Format.sprintf(
  // Do not touch string format!
"type %s @entity {
  id: ID!   # Address

  %s
}", 
    contract.name, 
    all_fields
  );
};

let of_subgraph = (subgraph: Subgraph.t) => {
  let event_entities = 
    subgraph
    |> List.map((contract) => 
      contract.handlers
      // Get events that have handlers and are stored
      |> List.filter_map((handler) => 
        switch (handler) {
        | Event(event, actions) when List.exists(fun | StoreEvent => true | _ => false, actions) => Some(event)
        | _ => None
        }
      ) 
      // Generate event entities
      |> List.map(entity_of_event(contract))
    )
    |> List.flatten
  ;

  let call_entities = 
    subgraph
    |> List.map((contract) => 
      contract.handlers
      // Get calls that have handlers and are stored
      |> List.filter_map((handler) => 
        switch (handler) {
        | Call(call, actions) when List.exists(fun | StoreCall => true | _ => false, actions) => Some(call)
        | _ => None
        }
      ) 
      // Generate call entities
      |> List.map(entity_of_call(contract))
    )
    |> List.flatten


    // subgraph
    // |> List.map(contract => 
    //   contract.handlers 
    //   |> List.map(t => (contract, t))
    // )
    // |> List.flatten
    // |> List.map(fun 
    //   | (contract, Call(_, actions)) => 
    //     actions 
    //     |> List.map(action => (contract, action)) 
    //   | _ => []
    // )
    // |> List.flatten
    // |> List.filter_map(fun 
    //   | (contract, StoreCall(call)) => Some((contract, call)) 
    //   | _ => None
    // )
    // |> List.map(((contract, call)) => entity_of_call(contract, call))
  ;

  let contract_entities = 
    subgraph
    |> List.map(entity_of_contract)
  ;

  [
    [transaction_entity],
    event_entities == [] ? [] : [event_interface],
    call_entities == [] ? [] : [call_interface],
    event_entities,
    call_entities,
    contract_entities
  ]
  |> List.flatten
  |> String.concat("\n\n")
};