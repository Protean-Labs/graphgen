open Jingoo;

let event_signature = (event: Subgraph.event) => {
  event.fields
  |> List.map(((_, typ, indexed)) => 
    Graphql.(convert_ast_type(typ) |> string_of_typ)
    |> (s) => indexed ? [%string "indexed %{s}"] : s
  )
  |> String.concat(",")
  |> (fields) => [%string "%{event.name}(%{fields})"]
};

let event_model = (event: Subgraph.event) => {
  open Jg_types;
  Tobj([
    ("name", Tstr(event.name)),
    ("signature", Tstr(event_signature(event))),
    ("fields", Tlist(
      event.fields
      |> List.map(((name, typ, _)) => Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    ))
  ])  
};

let call_signature = (call: Subgraph.call) => {
  call.inputs
  |> List.map(((_, typ)) => Graphql.(convert_ast_type(typ) |> string_of_typ))
  |> String.concat(",")
  |> (fields) => [%string "%{call.name}(%{fields})"]
};

let call_model = (call: Subgraph.call) => {
  open Jg_types;
  Tobj([
    ("name", Tstr(call.name)),
    ("signature", Tstr(call_signature(call))),
    ("inputs", Tlist(
      call.inputs
      |> List.map(((name, typ)) => Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    )),
    ("outputs", Tlist(
      call.outputs
      |> List.map(((name, typ)) => Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    ))
  ])
};

let field_model = ((name, typ, getter_name)) => {
  open Jg_types;
  // TODO: Use user specified default value
  Tobj([
    ("name", Tstr(name)),
    ("type", Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))),
    ("getter", Tstr(getter_name)),
    ("default", Tstr(Graphql.(convert_ast_type(typ) |> default_value)))
  ])
};

let rec contract_model = (subgraph, contract: Subgraph.contract) => {
  open Jg_types;

  Tobj([
    ("name", Tstr(contract.name)),
    ("events", Tlist(Subgraph.contract_events(contract)
      |> List.map(event_model)
    )),
    ("calls", Tlist(Subgraph.contract_calls(contract)
      |> List.map(call_model)
    )),
    ("fields", Tlist(List.map(field_model, contract.fields))),
    ("parentContract", Subgraph.parent_contract(subgraph, contract) 
      |> fun | Some(contract) => contract_model(subgraph, contract) | None => Tnull
    ),
    ("childContracts", Tlist(Subgraph.child_contracts(subgraph, contract)
      |> List.map(contract_model(subgraph))
    ))
  ])
};

let event_handler_model = (subgraph, contract, event, actions) => {
  let store = List.exists(fun | Subgraph.StoreEvent => true | _ => false, actions);

  // TODO: new entities contracts
  let new_entities = actions
    |> List.filter_map(fun 
      | Subgraph.NewEntity(name, _, field) => Subgraph.contract_of_name(subgraph, name)
        |> Option.map((contract) => (field, contract))
      | _ => None
    );

  let field_updates = actions
    |> List.filter_map(fun 
      | Subgraph.UpdateField(field_name) => Subgraph.field_of_contract(contract, field_name)
        |> Option.map(((typ, getter_name)) => (field_name, typ, getter_name))
      | _ => None
    );

  open Jg_types;
  Tobj([
    ("event", event_model(event)),
    ("store", Tbool(store)),
    ("fieldUpdates", Tlist(List.map(field_model, field_updates))),
    ("newEntities", Tlist(List.map(((field, contract)) => Tset([Tstr(field), contract_model(subgraph, contract)]), new_entities)))
  ])
};

// let manifest_model = (subgraph) => {

// };