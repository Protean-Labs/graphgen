open Jingoo;

let event_model = (event: Subgraph.event) => {
  Jg_types.Tobj([
    ("name", Tstr(event.name)),
    ("signature", Tstr(Subgraph.event_signature(event))),
    ("fields", Tlist(
      event.fields
      |> List.map(((name, typ, _)) => Jg_types.Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    ))
  ])  
};

let call_model = (call: Subgraph.call) => {
  Jg_types.Tobj([
    ("name", Tstr(call.name)),
    ("signature", Tstr(Subgraph.call_signature(call))),
    ("inputs", Tlist(
      call.inputs
      |> List.map(((name, typ)) => Jg_types.Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    )),
    ("outputs", Tlist(
      call.outputs
      |> List.map(((name, typ)) => Jg_types.Tset([Tstr(name), Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))]))
    ))
  ])
};

let field_model = ((name, typ, getter_name)) => {
  // TODO: Use user specified default value
  Jg_types.Tobj([
    ("name", Tstr(name)),
    ("type", Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))),
    ("getter", Tstr(getter_name)),
    ("default", Tstr(Graphql.(convert_ast_type(typ) |> default_value)))
  ])
};

let rec contract_model = (subgraph, contract: Subgraph.contract) => {
  Jg_types.Tobj([
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

let handler_model = (subgraph, contract, actions) => {
  let field_updates = actions
    |> List.filter_map(fun 
      | Subgraph.UpdateField(field_name) => Subgraph.field_of_contract(contract, field_name)
        |> Option.map(((typ, getter_name)) => (field_name, typ, getter_name))
      | _ => None
    )
    |> List.map(field_model);

  let new_entities = actions
    |> List.filter_map(fun 
      | Subgraph.NewEntity(name, _, field) => Subgraph.contract_of_name(subgraph, name)
        |> Option.map((contract) => (field, contract))
      | _ => None
    )
    |> List.map(((field, contract)) => Jg_types.Tset([Tstr(field), contract_model(subgraph, contract)]));

  (field_updates, new_entities)
}

let event_handler_model = (subgraph, contract, event, actions) => {
  let store = List.exists(fun | Subgraph.StoreEvent => true | _ => false, actions);
  let (field_updates, new_entities) = handler_model(subgraph, contract, actions);

  Jg_types.Tobj([
    ("event", event_model(event)),
    ("store", Tbool(store)),
    ("fieldUpdates", Tlist(field_updates)),
    ("newEntities", Tlist(new_entities))
  ])
};

let call_handler_model = (subgraph, contract, call, actions) => {
  let store = List.exists(fun | Subgraph.StoreCall => true | _ => false, actions);
  let (field_updates, new_entities) = handler_model(subgraph, contract, actions);

  Jg_types.Tobj([
    ("call_", call_model(call)),
    ("store", Tbool(store)),
    ("fieldUpdates", Tlist(field_updates)),
    ("newEntities", Tlist(new_entities))
  ])
};

let data_source_model = (subgraph, contract: Subgraph.contract) => {
  Jg_types.Tobj([
    ("name", Tstr(contract.name)),
    ("network", Tstr(contract.network)),
    ("instances", Tlist(contract.instances
      |> List.map(((address, start_block)) => Jg_types.Tset([Tstr(address), Tint(start_block)]))
    )),
    // ("address", Tstr(address)),
    // ("startBlock", Tstr(start_block)),
    ("contract", contract_model(subgraph, contract)),
    // TODO: Related contracts
    ("relatedContracts", Tlist(Subgraph.contract_related_contracts(subgraph, contract) 
      |> List.map(name => Jg_types.Tobj([("name", Tstr(name))]))
    )),
    // TODO: Related entities
    ("relatedEntities", Tlist(Subgraph.contract_related_entities(subgraph, contract) 
      |> List.map(name => Jg_types.Tobj([("name", Tstr(name))]))
    )),
    ("eventHandlers", Tlist(contract.handlers 
      |> List.filter_map(fun | Subgraph.Event(event, actions) => Some(event_handler_model(subgraph, contract, event, actions)) | _ => None)
    )),
    ("callHandlers", Tlist(contract.handlers 
      |> List.filter_map(fun | Subgraph.Call(call, actions) => Some(call_handler_model(subgraph, contract, call, actions)) | _ => None)
    ))
  ])
};

let template_model = (subgraph, contract: Subgraph.contract) => {
  Jg_types.Tobj([
    ("name", Tstr(contract.name)),
    ("network", Tstr(contract.network)),
    ("contract", contract_model(subgraph, contract)),
    // TODO: Related contracts
    ("relatedContracts", Tlist(Subgraph.contract_related_contracts(subgraph, contract) 
      |> List.map(name => Jg_types.Tobj([("name", Tstr(name))]))
    )),
    // TODO: Related entities
    ("relatedEntities", Tlist(Subgraph.contract_related_entities(subgraph, contract) 
      |> List.map(name => Jg_types.Tobj([("name", Tstr(name))]))
    )),
    ("eventHandlers", Tlist(contract.handlers 
      |> List.filter_map(fun | Subgraph.Event(event, actions) => Some(event_handler_model(subgraph, contract, event, actions)) | _ => None)
    )),
    ("callHandlers", Tlist(contract.handlers 
      |> List.filter_map(fun | Subgraph.Call(call, actions) => Some(call_handler_model(subgraph, contract, call, actions)) | _ => None)
    ))
  ])
};

let subgraph_model = (subgraph) => {
  Jg_types.Tobj([
    ("dataSources", Tlist(subgraph
      |> List.filter_map((contract: Subgraph.contract) => contract.instances != [] ? Some(data_source_model(subgraph, contract)) : None))),
    ("templates", Tlist(subgraph
      |> List.filter_map((contract: Subgraph.contract) => contract.instances == [] ? Some(template_model(subgraph, contract)) : None)))
  ])
};

let manifest_model = (subgraph) => subgraph_model(subgraph)
  |> (obj) => [("subgraph", obj)];

let schema_model = (subgraph) => subgraph_model(subgraph)
  |> (obj) => [("subgraph", obj)];

let data_sources_model = (subgraph) => subgraph
  |> List.filter_map((contract: Subgraph.contract) => 
    if (contract.instances != []) {
      [
        ("dataSource", data_source_model(subgraph, contract)),
        ("subgraph", subgraph_model(subgraph))
      ]
      |> Option.some
    }
    else None
  );

let templates_model = (subgraph) => subgraph
  |> List.filter_map((contract: Subgraph.contract) => 
    if (contract.instances == []) {
      [
        ("template", data_source_model(subgraph, contract)),
        ("subgraph", subgraph_model(subgraph))
      ]
      |> Option.some
    }
    else None
  );