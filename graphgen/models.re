open Jingoo;

let logger = Easy_logging.Logging.make_logger("Generator.Models", Debug, [Cli(Debug)]);

let event_model = (event: Subgraph.event) => {
  logger#debug("Generating %s event model", event.name);
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
  logger#debug("Generating %s call model", call.name);
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
  logger#debug("Generating %s field model", name);
  // TODO: Use user specified default value
  Jg_types.Tobj([
    ("name", Tstr(name)),
    ("type", Tstr(Graphql.(convert_ast_type(typ) |> string_of_typ))),
    ("getter", Tstr(getter_name)),
    ("default", Tstr(Graphql.(convert_ast_type(typ) |> default_value)))
  ])
};

let contract_model = (subgraph, contract: Subgraph.Contract.t) => {
  logger#debug("Generating %s contract model", contract.name);
  Jg_types.Tobj([
    ("name", Tstr(contract.name)),
    ("events", Tlist(Subgraph.Contract.events(contract)
      |> List.map(event_model)
    )),
    ("calls", Tlist(Subgraph.Contract.calls(contract)
      |> List.map(call_model)
    )),
    ("fields", Tlist(List.map(field_model, contract.fields))),
    ("parentContract", Subgraph.parent_contract(subgraph, contract) 
      // |> fun | Some(parent) => contract_model(subgraph, parent) | None => Tnull
      |> fun | Some(parent) => Jg_types.Tobj([("name", Tstr(parent.name))]) | None => Tnull
    ),
    ("childContracts", Tlist(Subgraph.child_contracts(subgraph, contract)
      // |> (l) => {List.iter((child: Subgraph.contract) => logger#debug("%s child contract: %s", contract.name, child.name), l); l}
      // |> List.map(contract_model(subgraph))
      |> List.map((child: Subgraph.Contract.t) => Jg_types.Tobj([("name", Tstr(child.name))]))
    ))
  ])
};

let handler_model = (subgraph, contract, actions) => {
  let field_updates = actions
    |> List.filter_map(fun 
      | Subgraph.UpdateField(field_name) => Subgraph.Contract.field(contract, field_name)
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

let event_handler_model = (subgraph, contract, event: Subgraph.event, actions) => {
  let store = List.exists(fun | Subgraph.StoreEvent => true | _ => false, actions);
  logger#debug("Generating %s event handler model (store = %b)", event.name, store);
  let (field_updates, new_entities) = handler_model(subgraph, contract, actions);

  Jg_types.Tobj([
    ("event", event_model(event)),
    ("store", Tbool(store)),
    ("fieldUpdates", Tlist(field_updates)),
    ("newEntities", Tlist(new_entities))
  ])
};

let call_handler_model = (subgraph, contract, call: Subgraph.call, actions) => {
  let store = List.exists(fun | Subgraph.StoreCall => true | _ => false, actions);
  logger#debug("Generating %s call handler model (store = %b)", call.name, store);
  let (field_updates, new_entities) = handler_model(subgraph, contract, actions);

  Jg_types.Tobj([
    ("call_", call_model(call)),
    ("store", Tbool(store)),
    ("fieldUpdates", Tlist(field_updates)),
    ("newEntities", Tlist(new_entities))
  ])
};

let data_source_model = (subgraph, contract: Subgraph.Contract.t) => {
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

let template_model = (subgraph, contract: Subgraph.Contract.t) => {
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

let subgraph_model = (subgraph: Subgraph.t) =>
  Jg_types.Tobj([
    ("github_user", Tstr(subgraph.github_user)),
    ("subgraph_name", Tstr(subgraph.subgraph_name)),
    ("description", Tstr(subgraph.description)),
    ("dataSources", Tlist(subgraph.contracts
      |> List.filter_map((contract: Subgraph.Contract.t) => contract.instances != [] ? Some(data_source_model(subgraph, contract)) : None))),
    ("templates", Tlist(subgraph.contracts
      |> List.filter_map((contract: Subgraph.Contract.t) => contract.instances == [] ? Some(template_model(subgraph, contract)) : None)))
  ]);

let event_abi_model = (event: Subgraph.event) =>
  Jg_types.Tobj([
    ("name", Tstr(event.name)),
    ("fields", Tlist(event.fields
      |> List.map(((name, typ, indexed)) => Jg_types.Tobj([
        ("name", Tstr(name)),
        ("type", Tstr(Parsing.Ast.string_of_typ(typ))),
        ("indexed", Tbool(indexed))
      ]))
    ))
  ]);

let function_abi_model = (call: Subgraph.call) => 
  Jg_types.Tobj([
    ("name", Tstr(call.name)),
    ("stateMutability", Tstr(Parsing.Ast.string_of_state_mut(call.state_mutability))),
    ("inputs", Tlist(call.inputs
      |> List.map(((name, typ)) => Jg_types.Tobj([
        ("name", Tstr(name)),
        ("type", Tstr(Parsing.Ast.string_of_typ(typ)))
      ]))
    )),
    ("outputs", Tlist(call.outputs
      |> List.map(((name, typ)) => Jg_types.Tobj([
        ("name", Tstr(name)),
        ("type", Tstr(Parsing.Ast.string_of_typ(typ)))
      ]))
    ))
  ]);

let contract_abi_model = (contract: Subgraph.Contract.t) =>
  Jg_types.Tobj([
    ("events", Tlist(contract.all_events
      |> List.map(event_abi_model)
    )),
    ("functions", Tlist(contract.all_calls
      |> List.map(function_abi_model)
    ))
  ]);

let manifest_models = (subgraph) => subgraph_model(subgraph)
  |> (obj) => [("subgraph", obj)];

let schema_models = (subgraph) => subgraph_model(subgraph)
  |> (obj) => [("subgraph", obj)];

let package_json_models = (subgraph) => subgraph_model(subgraph)
  |> (obj) => [("subgraph", obj)];

let abi_models = (subgraph: Subgraph.t) => subgraph.contracts
  |> List.map((contract: Subgraph.Contract.t) => (contract.name, [("contract", contract_abi_model(contract))]));

let util_ts_models = (_) => [];

let data_sources_models = (subgraph: Subgraph.t) => subgraph.contracts
  |> List.filter_map((contract: Subgraph.Contract.t) => 
    if (contract.instances != []) {
      (
        contract.name,
        [
          ("dataSource", data_source_model(subgraph, contract)),
          ("subgraph", subgraph_model(subgraph))
        ]
      )
      |> Option.some
    }
    else None
  );

let templates_models = (subgraph: Subgraph.t) => subgraph.contracts
  |> List.filter_map((contract: Subgraph.Contract.t) => 
    if (contract.instances == []) {
      (
        contract.name,
        [
          ("template", data_source_model(subgraph, contract)),
          ("subgraph", subgraph_model(subgraph))
        ]
      )
      |> Option.some
    }
    else None
  );