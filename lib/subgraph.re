open Rresult;

open Parsing;

let logger = Easy_logging.Logging.make_logger("Subgraph", Debug, [Cli(Debug)]);

module Event = {
  type t = {
    name: string,
    fields: list((string, Ast.typ, bool))
  };

  let signature = (e) => e.fields
    |> List.map(((_, typ, indexed)) => 
      Parsing.Ast.string_of_typ(typ)
      |> (s) => indexed ? [%string "indexed %{s}"] : s
    )
    |> String.concat(",")
    |> (fields) => [%string "%{e.name}(%{fields})"]
  ;

  let field = (e, name) => 
    List.find_opt(((name', _, _)) => name' == name, e.fields);

  let has_field = (e, name) => 
    List.exists(((name', _, _)) => name' == name, e.fields);

  let has_field_of_type = (c, name, typ) =>
    switch (field(c, name)) {
    | Some((_, t, _)) when t == typ => true
    | _ => false
    };
};

module Call = {
  type t = {
    name: string,
    state_mutability: Parsing.Ast.state_mutability,
    inputs: list((string, Parsing.Ast.typ)),
    outputs: list((string, Parsing.Ast.typ))
  };

  let signature = (c) => c.inputs
    |> List.map(((_, typ)) => Parsing.Ast.string_of_typ(typ))
    |> String.concat(",")
    |> (fields) => [%string "%{c.name}(%{fields})"]
  ;

  let field = (c, name) => 
    List.find_opt(((name', _)) => name' == name, c.inputs)  |> (maybe_field) =>
    switch (maybe_field) { 
    | None => List.find_opt(((name', _)) => name' == name, c.outputs) 
    | some_field => some_field
    };

  let has_field = (c, name) => 
    List.exists(((name', _)) => name == name', c.inputs) &&
    List.exists(((name', _)) => name == name', c.outputs);

  let has_field_of_type = (c, name, typ) =>
    switch (field(c, name)) {
    | Some((_, t)) when t == typ => true
    | _ => false
    };
};

type action = 
  | StoreEvent
  | StoreCall
  | UpdateField(string)                   // UpdateField(field_name)
  | NewEntity(string, string, string)     // NewEntity(name, raw_name, event_field)
;

type handler = 
  | Event(Event.t, list(action))
  | Call(Call.t, list(action))
;

module Contract = {
  type t = {
    name: string,
    network: string,
    instances: list((string, int)),
    raw_name: string,
    fields: list((string, Ast.typ, string)),
    handlers: list(handler),
    all_calls: list(Call.t),
    all_events: list(Event.t)
  };

  let make = (name, network, instances, raw_name, fields, handlers, all_calls, all_events) => 
    {name, network, instances, raw_name, fields, handlers, all_calls, all_events};

  let field = (contract, field_name) => contract.fields
    |> List.find_map(((name, typ, getter_name)) => 
      if (name == field_name) Some((typ, getter_name))
      else None
    );

  let new_entities = (contract) => contract.handlers
    |> List.filter_map(fun
      | Event(_, actions) => actions
        |> List.filter_map(fun | NewEntity(name, raw_name, field) => Some((name, raw_name, field)) | _ => None)
        |> Option.some
      | Call(_, actions) => actions
        |> List.filter_map(fun | NewEntity(name, raw_name, field) => Some((name, raw_name, field)) | _ => None)
        |> Option.some
    )
    |> List.flatten;

  let update_fields = (contract) => contract.handlers
    |> List.filter_map(fun
      | Event(_, actions) => actions
        |> List.filter_map(fun | UpdateField(name) => Some(name) | _ => None)
        |> Option.some
      | Call(_, actions) => actions
        |> List.filter_map(fun | UpdateField(name) => Some(name) | _ => None)
        |> Option.some
    )
    |> List.flatten;

  let calls = (~stored_only=false, contract) =>
    if (stored_only) {
      contract.handlers
      |> List.filter_map(fun 
        | Call(call, actions) => List.exists(fun | StoreCall => true | _ => false, actions) ? Some(call) : None
        | _ => None
      )
    }
    else {
      contract.handlers
      |> List.filter_map(fun | Call(call, _) => Some(call) | _ => None)
    };

  let events = (~stored_only=false, contract) => 
    if (stored_only) {
      contract.handlers
      |> List.filter_map(fun 
        | Event(event, actions) => List.exists(fun | StoreEvent => true | _ => false, actions) ? Some(event) : None
        | _ => None
      )
    }
    else {
      contract.handlers
      |> List.filter_map(fun | Event(event, _) => Some(event) | _ => None)
    };

  let has_field = (c, name) => List.exists(((name', _, _)) => name == name', c.fields);
};

type t = {
  github_user: string,
  subgraph_name: string,
  description: string,
  contracts: list(Contract.t)
};

let contract_of_name = (subgraph: t, name) => subgraph.contracts
  |> List.find_opt((contract: Contract.t) => contract.name == name)
;

let child_contracts = (subgraph: t, contract) => contract
  |> Contract.new_entities
  |> List.filter_map(((name, _, _)) => {
    logger#debug("%s child contract: %s", contract.name, name);
    contract_of_name(subgraph, name)
  })
;

let parent_contract = (subgraph, contract: Contract.t) => subgraph.contracts
  |> List.find_opt(contract' => Contract.new_entities(contract') 
    |> List.exists(((name, _, _)) => name == contract.name)
  )
;

let contract_related_entities = (subgraph, contract) => {
  [
    Contract.events(~stored_only=true, contract) |> List.map((event: Event.t) => event.name),
    Contract.calls(~stored_only=true, contract) |> List.map((call: Call.t) => call.name),
    parent_contract(subgraph, contract) |> (fun | Some(parent: Contract.t) => [parent.name] | None => []),
    child_contracts(subgraph, contract) |> List.map((child: Contract.t) => child.name)
  ]
  |> List.flatten
};

let contract_related_contracts = (subgraph, contract) => {
  [
    parent_contract(subgraph, contract) |> (fun | Some(parent: Contract.t) => [parent.name] | None => []),
    child_contracts(subgraph, contract) |> List.map((child: Contract.t) => child.name)
  ]
  |> List.flatten
};

module Builder = {
  let fmt_call = (name, inputs, outputs, state_mutability): Call.t => {
    let inputs = inputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ));

    let outputs = outputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ));

    {name, state_mutability, inputs, outputs}
  };

  let fmt_event = (name, params: list(Ast.event_param)): Event.t => {
    let fields = params
      |> List.map(({typ, name, indexed}: Ast.event_param) => (Option.value(name, ~default="arg"), typ, indexed));

    {name, fields}
  };

  let fmt_event_handler_actions = (ast, _, actions) => {
    let rec f = (actions, acc) => {
      switch (actions) {
      | [] => acc
      | [["StoreEvent"], ...rest] => f(rest, [StoreEvent, ...acc])
      | [["UpdateField", field_name], ...rest] => f(rest, [UpdateField(field_name), ...acc])
      | [["NewEntity", source, "from", event_field], ...rest] => 
        Ast.interface_of_name(ast, source)
        |> (fun
          | Some(intf) => f(rest, [NewEntity(source, intf.raw_name, event_field), ...acc])
          | None => f(rest, acc)
        )
      | [_, ...rest] => f(rest, acc)
      }
    };

    f(List.map(String.split_on_char(' '), actions), [])
  };

  let fmt_call_handler_actions = (ast, _, actions) => {
    let rec f = (actions, acc) => {
      switch (actions) {
      | [] => acc
      | [["StoreCall"], ...rest] => f(rest, [StoreCall, ...acc])
      | [["UpdateField", field_name], ...rest] => f(rest, [UpdateField(field_name), ...acc])
      | [["NewEntity", source, "from", event_field], ...rest] => 
        Ast.interface_of_name(ast, source)
        |> (fun
          | Some(intf) => f(rest, [NewEntity(source, intf.raw_name, event_field), ...acc])
          | None => {
            logger#warning("NewEntity %s from %s: no source named %s found! Skipping", source, event_field, source);
            f(rest, acc)
          }
        )
      | [_, ...rest] => f(rest, acc)
      }
    };

    f(List.map(String.split_on_char(' '), actions), [])
  };

  let get_handlers = (full_ast, intf_elements) => {
    open Ast;
    let rec f = (intf_elements, acc) => {
      switch (intf_elements) {
      | [] => acc
      | [FunctionDef(_, inputs, outputs, Some(GGHandler({name: Some(n), actions})), state_mut), ...rest] 
      | [FunctionDef(n, inputs, outputs, Some(GGHandler({name: None, actions})), state_mut), ...rest] => 
        let call = fmt_call(n, inputs, outputs, state_mut);
        f(rest, [Call(call, fmt_call_handler_actions(full_ast, call, actions)), ...acc])
      | [EventDef(_, fields, Some(GGHandler({name: Some(n), actions}))), ...rest] 
      | [EventDef(n, fields, Some(GGHandler({name: None, actions}))), ...rest] =>
        let event = fmt_event(n, fields);
        f(rest, [Event(event, fmt_event_handler_actions(full_ast, event, actions)), ...acc])
      | [_, ...rest] => f(rest, acc)
      };
    };

    f(intf_elements, [])
  };

  let all_calls = Parsing.Ast.(List.filter_map(fun
    | FunctionDef(name, inputs, outputs, _, state_mut) => fmt_call(name, inputs, outputs, state_mut) |> Option.some
    | _ => None
  ));

  let all_events = Parsing.Ast.(List.filter_map(fun
    | EventDef(name, fields, _) => fmt_event(name, fields) |> Option.some
    | _ => None
  ));

  let make = (~github_user="PLACEHOLDER", ~subgraph_name="PLACEHOLDER", ~desc="PLACEHOLDER", full_ast: Ast.t) => {
    let get_fields = (intf_elements) => {
      open Ast;
      let rec f = (intf_elements, acc) => {
        switch (intf_elements) {
        | [] => acc
        | [FunctionDef(getter_name, [], [output], Some(GGField({name, _})), _), ...rest] => 
          f(rest, [(Option.value(name, ~default=getter_name), output.typ, getter_name), ...acc])        
        | [_, ...rest] => f(rest, acc)
        };
      };

      f(intf_elements, [])
    };

    let fmt_address = (address) => {
      let regex = Str.regexp(".*\\(0x[A-Fa-f0-9]+\\).*");
      Str.replace_first(regex, "\\1", address)
    };

    let fmt_instances = (instances) => instances
      |> Option.value(~default=[]) 
      |> List.map((instance: Ast.instance) => (fmt_address(instance.address), instance.startBlock));
    
    // TODO: Set network field based on tags
    let rec to_subgraph = (ast: list(Ast.interface), acc) => {
      switch (ast) {
      | [] => acc

      | [{raw_name, elements, tag: Some(GGSource({name: None, instances, _}))}, ...rest] => 
        fmt_instances(instances)            |> (instances) => 
        get_fields(elements)                |> (fields) =>
        get_handlers(full_ast, elements)    |> (handlers) =>
        all_calls(elements)                 |> (all_calls) => 
        all_events(elements)                |> (all_events) => 
        Contract.make(raw_name, "mainnet", instances, raw_name, fields, handlers, all_calls, all_events)
        |> List.cons(_, acc)
        |> to_subgraph(rest)

      | [{raw_name, elements, tag: Some(GGSource({name: Some(name), instances, _}))}, ...rest] => 
        fmt_instances(instances)            |> (instances) => 
        get_fields(elements)                |> (fields) =>
        get_handlers(full_ast, elements)    |> (handlers) =>
        all_calls(elements)                 |> (all_calls) => 
        all_events(elements)                |> (all_events) => 
        Contract.make(name, "mainnet", instances, raw_name, fields, handlers, all_calls, all_events)
        |> List.cons(_, acc)
        |> to_subgraph(rest)

      | [_, ...rest] => to_subgraph(rest, acc)
      }
    };

    {
      github_user,
      subgraph_name,
      description: desc,
      contracts: to_subgraph(full_ast, [])
    }
  }

  let validate = (sg) => {
    /** [val_field_updates(c)] returns [Ok()] if all UpdateField actions are
        valid for the contract [c] and [Error(msg)] otherwise. */
    let val_field_updates = (c) => 
      List.fold_left((acc, field) => 
        acc >>= () => 
        Contract.has_field(c, field) ? R.ok() : R.error_msg([%string "%{c.name}: UpdateField: Entity %{c.name} has no field named %{field}"]), 
      R.ok(), Contract.update_fields(c));

    /** [val_event_new_entity_field(c, event, new_entities)] returns [Ok()] 
        if all [NewEntity] actions [new_entities] are valid for 
        the contract [c] and event [event] and returns [Error(msg)] otherwise. 
        More specifically, given an action [NewEntity X of Y], this function 
        checks if the event [event] has a field [Y]. */
    let val_event_new_entity_field = (c: Contract.t, event, new_entities) => 
      List.fold_left((acc, (_, field)) => 
        acc >>= () =>
        Event.has_field(event, field) ?
        switch (Event.field(event, field)) { 
        | Some((_, Ast.AddressT, _)) => R.ok() 
        | _ => R.error_msg([%string "%{c.name}.%{event.name}: NewEntity: Field %{field} is not of type address"])
        } : 
        R.error_msg([%string "%{c.name}.%{event.name}: NewEntity: Event %{event.name} has no field %{field}"]),
        R.ok(), new_entities
      );

    /** [val_call_new_entity_field(c, call, new_entities)] returns [Ok()] 
        if all [NewEntity] actions [new_entities] are valid for 
        the contract [c] and call [call] and returns [Error(msg)] otherwise. 
        More specifically, given an action [NewEntity X of Y], this function 
        checks if the call [call] has a field [Y]. */
    let val_call_new_entity_field = (c: Contract.t, call, new_entities) => 
      List.fold_left((acc, (_, field)) => 
        acc >>= () =>
        Call.has_field(call, field) ?
        switch (Call.field(call, field)) {
        | Some((_, Ast.AddressT)) => R.ok()
        | _ => R.error_msg([%string "%{c.name}.%{call.name}: NewEntity: Field %{field} is not of type address"])
        } :
        R.error_msg([%string "%{c.name}.%{call.name}: NewEntity: Call %{call.name} has no field %{field}"]),
        R.ok(), new_entities
      );

    /** [val_event_new_entity(c, event, new_entities)] returns [Ok()] 
        if all [NewEntity] actions [new_entities] are valid for 
        the contract [c] and event [event] and returns [Error(msg)] otherwise. 
        More specifically, given an action [NewEntity X of Y], this function 
        checks if the entity [X] exists in the subgraph. */
    let val_event_new_entity = (c: Contract.t, event: Event.t, new_entities) => 
      List.fold_left((acc, (name, _)) => 
        acc >>= () =>
        contract_of_name(sg, name) == None ?
        R.error_msg([%string "%{c.name}.%{event.name}: NewEntity: Entity %{name} does not exist"]) :
        R.ok(),
        R.ok(), new_entities
      );

    /** [val_call_new_entity(c, call, new_entities)] returns [Ok()] 
        if all [NewEntity] actions [new_entities] are valid for 
        the contract [c] and call [call] and returns [Error(msg)] otherwise. 
        More specifically, given an action [NewEntity X of Y], this function 
        checks if the entity [X] exists in the subgraph. */
    let val_call_new_entity = (c: Contract.t, call: Call.t, new_entities) => 
      List.fold_left((acc, (name, _)) => 
        acc >>= () =>
        contract_of_name(sg, name) == None ? 
        R.error_msg([%string "%{c.name}.%{call.name}: NewEntity: Entity %{name} does not exist"]) : 
        R.ok(),
        R.ok(), new_entities
      );

    /** [val_new_entity_handler(c, handler)] returns [Ok()] if all [NewEntity] 
        actions that are part of the handler [handler] of contract [c] are valid 
        and [Error(msg)] otherwise. */
    let val_new_entity_handler = (c, handler) =>
      switch (handler) {
      | Event(event, actions) => 
        List.filter_map(fun | NewEntity(name, _, field) => Some((name, field)) | _ => None, actions) |> (new_entities) =>
        val_event_new_entity_field(c, event, new_entities) >>= () =>
        val_event_new_entity(c, event, new_entities)
      | Call(call, actions) =>
        List.filter_map(fun | NewEntity(name, _, field) => Some((name, field)) | _ => None, actions) |> (new_entities) =>
        val_call_new_entity_field(c, call, new_entities)    >>= () =>
        val_call_new_entity(c, call, new_entities)
      };

    /** [val_new_entities(c)] returns [Ok()] if all [NewEntity] 
        actions that are triggered by contract [c] are valid. */
    let val_new_entities = (c) => 
      List.fold_left((acc, handler) => 
        acc     >>= () => 
        val_new_entity_handler(c, handler),
      R.ok(), c.handlers);


    List.fold_left((acc, c) => acc >>= () => val_field_updates(c), R.ok(), sg.contracts)  >>= () =>
    List.fold_left((acc, c) => acc >>= () => val_new_entities(c), R.ok(), sg.contracts)
  };
};
