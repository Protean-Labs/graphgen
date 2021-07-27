open Parsing;

let logger = Easy_logging.Logging.make_logger("Subgraph", Debug, [Cli(Debug)]);

type event = {
  name: string,
  fields: list((string, Ast.typ, bool))
};

type call = {
  name: string,
  inputs: list((string, Ast.typ)),
  outputs: list((string, Ast.typ))
};

type action = 
  | StoreEvent
  | StoreCall
  | UpdateField(string)                   // UpdateField(field_name)
  | NewEntity(string, string, string)     // NewEntity(name, raw_name, event_field)
;

type handler = 
  | Event(event, list(action))
  | Call(call, list(action))
;

module Contract = {
  type t = {
    name: string,
    network: string,
    instances: list((string, int)),
    raw_name: string,
    fields: list((string, Ast.typ, string)),
    handlers: list(handler)
  };

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
      | _ => None
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
};

type t = list(Contract.t);

let contract_of_name = (subgraph: t, name) => subgraph
  |> List.find_opt((contract: Contract.t) => contract.name == name)
;

let child_contracts = (subgraph: t, contract) => contract
  |> Contract.new_entities
  |> List.filter_map(((name, _, _)) => {
    logger#debug("%s child contract: %s", contract.name, name);
    contract_of_name(subgraph, name)
  })
;

let parent_contract = (subgraph, contract: Contract.t) => subgraph
  |> List.find_opt(contract' => Contract.new_entities(contract') 
    |> List.exists(((name, _, _)) => name == contract.name)
  )
;

let event_signature = (event: event) => event.fields
  |> List.map(((_, typ, indexed)) => 
    Parsing.Ast.string_of_typ(typ)
    |> (s) => indexed ? [%string "indexed %{s}"] : s
  )
  |> String.concat(",")
  |> (fields) => [%string "%{event.name}(%{fields})"]
;

let call_signature = (call: call) => call.inputs
  |> List.map(((_, typ)) => Parsing.Ast.string_of_typ(typ))
  |> String.concat(",")
  |> (fields) => [%string "%{call.name}(%{fields})"]
;

let contract_related_entities = (subgraph, contract) => {
  [
    Contract.events(~stored_only=true, contract) |> List.map((event: event) => event.name),
    Contract.calls(~stored_only=true, contract) |> List.map((call: call) => call.name),
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
  let fmt_call = (name, inputs, outputs): call => {
    let inputs = inputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ));

    let outputs = outputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ));
    
    {name, inputs, outputs}
  };

  let fmt_event = (name, params: list(Ast.event_param)): event => {
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
      | [FunctionDef(_, inputs, outputs, Some(GGHandler({name: Some(n), actions}))), ...rest] 
      | [FunctionDef(n, inputs, outputs, Some(GGHandler({name: None, actions}))), ...rest] => 
        let call = fmt_call(n, inputs, outputs);
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

  let make = (full_ast: Ast.t) => {

    let get_fields = (intf_elements) => {
      open Ast;
      let rec f = (intf_elements, acc) => {
        switch (intf_elements) {
        | [] => acc
        | [FunctionDef(getter_name, [], [output], Some(GGField({name, _}))), ...rest] => 
          f(rest, [(Option.value(name, ~default=getter_name), output.typ, getter_name), ...acc])        
        | [_, ...rest] => f(rest, acc)
        };
      };

      f(intf_elements, [])
    };
    
    // TODO: Set network field based on tags
    let rec to_subgraph = (ast: list(Ast.interface), acc): t => {
      switch (ast) {
      | [] => acc
      | [{raw_name, elements, tag: Some(GGSource({name: None, instances, _}))}, ...rest] => 
        let instances = instances |> Option.value(~default=[]) |> List.map((instance: Ast.instance) => (instance.address, instance.startBlock));
        to_subgraph(rest, [{raw_name, name: raw_name, network: "mainnet", instances, fields: get_fields(elements), handlers: get_handlers(full_ast, elements)}, ...acc])
      | [{raw_name, elements, tag: Some(GGSource({name: Some(n), instances, _}))}, ...rest] => 
        let instances = instances |> Option.value(~default=[]) |> List.map((instance: Ast.instance) => (instance.address, instance.startBlock));
        to_subgraph(rest, [{raw_name, name: n, network: "mainnet", instances, fields: get_fields(elements), handlers: get_handlers(full_ast, elements)}, ...acc])
      | [_, ...rest] => to_subgraph(rest, acc)
      }
    };

    to_subgraph(full_ast, [])
  };
}
