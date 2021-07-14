open Parsing;

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

type contract = {
  name: string,
  instances: list((string, int)),
  raw_name: string,
  fields: list((string, Ast.typ, string)),
  handlers: list(handler)
};

type t = list(contract);

let of_ast = (ast: Ast.t) => {
  let fmt_call = (name, inputs, outputs): call => {
    let inputs = 
      inputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ))
    ;

    let outputs = 
      outputs
      |> List.map((Ast.{typ, name, _}) => (Option.value(name, ~default="arg"), typ))
    ;
    
    {name, inputs, outputs}
  };

  let fmt_event = (name, params: list(Ast.event_param)): event => {
    let fields = 
      params
      |> List.map(({typ, name, indexed}: Ast.event_param) => (Option.value(name, ~default="arg"), typ, indexed))
    ;

    {name, fields}
  };

  let fmt_event_handler_actions = (_, actions) => {
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

  let fmt_call_handler_actions = (_, actions) => {
    let rec f = (actions, acc) => {
      switch (actions) {
      | [] => acc
      | [["StoreCall"], ...rest] => f(rest, [StoreCall, ...acc])
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

  let get_handlers = (intf_elements) => {
    open Ast;
    let rec f = (intf_elements, acc) => {
      switch (intf_elements) {
      | [] => acc
      | [FunctionDef(_, inputs, outputs, Some(GGHandler({name: Some(n), actions}))), ...rest] 
      | [FunctionDef(n, inputs, outputs, Some(GGHandler({name: None, actions}))), ...rest] => 
        let call = fmt_call(n, inputs, outputs);
        f(rest, [Call(call, fmt_call_handler_actions(call, actions)), ...acc])
      | [EventDef(_, fields, Some(GGHandler({name: Some(n), actions}))), ...rest] 
      | [EventDef(n, fields, Some(GGHandler({name: None, actions}))), ...rest] =>
        let event = fmt_event(n, fields);
        f(rest, [Event(event, fmt_event_handler_actions(event, actions)), ...acc])
      | [_, ...rest] => f(rest, acc)
      };
    };

    f(intf_elements, [])
  };

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
  
  let rec to_subgraph = (ast: list(Ast.interface), acc): t => {
    switch (ast) {
    | [] => acc
    | [{raw_name, elements, tag: Some(GGSource({name: None, instances, _}))}, ...rest] => 
      let instances = instances |> Option.value(~default=[]) |> List.map((instance: Ast.instance) => (instance.address, instance.startBlock));
      to_subgraph(rest, [{raw_name, name: raw_name, instances, fields: get_fields(elements), handlers: get_handlers(elements)}, ...acc])
    | [{raw_name, elements, tag: Some(GGSource({name: Some(n), instances, _}))}, ...rest] => 
      let instances = instances |> Option.value(~default=[]) |> List.map((instance: Ast.instance) => (instance.address, instance.startBlock));
      to_subgraph(rest, [{raw_name, name: n, instances, fields: get_fields(elements), handlers: get_handlers(elements)}, ...acc])
    | [_, ...rest] => to_subgraph(rest, acc)
    }
  };

  to_subgraph(ast, [])
};