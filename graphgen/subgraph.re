type event = {
  name: string,
  fields: list((string, Ast.typ))
};

type call = {
  name: string,
  inputs: list((string, Ast.typ)),
  outputs: list((string, Ast.typ))
};

type action = 
  | StoreEvent(event)
  | StoreCall(call)
  // | UpdateField(...)
  // | CreateEntity(...)
;

type handler = 
  | Event(event, list(action))
  | Call(call, list(action))
;

type contract = {
  name: string,
  fields: list((string, Ast.typ)),
  handlers: list(handler)
};

type t = list(contract);


let of_ast = (ast: Ast.t) => {
  let fmt_call = (name, inputs, outputs): call => {
    let inputs = 
      inputs
      |> List.map(({Ast.typ, data_loc, name}) => (Option.value(name, ~default="arg"), typ))
    ;

    let outputs = 
      outputs
      |> List.map(({Ast.typ, data_loc, name}) => (Option.value(name, ~default="arg"), typ))
    ;
    
    {name, inputs, outputs}
  };

  let fmt_event = (name, params: list(Ast.event_param)): event => {
    let fields = 
      params
      |> List.map(({typ, name}: Ast.event_param) => (Option.value(name, ~default="arg"), typ))
    ;

    {name, fields}
  };

  let fmt_event_handler_actions = (event, actions) => {
    let rec f = (actions, acc) => {
      switch (actions) {
      | [] => acc
      | ["StoreEvent", ...rest] => f(rest, [StoreEvent(event), ...acc])
      | [_, ...rest] => f(rest, acc)
      }
    };

    f(actions, [])
  };

  let fmt_call_handler_actions = (call, actions) => {
    let rec f = (actions, acc) => {
      switch (actions) {
      | [] => acc
      | ["StoreCall", ...rest] => f(rest, [StoreCall(call), ...acc])
      | [_, ...rest] => f(rest, acc)
      }
    };

    f(actions, [])
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
  
  let rec to_subgraph = (ast, acc): t => {
    switch (ast) {
    | [] => acc
    | [{Ast.name: n, elements, tag: Some(GGSource({name: None}))}, ...rest] => 
      to_subgraph(rest, [{name: n, fields: [], handlers: get_handlers(elements)}, ...acc])
    | [{Ast.elements, tag: Some(GGSource({name: Some(n)}))}, ...rest] => 
      to_subgraph(rest, [{name: n, fields: [], handlers: get_handlers(elements)}, ...acc])
    | [_, ...rest] => to_subgraph(rest, acc)
    }
  };

  to_subgraph(ast, [])
};