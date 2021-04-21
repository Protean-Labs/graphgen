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

type trigger = 
  | Event(event, list(action))
  | Call(call, list(action))
;

type contract = {
  name: string,
  fields: list((string, Ast.typ)),
  triggers: list(trigger)
};

type t = list(contract);