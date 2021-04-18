type event = {
  name: string,
  fields: list((string, Solidity.typ))
};

type call = {
  name: string,
  inputs: list((string, Solidity.typ)),
  outputs: list((string, Solidity.typ))
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
  fields: list((string, Solidity.typ)),
  triggers: list(trigger)
};

type t = list(contract);