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

let event_handler_model = (event, actions) => {
  let store = List.exists(fun | Subgraph.StoreEvent => true | _ => false, actions);

  // TODO: new entities contracts
  let new_entities = actions
    |> List.filter_map(fun | Subgraph.NewEntity(name, raw_name, field) =>  | _ => None);

  let field_updates = actions
    |> List.filter_map(fun | Subgraph.UpdateField(field_name) => Some(field_name) | _ => None);
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

// let manifest_model = (subgraph) => {

// };