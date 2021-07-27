let logger = Easy_logging.Logging.make_logger("Ast", Debug, [Cli(Debug)]);

/* ================================================================
GraphGen tags
================================================================ */

[@deriving (show, yaml)]
type instance = {
  address: string,
  startBlock: int
};

[@deriving (show, yaml)]
type gg_source_params = {
  name: option(string),
  instances: option(list(instance)),
  onInit: option(list(string))
};

[@deriving (show, yaml)]
type gg_handler_params = {
  name: option(string),
  actions: list(string)
};

[@deriving (show, yaml)]
type gg_field_params = {
  name: option(string),
  
  [@printer (fmt, v) => Option.value(v, ~default=`Null) |> Yaml.pp(fmt)]
  default: option(Yaml.value)
};

[@deriving show]
type gg_tag = 
  | GGSource(gg_source_params)
  | GGHandler(gg_handler_params)
  | GGField(gg_field_params)
;

/* ================================================================
Solidity
================================================================ */

[@deriving show]
type typ = 
  | AddressT
  | BoolT
  | StringT
  | FixedT
  | UfixedT
  | BytesT
  | FbytesT(int)
  | IntT(int)
  | UintT(int)
  | ArrayT(typ)
;

let rec string_of_typ = fun
  | AddressT => "address"
  | BoolT => "bool"
  | StringT => "string"
  | FixedT => "float"
  | UfixedT => "float"
  | BytesT => "bytes"
  | FbytesT(_) => "float"
  | IntT(n) => [%string "int%{string_of_int n}"]
  | UintT(n) => [%string "uint%{string_of_int n}"]
  | ArrayT(typ) => [%string "%{string_of_typ typ}[]"]
;

[@deriving show]
type event_param = {
  typ: typ,
  indexed: bool,
  name: option(string)
};

[@deriving show]
type data_location = 
  | Memory
  | Storage
  | Calldata
;

[@deriving show]
type fun_param = {
  typ: typ,
  data_loc: option(data_location),
  name: option(string)
};

[@deriving show]
type intf_element =
  | FunctionDef(string, list(fun_param), list(fun_param), option(gg_tag))
  | FallbackDef
  | ReceiveDef
  | StructDef
  | EnumDef
  | EventDef(string, list(event_param), option(gg_tag))
;

[@deriving show]
type interface = {
  raw_name: string,
  elements: list(intf_element),
  tag: option(gg_tag)
};

[@deriving show]
type t = list(interface);

let interface_of_raw_name = (ast: t, target) => {
  ast
  |> List.find_opt(({raw_name, _}) => raw_name == target)
};

let interface_of_name = (ast: t, target) => {
  ast
  |> List.find_opt(({tag, _}) => 
    switch (tag) {
    | Some(GGSource({name: Some(n), _})) => n == target
    | _ => false
    }
  )
};


// let rec tcheck = (subgraph) => {
//   // First pass scanning for object names and types (i.e.: Source, Event, Call and Field)
//   let rec scan = (subgraph, objects) => {
//     let rec scan_intf_tags = (tags, objects) => {
//       switch (tags) {
//       | [] => objects
//       | [(GGHandler({name: Some(n)}), EventDef(_)), ...rest] | [(GGHandler({name: None}), EventDef(n, _)), ...rest] => 
//         scan_intf_tags(rest, [(n, `Event), ...objects])
//       | [(GGHandler({name: Some(n)}), FunctionDef(_)), ...rest] | [(GGHandler({name: None}), FunctionDef(n, _, _)), ...rest] => 
//         scan_intf_tags(rest, [(n, `Call), ...objects])
//       | [(GGField({name: Some(n)}), FunctionDef(_)), ...rest] | [(GGField({name: None}), FunctionDef(n, _, _)), ...rest] => 
//         scan_intf_tags(rest, [(n, `Field), ...objects])
//       | [_, ...rest] => scan_intf_tags(rest, objects)
//       }
//     };

//     switch (subgraph) {
//     | [] => objects
//     | [(GGSource({name: Some(n)}), {tags}), ...rest] => 
//       scan(rest, [(n, `Source), ...scan_intf_tags(tags, objects)])
//     | [(GGSource({name: None}), {name as intf_name, tags}), ...rest] => 
//       scan(rest, [(intf_name, `Source), ...scan_intf_tags(tags, objects)])
//     | [_, ...rest] => scan(rest, objects)
//     }
//   }
  
//   let rec tcheck_intf = (intf) => {
//     switch (intf) {
//     | [] => Ok()
//     | [(GGHandler(_), FunctionDef(_)), ...rest] | [(GGField(_), EventDef(_)), ...rest] => tcheck_intf(rest)
//     | [(GGField(_), FunctionDef(_)), ...rest] => tcheck_intf(rest)
//     | [_, ...rest] => tcheck_intf(rest)
//     }
//   };

//   let objects = scan(subgraph);

//   switch (subgraph) {
//   | [] => Ok()
//   | [(GGSource(params), intf), ...rest] => 

//   }
// };


let tcheck = (ast) => {
  // First pass scanning for object names and types (i.e.: Source, Event, Call and Field)
  let rec scan = (ast, tenv) => {
    let rec scan_intf = (intf_elements, tenv) => {
      switch (intf_elements) {
      | [] => tenv
      | [FunctionDef(_, _, _, Some(GGHandler({name: Some(n), _}))), ...rest] 
      | [FunctionDef(n, _, _, Some(GGHandler({name: None, _}))), ...rest] => 
        scan_intf(rest, [(n, `Call), ...tenv])
      | [EventDef(_, _, Some(GGHandler({name: Some(n), _}))), ...rest] 
      | [EventDef(n, _, Some(GGHandler({name: None, _}))), ...rest] => 
        scan_intf(rest, [(n, `Event), ...tenv])
      | [FunctionDef(_, _, _, Some(GGField({name: Some(n), _}))), ...rest] 
      | [FunctionDef(n, _, _, Some(GGField({name: None, _}))), ...rest] => 
        scan_intf(rest, [(n, `Field), ...tenv])
      | [_, ...rest] => scan_intf(rest, tenv)
      }
    };

    switch (ast) {
    | [] => tenv
    | [{raw_name: n, elements, tag: Some(GGSource({name: None, _}))}, ...rest] => 
      scan(rest, scan_intf(elements, [(n, `Source), ...tenv]))
    | [{elements, tag: Some(GGSource({name: Some(n), _})), _}, ...rest] => 
      scan(rest, scan_intf(elements, [(n, `Source), ...tenv]))
    | [{elements, tag: None, _}, ...rest] => 
      scan(rest, scan_intf(elements, tenv))
    | [_, ...rest] => scan(rest, tenv)
    }
  };

  let _ = scan(ast, []);
};