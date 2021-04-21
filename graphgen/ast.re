// type toplevel = 
//   | Pragma(pragma_token, expr)
//   | Import(import, expr)
//   | Interface(interface, expr)
//   | Library(library, expr)
//   | Function(function_t, expr)
//   | Tag(args, expr)
//   | Const(const, expr)
//   | Struct(struct_t, expr)
//   | Enum(enum, expr)
//   | End
// ;

// let parse_dir: list(string) => list((string, toplevel));

// let expand_imports = (files) = fun
//   | Import(import, expr) => List.assoc(import, files) |> replace_end_node(_, expr)
//   | 
// ;

// foo.sol
//  
// function f(uint x) returns (unint);
// 
// => toplevel

// bar.sol
// 
// import foo.sol
// 
// 
// => toplevel

// expand_imports 

[@deriving show]
type typ = 
  | AddressT
  | BoolT
  | StringT
  | FixedT
  | UfixedT
  | BytesT
  | FbytesT
  | IntT
  | UintT
  | ArrayT(typ)
;

[@deriving show]
type event_param = {
  typ: typ,
  indexed: bool,
  name: option(string)
};

let make_event_param = (typ, indexed, name) => {typ, indexed, name};

[@deriving show]
type interface_element =
  | FunctionDef
  | FallbackDef
  | ReceiveDef
  | StructDef
  | EnumDef
  | EventDef(string, list(event_param))
;

[@deriving show]
type gg_tag = 
  | GGSource(string)
  | GGHandler(string)
  | GGField(string)
;

[@deriving show]
type interface = {
  name: string,
  tags: list((gg_tag, interface_element))
};

let make_interface = (name, tags) => {name, tags};

[@deriving show]
type subgraph = list((gg_tag, interface));