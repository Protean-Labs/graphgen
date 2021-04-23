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
type interface_element =
  | FunctionDef(string, list(fun_param), list(fun_param))
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

[@deriving show]
type subgraph = list((gg_tag, interface));