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
  
  [@printer (fmt, v) => Option.value(v, ~default=Yaml.(`Null)) |> Yaml.pp(fmt)]
  default: option(Yaml.value)
};

[@deriving show]
type gg_tag = 
  | GGSource(gg_source_params)
  | GGHandler(gg_handler_params)
  | GGField(gg_field_params)
;

[@deriving show]
type interface = {
  name: string,
  tags: list((gg_tag, interface_element))
};

[@deriving show]
type subgraph = list((gg_tag, interface));