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
  | FbytesT(n) => [%string "bytes%{string_of_int n}"]
  | IntT(n) => [%string "int%{string_of_int n}"]
  | UintT(n) => [%string "uint%{string_of_int n}"]
  | ArrayT(typ) => [%string "%{string_of_typ typ}[]"]
;

[@deriving show]
type state_mutability = 
  | Pure 
  | View 
  | Nonpayable 
  | Payable
;

let string_of_state_mut = fun
  | Pure => "pure"
  | View => "view"
  | Nonpayable => "nonpayable"
  | Payable => "payable"
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
  | FunctionDef(string, list(fun_param), list(fun_param), option(gg_tag), state_mutability)
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