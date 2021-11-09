module Ast: {
  /* ================================================================
  GraphGen tags
  ================================================================ */

  type instance = {
    address: string,
    startBlock: int
  };

  type gg_source_params = {
    name: option(string),
    instances: option(list(instance)),
    onInit: option(list(string))
  };

  type gg_handler_params = {
    name: option(string),
    actions: list(string)
  };

  type gg_field_params = {
    name: option(string),
    default: option(Yaml.value)
  };

  type gg_tag = 
    | GGSource(gg_source_params)
    | GGHandler(gg_handler_params)
    | GGField(gg_field_params)
  ;

  /* ================================================================
  Solidity
  ================================================================ */

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

  let string_of_typ: typ => string;

  type state_mutability = 
    | Pure 
    | View 
    | Nonpayable 
    | Payable
  ;

  let string_of_state_mut: state_mutability => string;

  type event_param = {
    typ: typ,
    indexed: bool,
    name: option(string)
  };

  type data_location = 
    | Memory
    | Storage
    | Calldata
  ;

  type fun_param = {
    typ: typ,
    data_loc: option(data_location),
    name: option(string)
  };

  type intf_element =
    | FunctionDef(string, list(fun_param), list(fun_param), option(gg_tag), state_mutability)
    | FallbackDef
    | ReceiveDef
    | StructDef
    | EnumDef
    | EventDef(string, list(event_param), option(gg_tag))
  ;

  type interface = {
    raw_name: string,
    elements: list(intf_element),
    tag: option(gg_tag)
  };

  type t = list(interface);

  /** [interface_of_name(ast, s)] returns [Some(intf)] where [intf] is 
      an interface that is part of the AST [ast] with name [s] or [None]
      if no such interface exists. */
  let interface_of_name: t => string => option(interface);
};

/** [parse(source)] returns [Ok(ast)] where [ast] is an AST 
    (asbtract syntax tree) of the solidity source code [source] 
    which includes GraphGen annotation data. If the source is 
    invalid, [Error(msg)] is returned instead. */
let parse: string => result(Ast.t, [> Rresult.R.msg]);