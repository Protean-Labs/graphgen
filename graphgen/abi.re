module FunctionType = {
  [@deriving yojson]
  type t = 
  | Function 
  | Constructor 
  | Receive 
  | Fallback
  ;

  let to_yojson = fun
    | Function => `String("function")
    | Constructor => `String("constructor")
    | Receive => `String("receive")
    | Fallback => `String("fallback")
    ;
};

module Mutability = {
  [@deriving yojson]
  type t = 
  | Pure 
  | View 
  | Nonpayable 
  | Payable
  ;

  let to_yojson = fun
    | Pure => `String("pure")
    | View => `String("view")
    | Nonpayable => `String("nonpayable")
    | Payable => `String("payable")
  ;

};

let rec sol_type_to_string = fun
  | AddressT => "address"
  | BoolT => "bool"
  | StringT => "string"
  | FixedT => ""
  | UfixedT => ""
  | BytesT => "bytes"
  | FbytesT => ""
  | IntT(n) => Format.sprintf("int%s", n)
  | UintT(n) => Format.sprintf("int%s", n)
  | ArrayT(typ) => Format.sprintf("%s[]", sol_type_to_string(typ))
;

module EventParam = {
  [@deriving yojson]
  type t = {
    name: string,
    [@key "type"] typ: string,
    indexed: bool
  }

  let make = (input) => {
    typ: input.typ |> sol_type_to_string,
    name: {
      input.name |> fun
        | Some(s) => s
        | None => ""
    },
    indexed: false,
  }
};



module Input = {
  [@deriving yojson]
  type t = {
    name: string,
    [@key "type"] typ: string
  }

  let make = (input) => {
    typ: input.typ |> sol_type_to_string,
    name: {
      input.name |> fun
        | Some(s) => s
        | None => ""
    }
  };
};


module Function = {
  [@deriving yojson]
  type t = {
    [@key "type"] typ: string,
    name: string,
    inputs: list(Input.t),
    outputs: list(Output.t),
    stateMutability: Mutability.t
  };

  let make = (name, inputs, outputs, tags) => {
    typ: "function",
    name: name,
    inputs: List.map(Input.make, inputs),
    outputs: List.map(Input.make, outputs),
    stateMutability: View
  };

};

module Event = {
  [@deriving yojson]
  type t = {
    [@key "type"] typ: string,
    name: string,
    inputs: list(Input.t),
    anonymous: bool
  };

  let make = (name, inputs, tags) => {
    typ: "event",
    name: name,
    inputs: List.map(EventParam.make, inputs),
    anonymous: false
  }
};

[@deriving yojson]
type description =  [ `Event(Event.t) | `Function(Function.t) ];
let description_to_yojson = fun
  | `Event(event) => event
  | `Function(func) => func
  ;

[@deriving yojson]
type t = {
  contractName: string,
  sha1: string,
  source: string,
  [@key "type"] typ: string,
  abis: list(description)
};

let abi_of_element = (el: Ast.intf_element) => {
  switch el {
    | FunctionDef(name,inputs,outputs,tags) => `Function(Function.make(name, inputs, outputs, tags))
    | EventDef(name,params,tags) => `Event(Event.make(name, inputs, tags))
    | _ => None
  };
}

let make = (interface: Ast.interface) => {
  contractName: interface.name,
  sha1:"",
  source: "",
  typ: "interface",
  abis: List.filter_map(abi_of_element,interface.elements)
};

let to_file = (abi) => {
  abi
  |> to_yojson
  |> Yojson.Safe.to_file(Format.sprintf("../abis/%s.json", abi.contractName))
};