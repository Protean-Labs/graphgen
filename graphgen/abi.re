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

module SolTypes = {
  type t = 
  | UintT(int)
  | IntT(int)
  | AddressT
  | BoolT
  | Fixed(int,int)
  | Ufixed(int,int)
  | BytesT(int)
  | FunctionT
  | FArrayT(t)
  | DBytesT
  | String
  | DArray
  | AddressPayable
  | Contract
  | Enum
  | Struct
};



module Input = {
  [@deriving yojson]
  type t = {
    name: string,
    [@key type] typ: canonical types,
    internalType: string, 
    components: list(Input.t),
    indexed: bool
  }
};


module Function = {
  [@deriving yojson]
  type t = {
    [@key "type"] typ: string,
    name: string,
    inputs: list(Input.t),
    outputs: list(Output.t),
    stateMutability: Mutability.t
  }
};

module Event = {
  [@deriving yojson]
  type t = {
    [@key "type"] typ: string,
    name: string,
    inputs: list(Input.t),
    anonymous: bool
  }
};

type description =  [ `Event(Event.t) | `Function(Function) ]

type t = {
  contractName: string,
  sha1: string,
  [@key "type"] typ: "string",
  abis: list(description)
}