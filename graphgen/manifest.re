open Subgraph;
open Ast;

let rec field_to_string = fun
  | (_, AddressT) => "address"
  | (_, UintT(n)) => "uint" ++ string_of_int(n)
  | (_, IntT(n)) => "int" ++ string_of_int(n)
  | (_, StringT) => "string"
  | (_, BoolT) => "bool"
  | (_, FixedT) => "fixed"
  | (_, UfixedT) => "ufixed"
  | (_, BytesT) => "bytes"
  | (_, FbytesT(_)) => ""
  | (_, ArrayT(typ)) => Format.sprintf("%s[]", field_to_string(("", typ)))
;
module CallHandler = {
  [@deriving yaml]
  type t = {
    [@key "function"] func: string,
    handler: string
  };

  let make = (call: call) => {
    let call_fields = List.map(field_to_string, call.inputs) |> String.concat(", ");

    {
      func: Format.sprintf("%s(%s)", call.name, call_fields),
      handler:Format.sprintf("handle%s", call.name)
    }
  };
};

module EventHandler = {
  [@deriving yaml]
  type t = {
    event: string,
    handler: string
  };

  let make = (event:event) => {
    let event_fields = List.map(field_to_string, event.fields) |> String.concat(",");
    {
      event: Format.sprintf("%s(%s)", event.name, event_fields),
      handler:Format.sprintf("handle%s", event.name)
    }
  };
};

module Abi = {
  [@deriving yaml]
  type t = {
    name: string,
    file: string
  };

};

module Mapping = {
  [@deriving yaml]
  type t = {
    kind: string,
    apiVersion: string,
    language: string,
    file: string,
    entities: list(string),
    abis: list(Abi.t),
    eventHandlers: list(EventHandler.t),
    callHandlers: list(CallHandler.t)
  };

  let make = (contract: Subgraph.contract) => {
    kind: "ethereum/events",
    apiVersion: "0.0.1",
    language: "wasm/assemblyscript",
    file:Format.sprintf("./src/mappings/%s.ts", contract.name),
    entities: ["entitiy1", "entity2"],
    abis:[],
    eventHandlers: {
      contract.handlers 
      |> List.filter_map(fun 
      | Event(event, actions) => Some(EventHandler.make(event))
      | _ => None
      )
    },
    callHandlers: {
      contract.handlers 
      |> List.filter_map(fun 
      | Call(call, actions) => Some(CallHandler.make(call))
      | _ => None
      )
    }
  }
};

module Source = {
  [@deriving yaml]
  type t = {
    address: option(string),
    abi: string,
    startBlock: option(int)
  };

  let make = (~address=?,~startBlock=?,abi) => {
    address: address,
    abi: abi,
    startBlock: startBlock
  }
};


module DataSource = {
  [@deriving yaml]
  type t = {
    kind: string,
    name: string,
    network: string,
    source: Source.t,
    mapping: Mapping.t
  }

  let make = (contract: Subgraph.contract) => {
    kind: "ethereum/contract",
    name: contract.name,
    network: "mainnet",
    source: Source.make(~address="placeholder",~startBlock=100000,contract.name),
    mapping: Mapping.make(contract)
  }
};

module Template = {
  [@deriving yaml]
  type t = {
    kind: string,
    name: string,
    network: string,
    source: Source.t,
    mapping: Mapping.t
  };

  let make = (contract: Subgraph.contract) => {
    kind: "ethereum/contract",
    name: contract.name,
    network: "mainnet",
    source: Source.make("IERC20"),
    mapping: Mapping.make(contract)
  };
};

module Schema = {
  [@deriving yaml]
  type t = {
    file: string
  };

  let make = (file_name) => {file: file_name}
  
};

[@deriving yaml]
type t = {
  specVersion: string,
  description: string,
  repository: string,
  schema: Schema.t,
  dataSources: list(DataSource.t),
  templates: list(Template.t)
};

let make = (subg: Subgraph.t) => {
  {
    specVersion: "0.0.1",
    description: "Auto generated subgraph",
    repository: "place holder",
    schema: Schema.make("./schema.graphql"),
    dataSources: List.map(DataSource.make,subg),
    templates: List.map(Template.make,subg)
  }
};

let to_file = (manifest, path) => {
  manifest 
  |> to_yaml
  |> Yaml_unix.to_file(Fpath.v([%string "%{path}/subgraph.yaml"]));
}