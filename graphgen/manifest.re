open Subgraph;

let rec field_to_string = Ast.(fun
  | (_, AddressT) => "address"
  | (_, UintT(n)) => "uint" ++ string_of_int(n)
  | (_, IntT(n)) => "int" ++ string_of_int(n)
  | (_, StringT) => "string"
  | (_, BoolT) => "bool"
  | (_, FixedT) => "fixed"
  | (_, UfixedT) => "ufixed"
  | (_, BytesT) => "bytes"
  | (_, FbytesT(_)) => ""
  | (_, ArrayT(typ)) => Format.sprintf("%s[]", field_to_string(("", typ))))
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
  // requires multiple abis?

  let make = ((name, raw_name)) => {
    name: name,
    file: "./abis/" ++ raw_name
  }

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

  let make = (contract: Subgraph.contract, names) => {
    kind: "ethereum/events",
    apiVersion: "0.0.1",
    language: "wasm/assemblyscript",
    file: Format.sprintf("./src/mappings/%s.ts", contract.name),
    entities: ["entitiy1", "entity2"],
    abis: names |> List.map(Abi.make),
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
    address: string,
    abi: string,
    startBlock: int
  };

  let make = (instance: (string,int),abi_name) => {
  let (address, start_block) = instance;
  
  {
    address: address,
    abi: abi_name,
    startBlock: start_block
  }
  }
};


module DataSource = {

  [@deriving yaml]
  type t = {
    kind: string,
    name: string,
    network: string,
    source: list(Source.t),
    mapping: Mapping.t
  };

  let make = (contract: Subgraph.contract, names) => {
    let abi_name = contract.name
    let instances = contract.instances;
    {
      kind: "ethereum/contract",
      name: contract.name,
      network: "mainnet",
      source: List.map(Source.make(_, abi_name),instances),
      mapping: Mapping.make(contract, names)
    };
  };
};

module Template = {
  [@deriving yaml]
  type source = {abi: string};

  [@deriving yaml]
  type t = {
    kind: string,
    name: string,
    network: string,
    source: source,
    mapping: Mapping.t
  };

  let make = (contract: Subgraph.contract, names) => {
    let instances = contract.instances;
    let abi_name = contract.name;
    {
      kind: "ethereum/contract",
      name: contract.name,
      network: "mainnet",
      source: {abi: abi_name},
      mapping: Mapping.make(contract, names)
    };
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

let get_contract_names = (subg) => List.map((contract => (contract.name, contract.raw_name)), subg);

let make = (subg: Subgraph.t) => {
  let names = subg |> get_contract_names;
  {
    specVersion: "0.0.1",
    description: "Auto generated subgraph",
    repository: "place holder",
    schema: Schema.make("./schema.graphql"),
    dataSources: List.filter_map
    (
      (contract=> 
      List.length(contract.instances) == 0 ? 
      None : 
      Some(DataSource.make(contract, names))),
      subg
    ),
    templates: List.filter_map
    (
      (contract =>
      List.length(contract.instances) == 0 ? 
      Some(Template.make(contract, names)) : 
      None),
      subg
    ),
  };

};

let to_file = (manifest, path) => {
  manifest 
  |> to_yaml
  |> Yaml_unix.to_file(Fpath.v([%string "%{path}/subgraph.yaml"]));
}