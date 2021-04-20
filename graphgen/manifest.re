open Subgraph
open Yaml

module CallHandler = {
  [@deriving yaml]
  type t = {
    [@key "function"] func: string,
    handler: string
  };

  let make = ((call, actions)) => {
    let call_fields = List.map(Solidty.Typ.to_string, call.fields) |> String.concat(",");
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

  let make = ((event, actions)) => {
    let event_fields = List.map(Solidty.Typ.to_string, event.fields) |> String.concat(",");
    {
      event: Format.sprintf("%s()", event.name),
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
      contract.triggers 
      |> List.filter_map(fun 
      | Event(event, actions) => Some(EventHandler.make(event, actions))
      | _ => None
      )
    },
    callHandlers: {
      contract.triggers 
      |> List.filter_map(fun 
      | Call(call, actions) => Some(CallHandler.make(call, actions))
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

  let make = () => {
    address: "placeholder",
    abi: "placeholder",
    startBlock: 100000
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
    source: Source.make(),
    mapping: Mapping.make(contract)
  }

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
  dataSources: list(DataSource.t)
};

let make = (subg: Subgraph.t) => {
  {
    specVersion: "0.0.1",
    description: "Auto generated subgraph",
    repository: "place holder",
    schema: Schema.make("./schema.graphql"),
    dataSources: List.map(DataSource.make,subg)
  }
}

let to_file = (manifest: Manifest.t) => {
  manifest 
  |> Manifest.to_yaml
  |> Unix.to_file(_, "subgraph.yaml")
}