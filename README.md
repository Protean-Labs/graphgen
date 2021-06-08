# graphgen
A subgraph generator for The Graph Network.

# Intro
This project aims to facilitate subgraph development by abstracting away simple but common subgraph writing patterns and automating the set up of peripherals.

# Installation

`npm i graphgen`

# Usage

Once the package has been downloaded and built in your subgraph folder, it is recommended to make a copy of you solidity interface files and move the copies to the folder. You should also remove any comments from them to prevent any interference with the parsers. Once this is done, you can start adding annotations according to the following specs:

## Annotations

- Source - 
Specifies the usual DataSources found in most subgraphs. It is possible to generate both unique sources and source templates which are usually used for contract factories.

- Handler
  - StoreCall
  - StoreEvent
  - UpdateField
  - NewEntity
  
- Fields - They specify the attributes of each entity that is tracked by the subgraph.

## CLI

`graphgen --graph_account --subgraph_name --path_to_folder`

## Example

## Generated Subgraph
- Subgraph Manifest
- Package.json
- Graphql Interface
- Typescript mappings and utils files
- abis

# Technologies
Graphgen uses parser generator technologies in order to leverage the information that resides in solidity interface files. It is written with the OCaml/ReasonML toolchain and compiled to native code.

# About us
We at Protean Labs focus on building various pieces of infrastructure and developer tooling around the Web3 ecosystem.