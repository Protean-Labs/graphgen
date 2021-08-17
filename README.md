<h1 align="center">GraphGen - A subgraph generator for The Graph Network</h1>

<p align="center">
  <img src="https://github.com/cvauclair/graphgen/blob/main/assets/img/graphgen.png?raw=true" alt="graphgen-logo" width="120px" height="120px"/>
  <br>
  <i>Graphgen aims to facilitate subgraph development by abstracting away simple but common subgraph writing patterns
    <br>  and automating the set up of peripherals.</i>
  <br>
</p>

<p align="center">
  <strong> Designed by Protean Labs </strong> <br>
  <a href="https://www.protean.so/graphgen"><strong>www.protean.so</strong></a>
  <br>
</p>

<p align="center">
  <a href="CONTRIBUTING.md">Contributing Guidelines</a>
  ·
  <a href="https://github.com/protean-labs/graphgen/issues">Submit an Issue</a>
  ·
  <a href="https://medium.com/protean-labs">Blog</a>
  <br>
  <br>
</p>

<p align="center">
  <a href="https://github.com/protean-labs/graphgen/blob/HEAD/LICENSE">
    <img src="https://img.shields.io/badge/license-Apache 2.0-blue.svg" alt="GraphGen is released under the MIT license." />
  </a>
  <a href="https://github.com/protean-labs/graphgen/actions/workflows/build.yml">
    <img src="https://img.shields.io/github/workflow/status/protean-labs/graphgen/Build/main" alt="Current CircleCI build status." />
  </a>
  <a href="https://www.npmjs.com/@protean-labs/graphgen">
    <img src="https://img.shields.io/npm/v/@protean-labs/graphgen.svg?logo=npm&logoColor=fff&label=NPM+package&color=limegreen" alt="GraphGen on npm" />
  </a>
  <br>
  <a href="https://www.npmjs.com/@protean-labs/graphgen">
    <img src="https://img.shields.io/npm/dt/@protean-labs/graphgen" alt="Graphgen downloads on npm" />
  </a>
    <a href="https://discord.gg/protean-labs">
    <img src="https://img.shields.io/discord/789917050922336357.svg?logo=discord&logoColor=fff&label=Discord&color=7389d8" alt="Discord conversation"/>
  </a>
  <a href="https://twitter.com/intent/follow?screen_name=proteancrypto">
    <img src="https://img.shields.io/twitter/follow/proteancrypto?style=social" alt="Follow @proteancrypto"/>
  </a>
</p>

# Getting started

Graphgen is released on the npm.js registry and Github package registry. 

You can install Graphgen globally with:

```
npm i -g @protean-lab/graphgen
```

Basic usage is to call on an already annotated .sol contract file. If installed globally, use npx to call the executable if your $PATH is not already set.

```
npx graphgen [CONTRACT_FILE].sol
``` 

# Usage

```
NAME
       graphgen - Generate a subgraph from annotated solidity interfaces

SYNOPSIS
       graphgen [OPTION]... SOURCE

ARGUMENTS
       SOURCE (required)
           Solidity interface file or directory containing multiple interface
           files annotated with graphgen tags

OPTIONS
       -d VAL, --description=VAL (absent=PLACEHOLDER)
           Subgraph description

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       -n VAL, --name=VAL (absent=PLACEHOLDER)
           The name of the subgraph

       -o VAL, --output-dir=VAL (absent=subgraph)
           The name of the output directory to which the subgraph will be
           generated

       -u VAL, --user=VAL (absent=PLACEHOLDER)
           The Graph Github user

       -v, --verbose
           Verbose output
```

# Getting started
GraphGen takes as inputs solidity files containing annotated interfaces and outputs a working subgraph. The annotations indicate to GraphGen which contracts should be indexed (as `dataSources` or `templates`), which events and calls to listen to and what to do when one happens, etc.

Given a solidity file containing annotated interfaces, or a directory containing multiple such files, the following command is used to generate the subgraph:
`npx graphgen -o OUTPUT_DIR FILE_OR_DIR`.

Provided the annotations and interfaces are valid, GraphGen will generate a subgraph (located in in `OUTPUT_DIR/`) with the following files:
- `subgraph.yaml`: Subgraph manifest
- `package.json`: Package.json
- `schema.graphql`: Graphql schema
- `src/mappings/X.ts`: Typescript mappings for each indexed contracts `X`
- `src/utils.ts`: Utility functions used in generated mappings
- `abis/X.json`: Solidity ABIs for each indexed subgraph `X`

**IMPORTANT**: GraphGen does not include any functionality from the `graph-cli`. One must therefore run `npm run codegen` and `npm run build` after generating the source files with GraphGen.

# Annotations
GraphGen supports three kinds of annotations, each with their specific parameters:
- `@gg:source`: Defines a `dataSource` or `template` (depending on parameters). Must precede an interface definition.
- `@gg:field`: Defines an entity field with an optional default value. Must precede a `view` or `pure` function definition.
- `@gg:handler`: Defines an event (or call) handler, as well as the logic to be executed when the handler is triggered. Must precede an event (or call) definition.

**IMPORTANT**: All GraphGen annotations must directly precede their associated code block and be written inside a comment block delimited by `/*` and `*/`.

## `@gg:source`
### Parameters
- `name`: `STRING` - (optional) Name used for the entity type representing this interface and all other references to it. Defaults to interface name.
- `instances`: `[INSTANCE]` - (optional) A list of instances. Defaults to empty list, indicating that this interface maps to a subgraph template.

Where `INSTANCE` has two parameters:
- `address`: `STRING` - The address of the contract implementing the interface.
- `startBlock`: `INTEGER` - The block at which to start the indexing of this contract.
  

### Example
Template:
```solidity
/* @gg:source
  name: MySuperContract 
*/
interface MyContract {
  ...
}
```

DataSource:
```solidity
/* @gg:source
  instances:
    - address: '0xaa7fb1c8ce6f18d4fd4aabb61a2193d4d441c54f'
      startBlock: 8335916 
*/
interface MyContract {
  ...
}
```

## `@gg:field`
### Parameters
- `name`: `STRING` - (optional) Name used as the name of the entity field. Defaults to the name of the function.
- `default`: `STRING` - (optional) Default value for the field, used in case of errors. Defaults to predefined values for each type (e.g.: 0 for BigInt, "" for String).

### Example
```solidity
/* @gg:source
  name: MySuperContract 
*/
interface MyContract {
  /* @gg:field */
  function totalSupply() external view returns (uint);

  /* @gg:field
    name: speedOfLight
  */
  function c() external pure returns (uint);
}
```

## `@gg:handler`:
### Parameters
- `name`: `STRING` - (optional) Name used for the entity type representing this event (or call) and all other references to it. Defaults to event (or call) name.
- `actions`: `[STRING]` - List of handler actions, see below for more information.

### Handler actions
Actions are commands (encoded as simple strings) that tell GraphGen what kind of logic should be executed when a certain event (or call) handler is triggered. GraphGen currently supports four distinct actions:
- `StoreEvent`: When the handler is associated with an event, this action command tells GraphGen to: 1) Create a new entity type for the event; 2) Store each of those events; and 3) Add edges between the emitter contract entity and the event entities.
- `StoreCall`: Same as `StoreEvent`, but for storing function calls. (Note: `StoreEvent` and `StoreCall` might get merged into a single action command in the future).
- `UpdateField X`: This action command tells GraphGen that whenever the handler is triggered, the field `X` (of the emitter contract entity) should be updated. The field `X` must be defined using a `@gg:field` annotation on the same interface.
- `NewEntity X of Y`: This action command tells GraphGen that whenever the handler is triggered, a new template contract entity of type `X` should be created, using the field `Y` of the event (or call) as the address and id of this new entity. `X` must be defined using a `@gg:source` annotation. 

### Example
```solidity
/* @gg:source
  name: MySuperContract 
*/
interface MyContract {
  /* @gg:field */
  function totalSupply() external view returns (uint);

  /* @gg:handler 
    actions:
      - StoreEvent
      - UpdateField totalSupply
  */
  event Mint(uint amount);

  /* @gg:handler 
    name: AdminChange
    actions:
      - StoreCall
  */
  function changeAdmin(address newAdmin) external returns (bool);

  /* @gg:handler 
    actions:
      - NewEntity ERC20 from token
  */
  event NewToken(address indexed token);
}
```

## Example
See `example/README.md`

# Technologies

Graphgen uses parser generator technologies in order to leverage the information that resides in solidity interface files. It is written with the OCaml/ReasonML toolchain and compiled to native OCaml code.

# About us

We at Protean Labs focus on building various pieces of infrastructure and developer tooling around the Web3 ecosystem. You can reach out to us through our multiple social accounts and at <a href="https://www.protean.so/graphgen"><strong>www.protean.so</strong></a>.
