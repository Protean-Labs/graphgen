[@ocaml.text {|
  The [Subgraph] module contains the data structures used to represent a complete 
  subgraph as well as useful functions to extract data from the subgraph 
  data structure. The {!Contract} module provides functions specific to {!Contract.t} 
  types. The {!Builder} module provides functions to build the subgraph representation 
  from the abstract syntax tree (see {!Parsing.Ast}) produced by the parser.
|}]

/** Type of smart contract events. */
type event = {
  name: string,
  fields: list((string, Parsing.Ast.typ, bool))
};

/** [event_signature(e)] returns a {!string} containing the signature of the function
    represented by the {!event} [e] to be used in the subgraph manifest. */
let event_signature: event => string;

/** Type of smart contract function calls. */
type call = {
  name: string,
  state_mutability: Parsing.Ast.state_mutability,
  inputs: list((string, Parsing.Ast.typ)),
  outputs: list((string, Parsing.Ast.typ))
};

/** [call_signature(c)] returns a {!string} containing the signature of the function
    represented by the {!call} [c] to be used in the subgraph manifest. */
let call_signature: call => string;

/** The type of a handler action. 
    - [StoreEvent] indicates that the event associated with this action in the 
      corresponding {!handler} should be stored as part of the subgraph. Ignore 
      if the action is present in a [Call] {!handler}. 
    - [StoreCall] similar to [StoreEvent], but for function calls. 
    - [UpdateField(field_name)] indicates that the field with [field_name] of the 
      contract which triggered the handler should be updated. This action is 
      ignored if [field_name] does not match any valid contract field. 
    - [NewEntity(name, raw_name, field_name)] indicates that a new entity of type 
      [name] should be created from the [field_name] variable of the event of call
      which triggered this action. Note that the [field_name] must refer to an event 
      or call parameter of type [address]. For example, the Uniswap V2 [Factory] 
      contract emits [PairCreated] events whit a field [pair] which contains the 
      address of the newly created [Pair] contract. The action 
      [NewEntity("Pair", "IUniswapV2Pair", "pair")] would indicate that a new entity of 
      type [Pair] should be created from the event's [pair] field. */
type action = 
  | StoreEvent
  | StoreCall
  | UpdateField(string)                   
  | NewEntity(string, string, string)
;

/** The type of a subgraph handler. Each (valid) [@gg:handler] comment tag in the 
    interface files creates a {!handler} in the subgrpah representation. 
    - [Event(e, actions)] represents an event handler, where [e] is a value of type 
      {!event} representing the event in question and [actions] is a list of {!action}
      which indicates what to do when the event is emitted. 
    - [Call(c, actions)] same as [Event] but for a {!call} instead of an {!event}. */
type handler = 
  | Event(event, list(action))
  | Call(call, list(action))
;

module Contract: {
  type t = {
    name: string,
    network: string,
    instances: list((string, int)),
    raw_name: string,
    fields: list((string, Parsing.Ast.typ, string)),
    handlers: list(handler),
    all_calls: list(call),
    all_events: list(event)
  };

  /** [field(contract, field_name)] returns a tuple [Some((typ, getter))] 
      where [typ] is the type of the contract field and [getter] is the 
      name of the contract getter function used to query the field's value.
      If the field does not exist, [None] is returned. */
  let field: t => string => option((Parsing.Ast.typ, string));

  /** [new_entities(contract)] returns a list of tuples [(name, raw_name, field)] 
      where [name] is the name of the entity as specified in the tags, [raw_name] 
      is the original name of the entity and [field] is the name of the event field 
      (or call field), which must be and address, from which a new entity is created. */
  let new_entities: t => list((string, string, string));

  /** [calls(~stored_only, c)] returns the list of calls of the contract 
      [c] that can be indexed as part of the subgraph. If [stored_only] is 
      [true] (default: [false]), then only the calls that are stored as per the 
      comment tags are returned. */
  let calls: (~stored_only:bool=?) => t => list(call);

  /** [events(~stored_only, c)] returns the list of events of the contract 
      [c] that can be indexed as part of the subgraph. If [stored_only] is 
      [true] (default: [false]), then only the events that are stored as per the 
      comment tags are returned. */
  let events: (~stored_only:bool=?) => t => list(event);
};

/** The type of a subgraph. */
type t = {
  github_user: string,
  subgraph_name: string,
  description: string,
  contracts: list(Contract.t)
};

/** [contract_of_name(sg, name)] searches the subgraph [sg] for a contract with name [name] and
    returns [Some(contract)] if such a contract exists or [None] otherwise. */
let contract_of_name: t => string => option(Contract.t);

/** [child_contracts(subgraph, c)] returns a list of contract [c]'s child contracts, 
    i.e.: contracts that are added to the subgraph from an event or call of contract 
    [c]. For instance, consider Uniswap V2's two main contracts: the [Factory] contract 
    and the [Pair] contract. The [Factory] contract is usually a dataSource, but the 
    [Pair] contract (and its instances) are generated via templates from the [PairCreated] 
    events emitted by the [Factory] contract. In this scenario, the child contracts of the
    [Factory] contract consists of the [Pair] contract. */
let child_contracts: t => Contract.t => list(Contract.t);

/** [parent_contract(subgraph, c)] returns [Some(p)] where [p] is the parent contract
    of the contract [c], or [None] if contract [c] has no parent. Using the same example
    as for the [child_contracts] function, the parent contract of Uniswap V2's [Pair] 
    contract would be the [Factory] contract. The [Factory] contract does not have a 
    parent as it is a dataSource. */
let parent_contract: t => Contract.t => option(Contract.t);

/** [contract_related_entities(sg, c)] returns a list of {!string} values containing 
    the names of the entities related to contract [c] in subgraph [sg]. Related entities 
    include the entity of contract [c] itself, [c]'s parent contract entity (if any), 
    [c]'s child contracts entities, [c]'s events and calls entities (if any) 
    and any other entity that might be created, accessed or modified in the handlers 
    of contract [c]. */
let contract_related_entities: t => Contract.t => list(string);

/** [contract_related_contracts(sg, c)] returns a list of {!string} values containing 
    the names of the contracts related to contract [c] in subgraph [sg]. Related contracts 
    include the contract [c] itself, [c]'s parent contract (if any), [c]'s child contracts 
    and any other contract which are necessary for the handlers of contract [c]. */
let contract_related_contracts: t => Contract.t => list(string);

module Builder: {
  /** [make(~github_user, ~subgraph_name, ~desc, ast)] builds and returns a {!Subgraph.t} value from the abstract syntax tree. */
  let make: (~github_user:string=?) => (~subgraph_name:string=?) => (~desc:string=?) => Parsing.Ast.t => t;

  let validate: t => result(unit, [> Rresult.R.msg]);
};