type unary_op =
  | Neg
;

type binary_op = 
  | Add
  | Sub
  | Mult
  | Div
;

type value = 
  | StringV(string)
  | IntV(int)
  | FloatV(float)
;

type expr = 
  | Var(string)
  | Literal(value)
  | UnaryOp(unary_op, expr)
  | BinaryOp(binary_op, expr, expr)
  | Declare(string, expr)
  | Assign(expr, expr)
  | Apply(expr, list(expr))
  | Cast(expr, string)
  | Member(expr, string)
  | New(expr, list(expr))
  | FunDeclare(string, list((string, string)), string, list(expr))
  | Export(expr)
  | Return(expr)
  // Not actual typescript, only used in generation  
  | Empty
  | Block(string, list(expr))   // Not actual typescript, only useful in generation to annotate code blocks
;

let generate_expr: expr => string;

let generate_exprs: list(expr) => string;

module Blocks: {
  /** [wrap(e1, exprs, e2)] generates the following:
  
  ```
  code from e1

  code from all e in exprs

  code from e2
  ```
  */
  let wrap: expr => list(expr) => expr => list(expr);

  /**
    [update_field_event_handler("ERC20", ["name"])] generates the following code block:

    ```
    let emitterContract = ERC20.bind(event.address)
    emitter.name = emitterContract.name()
    ```
  */
  let update_fields_event_handler: string => list(string) => expr;

  /**
    [init_fields_entity_creation(entity_name, field_names)]
    [init_fields_entity_creation("ERC20", ["name"])] generates the following code block:

    ```
    let entityContract = ERC20.bind(Address.fromString(entity.id))
    entity.name = entityContract.name()
    ```
  */
  let init_entity_contract_fields: string => list(string) => expr;

  let init_entity_event_fields: list(Subgraph.event) => expr;

  /**
    [create_entity_from_event_field(entity_name, contract_name, field)]
    [create_entity_from_event_field("OToken", "OTokenContract", "addr")] 
    generates the following code block:

    ```
    let entityAddress = event.params.addr
    OTokenContract.create(entityAddress)
    createOToken(entityAddress.toHexString())
    ```
  */
  let create_entity_from_event_field: string => string => string => expr;
  
  /**
    [store_event({"MyEvent", [("addr", AddressT)]})] generates the following:

    ```
    // Create event and update emitter
    let event = createMyEvent(emitter.numMyEvents, event)
    emitter.numMyEvents = emitter.numMyEvents + BigIntOne
    emitter.latestMyEvent = event
    emitter.save()
    ```
   */
  let store_event: Subgraph.event => expr;
};

module Functions: {
  /**
    [create_event({"MyEvent", [("addr", AddressT)]})] generates the following:

    ```
    function createMyEvent(counter: BigInt, event: MyEvent): MyEvent {
      let eventEntity = new MyEvent(event.address.toHexString() + "-" counter.toString())
      eventEntity.logIndex = event.logIndex
      eventEntity.tx = event.transaction.hash.toHexString()
      eventEntity.addr =  event.params.addr
      eventEntity.save()
      return eventEntity
    }
    ```
  */
  let create_event: Subgraph.event => expr;

  /**
    [create_entity(entity_name, contract_name, fields]
    [create_entity("myEntity", "myContract", ["addr"], ["Swap"])] generates the following:

    ```
    function create(address: Address): myEntity {
      let entity = new Entity(address.toHexString())
      let contract = myContract.bind(address)
      entity.addr = contract.addr()
      entity.numSwaps = BigIntZero
      entity.save()
      return entity
    }
    ```
   */
  let create_entity: Subgraph.contract => expr;
};




// Old functions
let utils_ts: string;

let of_subgraph: Subgraph.t => list((string, string))