open OUnit2;
open Graphgen;
open Graphgen.Typescript;

let test_declare = (_) => {
  let expr = Declare("x", Literal(IntV(2)));
  let expected = "let x = 2";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_assign = (_) => {
  let expr = Assign(Var("x"), Literal(IntV(2)));
  let expected = "x = 2";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_obj_member_assign = (_) => {
  let expr = Assign(Member(Var("obj"), "attr1"), Literal(IntV(2)));
  let expected = "obj.attr1 = 2"

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_function_declare = (_) => {
  let expr = FunDeclare("f", [("x", "Int")], "void", [
    Declare("obj", Apply(Var("foo"), [])),
    Assign(Member(Var("obj"), "attr1"), Literal(IntV(2)))
  ]);
  let expected = 
"function f(x: Int): void {
  let obj = foo()
  obj.attr1 = 2
}";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_update_fields_event_handler = (_) => {
  let expr = Blocks.update_fields_event_handler("ERC20", ["name"]);

  let expected = 
"// Update entity fields
let emitterContract = ERC20.bind(event.address)
emitter.name = emitterContract.name()";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

// let test_init_fields_entity_creation = (_) => {
//   let expr = Blocks.init_fields_entity_creation("ERC20", ["name"]);

//   let expected = 
// "// Initialize entity fields
// let entityContract = ERC20.bind(Address.fromString(entity.id))
// entity.name = entityContract.name()";

//   assert_equal(~printer=x => x, expected, generate_expr(expr))
// };

let test_create_entity_from_event_field = (_) => {
  let expr = Blocks.create_entity_from_event_field("OToken", "OTokenContract", "addr");

  let expected = 
"let entityAddress = event.params.addr
OTokenContract.create(entityAddress)
OToken.createOToken(entityAddress.toHexString())";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_create_entity_function_with_1init = (_) => {
  let contract: Subgraph.contract = {
    name: "Pair", 
    raw_name: "IUniswapV2Pair", 
    fields: [("addr", AddressT)],
    handlers: [
      Event({name: "Swap", fields: []}, [StoreEvent])
    ] 
  };

  let expr = Functions.create_entity(contract);

  let expected = 
"export function createPair(address: Address): Pair {
  let entity = new Pair(address.toHexString())
  // Initialize entity fields
  let entityContract = IUniswapV2Pair.bind(address)
  entity.addr = entityContract.addr()
  entity.numSwaps = BigIntZero
  entity.save()
  return entity
}";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_create_entity_function_with_2init = (_) => {
  let contract: Subgraph.contract = {
    name: "Pair", 
    raw_name: "IUniswapV2Pair", 
    fields: [("addr", AddressT), ("symbol", StringT)],
    handlers: [
      Event({name: "Swap", fields: []}, [StoreEvent])
    ] 
  };

  let expr = Functions.create_entity(contract);

  let expected = 
"export function createPair(address: Address): Pair {
  let entity = new Pair(address.toHexString())
  // Initialize entity fields
  let entityContract = IUniswapV2Pair.bind(address)
  entity.addr = entityContract.addr()
  entity.symbol = entityContract.symbol()
  entity.numSwaps = BigIntZero
  entity.save()
  return entity
}";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_create_event = (_) => {
  let expr = Functions.create_event({
    name: "MyEvent", 
    fields: [("addr", AddressT), ("word", StringT), ("x", UintT(256))]
  });

  let expected = 
"function createMyEvent(counter: BigInt, event: MyEvent): MyEvent {
  let eventEntity = new MyEvent(event.address.toHexString() + \"-\" + counter.toString())
  eventEntity.logIndex = event.logIndex
  eventEntity.tx = event.transaction.hash.toHexString()
  eventEntity.addr = event.params.addr.toHexString()
  eventEntity.word = event.params.word
  eventEntity.x = event.params.x
  eventEntity.save()
  return eventEntity
}";

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let suite = "Typescript generation test suite" >::: [
  "test_declare" >:: test_declare,
  "test_assign" >:: test_assign,
  "test_obj_member_assign" >:: test_obj_member_assign,
  "test_function_declare" >:: test_function_declare,
  "test_update_fields_event_handler" >:: test_update_fields_event_handler,
  // "test_init_fields_entity_creation" >:: test_init_fields_entity_creation,
  "test_create_entity_from_event_field" >:: test_create_entity_from_event_field,
  "test_create_entity_function_with_1init" >:: test_create_entity_function_with_1init,
  "test_create_entity_function_with_2init" >:: test_create_entity_function_with_2init,
  "test_create_event" >:: test_create_event
];