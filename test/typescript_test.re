open OUnit2;
open Graphgen.Typescript;

let test_declare = (_) => {
  let expr = Declare("x", Literal(IntV(2)));
  let expected = "let x = 2"

  assert_equal(~printer=x => x, expected, generate_expr(expr))
};

let test_assign = (_) => {
  let expr = Assign(Var("x"), Literal(IntV(2)));
  let expected = "x = 2"

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

let test_update_field_event_handler = (_) => {
  let exprs = update_field_event_handler("ERC20", ["name"]);

  let expected = 
"let emitterContract = ERC20.bind(event.address)
emitter.name = emitterContract.name()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
};

let test_update_field_entity_creation = (_) => {
  let exprs = update_field_entity_creation("ERC20", ["name"]);

  let expected = 
"let entityContract = ERC20.bind(Address.fromString(entity.id))
entity.name = entityContract.name()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
};

let test_create_entity_from_event_field = (_) => {
  let exprs = create_entity_from_event_field("OToken", "addr", None);

  let expected = 
"let entityAddress = event.params.addr.toHexString().toString()
let entity = new OToken(entityAddress)
entity.save()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
};

let test_create_entity_from_event_field_with_1init = (_) => {
  let exprs = create_entity_from_event_field("OToken", "addr", Some(("ERC20", ["name"])));

  let expected = 
"let entityAddress = event.params.addr.toHexString().toString()
let entity = new OToken(entityAddress)
let entityContract = ERC20.bind(Address.fromString(entity.id))
entity.name = entityContract.name()
entity.save()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
};

let test_create_entity_from_event_field_with_2init = (_) => {
  let exprs = create_entity_from_event_field("OToken", "addr", Some(("ERC20", ["name", "symbol"])));

  let expected = 
"let entityAddress = event.params.addr.toHexString().toString()
let entity = new OToken(entityAddress)
let entityContract = ERC20.bind(Address.fromString(entity.id))
entity.name = entityContract.name()
entity.symbol = entityContract.symbol()
entity.save()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
};

let suite = "Typescript generation test suite" >::: [
  "test_declare" >:: test_declare,
  "test_assign" >:: test_assign,
  "test_obj_member_assign" >:: test_obj_member_assign,
  "test_function_declare" >:: test_function_declare,
  "test_update_field_event_handler" >:: test_update_field_event_handler,
  "test_update_field_entity_creation" >:: test_update_field_entity_creation,
  "test_create_entity_from_event_field" >:: test_create_entity_from_event_field,
  "test_create_entity_from_event_field_with_1init" >:: test_create_entity_from_event_field_with_1init,
  "test_create_entity_from_event_field_with_2init" >:: test_create_entity_from_event_field_with_2init,
];