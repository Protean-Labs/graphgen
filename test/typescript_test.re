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
  let exprs = update_field_event_handler("name", "ERC20");

  let expected = 
"let emitterContract = ERC20.bind(event.address)
emitter.name = emitterContract.name()";

  assert_equal(~printer=x => x, expected, generate_exprs(exprs))
}

let suite = "Typescript generation test suite" >::: [
  "test_declare" >:: test_declare,
  "test_assign" >:: test_assign,
  "test_obj_member_assign" >:: test_obj_member_assign,
  "test_function_declare" >:: test_function_declare,
  "test_update_field_event_handler" >:: test_update_field_event_handler,
];