open OUnit2;

let () = {
  run_test_tt_main("GraphGen test suite" >::: [
    Typescript_test.suite
  ])
};