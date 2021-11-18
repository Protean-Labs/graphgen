open OUnit2;

Printexc.record_backtrace(true);

let suite = test_list([
  Test_parser.suite,
  Test_parser_inline.suite,
  Test_schema.suite,
  Test_assemblyscript.suite,
  Test_manifest.suite
]);

let () = 
  try (run_test_tt_main(suite)) {
  | _ => 
    print_endline @@ Printexc.get_backtrace();
  };