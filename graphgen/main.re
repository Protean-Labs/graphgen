let load_file = (filename) => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s
};

let () = {
  // Easy_logging.Handlers.set_level h Info;
  Printexc.record_backtrace(true);
  let interface = load_file("IERC20.sol");

  // let i = Parser.source_unit(Lexer.token, Lexing.from_string(interface));
  try (Ok(Parser.source_unit(Lexer.token, Lexing.from_string(interface)))) {
    | Lexer.ParsingError(err) => Error(err)
    // | Parser.Error => Error("Unhandled parser error")
    // | Parser.MenhirBasics.Error(err) => Error(err)
    | exn => Error(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  }
  |> fun
    | Error(msg) => print_endline(msg)
    | Ok(ast) => Format.asprintf("%a", Ast.pp_subgraph, ast) |> print_endline;
};
