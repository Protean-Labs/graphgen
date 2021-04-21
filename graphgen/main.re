let load_file = (filename) => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s
};

// let f_expression_eof = Parser.Incremental.f_expression_eof;
// let component_eof = Parser.Incremental.component_eof;

// let parse = (parse_fun, lexbuf) => {
//   /* see the Menhir manual for the description of
//      error messages support */
//   open MenhirLib.General;
//   module Interp = Parser.MenhirInterpreter;
//   let input = Interp.lexer_lexbuf_to_supplier(Lexer.token, lexbuf);
//   let success = prog => prog;
//   let failure = error_state => {
//     let env =
//       [@warning "-4"]
//       (
//         switch (error_state) {
//         | Interp.HandlingError(env) => env
//         | _ => assert(false)
//         }
//       );
//     switch (Interp.stack(env)) {
//     | lazy Nil => assert(false)
//     | lazy (
//         [@implicit_arity]
//         Cons(
//           [@implicit_arity] Interp.Element(state, _, start_pos, end_pos),
//           _,
//         )
//       ) =>
//       let message =
//         try (Some(Parser_messages.message(Interp.number(state)))) {
//         | Not_found => None
//         };
//       raise(Error([@implicit_arity] Parsing(message, start_pos, end_pos)));
//     };
//   };

//   try (
//     Interp.loop_handle(
//       success,
//       failure,
//       input,
//       parse_fun(lexbuf.Lexing.lex_curr_p),
//     )
//   ) {
//   | [@implicit_arity] Lexer.Error(input, pos) =>
//     raise(Error([@implicit_arity] Lexing(input, pos)))
//   };
// };

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
    | Ok(i) => print_endline(Format.sprintf("%d", i))
};
