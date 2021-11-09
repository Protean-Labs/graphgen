open Rresult;

module Ast = Ast;

let parse = (source) => 
  try (Result.ok @@ Parser.source_unit(Lexer.token, Lexing.from_string(source))) {
  | Lexer.ParsingError(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };