module Ast = Ast;

let parse = (source) => 
  try (Result.ok @@ Parser.source_unit(Lexer.token, Lexing.from_string(source))) {
  | Lexer.ParsingError(err) => Result.error(err)
  | exn => Error(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };