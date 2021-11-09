open Rresult;

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

module Parsetree = Parsetree;
module Parsetree_util = Parsetree_util
module Typetree = Typetree;
module Validate = Validate;
module Database = Database;

exception Runtime_error(string);

let read_file = (path) =>{
  let path = Fpath.v(path);
  // logger#debug("Trying path %s", Fpath.filename(path));
  
  switch (File.read(path)) {
  | Ok(source) => source
  | Error(`Msg(msg)) => raise(Runtime_error(msg))
  };
};

let parse = (source) => 
  try (Result.ok @@ Parser.document(Lexer.token, Lexing.from_string(source))) {
  | Lexer.Parsing_error(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

let parse_file = (path) => parse(read_file(path));