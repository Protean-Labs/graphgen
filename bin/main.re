open Graphgen;
open Ast;
open Subgraph;

let load_file = (filename) => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s
};

let generate = (ast: Ast.t) => {
  let subgraph = Subgraph.of_ast(ast);

  open Rresult;

  Bos.OS.Dir.create(~path=true, Fpath.v("subgraph/abis"))
  >>= (_) => 
    List.map(Abi.make, ast)
    |> List.iter(Abi.to_file(_, "subgraph/abis"))
    |> _ => Ok()
  >>= (_) => 
    Schema.of_subgraph(subgraph)
    |> Bos.OS.File.write(Fpath.v("subgraph/schema.graphql"))
  >>= (_) =>
    Bos.OS.Dir.create(~path=true, Fpath.v("subgraph/src/mappings"))
  >>= (_) =>
    Bos.OS.File.write(Fpath.v("subgraph/src/utils.ts"), Typescript.utils_ts)
  |> (r) => 
    Typescript.of_subgraph(subgraph)
    |> List.fold_left((acc, (filename, code)) => acc >>= ((_) => Bos.OS.File.write(Fpath.v([%string "subgraph/src/mappings/%{filename}"]), code)), r)
  >>= (_) =>
    Manifest.make(subgraph)
    |> Manifest.to_file(_, "subgraph")
  >>= (_) =>
    Package.make("placeholder", "placeholder", "placeholder")
    |> Package.to_file
};

let () = {
  Printexc.record_backtrace(true);
  if (Array.length(Sys.argv) != 2) {
    failwith("Usage: graphgen [FILE_OR_DIR]")
  } else {
    switch (Bos.OS.(Dir.exists(Fpath.v(Sys.argv[1])), File.exists(Fpath.v(Sys.argv[1])))) {
    | (Ok(true), Ok(false)) => failwith("Not implemented")
    | (Ok(false), Ok(true)) => {
      let file = load_file(Sys.argv[1]);
      try (Ok(Parser.source_unit(Lexer.token, Lexing.from_string(file)))) {
        | Lexer.ParsingError(err) => Error(err)
        // | Parser.Error => Error("Unhandled parser error")
        // | Parser.MenhirBasics.Error(err) => Error(err)
        | exn => Error(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
      }
      |> fun
        | Error(msg) => print_endline(msg)
        | Ok(ast) => 
          generate(ast)
          |> fun | Ok(_) => () | Error(`Msg(err)) => failwith(err)
    }
    | _ => failwith("Invalid filename" ++ Sys.argv[1])
    }
  }
};