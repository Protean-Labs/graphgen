open Graphgenlib;
open Parsing;
// open Subgraph;

let logger = Easy_logging.Logging.make_logger("GraphGen", Debug, [Cli(Debug)]);

let load_file = (filename) => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s
};

let generate = (ast: Ast.t) => {
  print_endline(Ast.show(ast));
  Generator.generate_directories();
  let subgraph = Subgraph.Builder.make(ast);
  Generator.single_file("templates/manifest.j2", "subgraph/subgraph.yaml", Models.manifest_models, subgraph);
  Generator.single_file("templates/schema.j2", "subgraph/schema.graphql", Models.schema_models, subgraph);
  Generator.multi_file("templates/data_source.j2", (key) => [%string "subgraph/src/mappings/%{String.uncapitalize_ascii key}.ts"], Models.data_sources_models, subgraph);
  Generator.multi_file("templates/template.j2", (key) => [%string "subgraph/src/mappings/%{String.uncapitalize_ascii key}.ts"], Models.templates_models, subgraph);

  let (>>=) = Result.bind;
  
  Package.make("PLACEHOLDER", "PLACEHOLDER", "PLACEHOLDER") 
  |> Package.to_file
  >>= (() => 
    List.map(Abi.make, ast)
    |> List.iter(Abi.to_file(_, "subgraph/abis"))
    |> _ => Ok())
  |> fun
    | Ok() => ()
    | Error(msg) => logger#error("%s", msg)
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
          // |> fun | Ok(_) => () | Error(`Msg(err)) => failwith(err)
    }
    | _ => failwith("Invalid filename" ++ Sys.argv[1])
    }
  }
};