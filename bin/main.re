open Cmdliner;

open Graphgenlib;
open Parsing;

Printexc.record_backtrace(true);
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

let graphgen = (_, _, source) => {
  

  switch (Bos.OS.(Dir.exists(Fpath.v(source)), File.exists(Fpath.v(source)))) {
  | (Ok(true), Ok(false)) => failwith("Not implemented")
  | (Ok(false), Ok(true)) => {
    let file = load_file(source);
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
  | _ => failwith("Invalid filename" ++ source)
  }
};

let description = {
  let doc = "Subgraph description"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["d", "description"], ~doc))
};

let repository = {
  let doc = "Subgraph repository"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["r", "repository"], ~doc))
};

let intf_path = {
  let doc = "Solidity interface file or directory containing multiple interface files"
  Arg.(required & pos(~rev=true, 0, some(string), None) & info([], ~doc))
};

let graphgen_t = Term.(const(graphgen) $ description $ repository $ intf_path);

let info = {
  let doc = "Generate a subgraph from annotated solidity interfaces"
  Term.info("graphgen", ~doc, ~exits=Term.default_exits)
};

let () = Term.exit @@ Term.eval((graphgen_t, info));