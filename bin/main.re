open Cmdliner;

open Graphgenlib;
open Parsing;

Printexc.record_backtrace(true);
let logger = Easy_logging.Logging.make_logger("GraphGen", Debug, [Cli(Debug)]);

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

let graphgen = (desc, repo, target_path) => {
  let (>>=) = Result.bind;
  let (>|=) = (res, f) => Result.map(f, res);
  let fmt_err = Result.map_error((`Msg(msg)) => msg);

  let generate_manifest = Generator.single_file("templates/manifest.j2", "subgraph/subgraph.yaml", Models.manifest_models)
  let generate_schema = Generator.single_file("templates/schema.j2", "subgraph/schema.graphql", Models.schema_models)
  let generate_data_sources = Generator.multi_file("templates/data_source.j2", (key) => [%string "subgraph/src/mappings/%{String.uncapitalize_ascii key}.ts"], Models.data_sources_models)
  let generate_templates = Generator.multi_file("templates/template.j2", (key) => [%string "subgraph/src/mappings/%{String.uncapitalize_ascii key}.ts"], Models.templates_models)

  let generate_from_file = (path) => {
    fmt_err @@ File.read(path)            >>= (source) => 
    parse(source)                         >|= 
    Subgraph.Builder.make(~desc, ~repo)   >>= (subgraph) =>
    Generator.generate_directories()      >>= (_) => 
    generate_manifest(subgraph)           >>= (_) =>
    generate_schema(subgraph)             >>= (_) =>
    generate_data_sources(subgraph)       >>= (_) =>
    generate_templates(subgraph)
  };

  let generate_from_dir = (_) => {
    failwith("Not implemented")
  };

  let path = Fpath.v(target_path);

  switch (Bos.OS.(Dir.exists(path), File.exists(path))) {
  | (Ok(true), Ok(false)) => generate_from_dir(path)
  | (Ok(false), Ok(true)) => generate_from_file(path)
  | _ => Result.error([%string "Invalid path: %{target_path}"])
  }
  |> fun
    | Error(msg) => logger#error("%s", msg)
    | Ok() => ()
};

let description = {
  let doc = "Subgraph description"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["d", "description"], ~doc))
};

let repository = {
  let doc = "Subgraph repository"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["r", "repository"], ~doc))
};

let path = {
  let doc = "Solidity interface file or directory containing multiple interface files"
  Arg.(required & pos(~rev=true, 0, some(string), None) & info([], ~doc))
};

let graphgen_t = Term.(const(graphgen) $ description $ repository $ path);

let info = {
  let doc = "Generate a subgraph from annotated solidity interfaces"
  Term.info("graphgen", ~doc, ~exits=Term.default_exits)
};

let () = Term.exit @@ Term.eval((graphgen_t, info));