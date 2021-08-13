open Cmdliner;
open Rresult;

open Libgraphgen;
open Parsing;

Printexc.record_backtrace(true);
let logger = Easy_logging.Logging.make_logger("GraphGen", Debug, [Cli(Debug)]);

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

let is_solidity = (path) => {
  let regex = Str.regexp("[-A-Za-z0-9_]+.sol");
  Str.string_match(regex, path, 0)
};

let graphgen = (github_user, subgraph_name, desc, output_dir, target_path) => {
  let generate_package_json = Generator.single_file("templates/package_json.j2", [%string "%{output_dir}/package.json"], Models.package_json_models)
  let generate_manifest = Generator.single_file("templates/manifest.j2", [%string "%{output_dir}/subgraph.yaml"], Models.manifest_models)
  let generate_schema = Generator.single_file("templates/schema.j2", [%string "%{output_dir}/schema.graphql"], Models.schema_models)
  let generate_util_ts = Generator.single_file("templates/util_ts.j2", [%string "%{output_dir}/src/util.ts"], Models.util_ts_models)
  let generate_abi = Generator.multi_file("templates/abi.j2", (key) => [%string "%{output_dir}/abis/%{key}.json"], Models.abi_models)
  let generate_data_sources = Generator.multi_file("templates/data_source.j2", (key) => [%string "%{output_dir}/src/mappings/%{key}.ts"], Models.data_sources_models)
  let generate_templates = Generator.multi_file("templates/template.j2", (key) => [%string "%{output_dir}/src/mappings/%{key}.ts"], Models.templates_models)

  let read_and_parse = (path) => {
    logger#debug("Parsing %s...", Fpath.filename(path));
    File.read(path)  >>= (source) => 
    parse(source)
  };

  let generate_from_ast = (ast) => {
    Subgraph.Builder.make(~github_user, ~subgraph_name, ~desc, ast)    |>  (subgraph) =>
    Subgraph.Builder.validate(subgraph)         >>= () =>
    Generator.generate_directories(output_dir)  >>= (_) => 
    generate_package_json(subgraph)             >>= () =>
    generate_manifest(subgraph)                 >>= () =>
    generate_schema(subgraph)                   >>= () =>
    generate_util_ts(subgraph)                  >>= () =>
    generate_abi(subgraph)                      >>= () =>
    generate_data_sources(subgraph)             >>= () =>
    generate_templates(subgraph)
  }

  let generate_from_file = (path) => {
    read_and_parse(path)    >>= (ast) =>
    generate_from_ast(ast)
  };

  let generate_from_dir = (path) => {
    let f = (acc, path) => {
      if (is_solidity(Fpath.filename(path))) {
        acc                   >>= (acc) =>
        read_and_parse(path)  >>| (ast) =>
        List.concat([acc, ast])
      } 
      else acc
    };

    Dir.contents(path)                >>= (files) =>
    List.fold_left(f, Ok([]), files)  >>= (ast) =>
    generate_from_ast(ast)
  };

  let path = Fpath.v(target_path);

  switch (Bos.OS.(Dir.exists(path), File.exists(path))) {
  | (Ok(true), Ok(false)) => generate_from_dir(path)
  | (Ok(false), Ok(true)) => generate_from_file(path)
  | _ => R.error_msg([%string "Invalid path: %{target_path}"])
  }
  |> fun
    | Error(`Msg(msg)) => {logger#error("%s", msg); exit(-1)}
    | Ok() => ()
};

// Command-line arguments
let description = {
  let doc = "Subgraph description"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["d", "description"], ~doc))
};

let github_user = {
  let doc = "The Graph Github user"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["u", "user"], ~doc))
};

let subgraph_name = {
  let doc = "The name of the subgraph"
  Arg.(value & opt(string, "PLACEHOLDER") & info(["n", "name"], ~doc))
};

let output_dir = {
  let doc = "The name of the output directory to which the subgraph will be generated"
  Arg.(value & opt(string, "subgraph") & info(["o", "output-dir"], ~doc))
};

let path = {
  let doc = "Solidity interface file or directory containing multiple interface files annotated with graphgen tags"
  Arg.(required & pos(~rev=true, 0, some(string), None) & info([], ~docv="SOURCE", ~doc))
};

let graphgen_t = Term.(const(graphgen) $ github_user $ subgraph_name $ description $ output_dir $ path);

let info = {
  let doc = "Generate a subgraph from annotated solidity interfaces"
  Term.info("graphgen", ~doc, ~exits=Term.default_exits)
};

let () = Term.exit @@ Term.eval((graphgen_t, info));