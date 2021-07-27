open Jingoo;

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

let logger = Easy_logging.Logging.make_logger("Generator", Debug, [Cli(Debug)]);

let uncapitalize_filter = ("uncapitalize", Jg_types.func_arg1_no_kw((value) => {
  Jg_runtime.string_of_tvalue(value)
  |> String.uncapitalize_ascii
  |> (s) => Jg_types.Tstr(s)
}));

let plural = (word) => {
  let rule1 = Str.regexp_string("[A-Za-z]+\\(s\\|ss\\|sh\\|ch\\|x\\|z\\|\\)");
  let rule2 = Str.regexp_string("\\([A-Za-z]+\\)\\(a\\|e\\|i\\|o\\|u\\)y");
  let rule3 = Str.regexp_string("\\([A-Za-z]+\\)y");
  let rule4 = Str.regexp_string("[A-Za-z]+o");
  let rule5 = Str.regexp_string("\\([A-Za-z]\\)+is");
  let rule6 = Str.regexp_string("\\([A-Za-z]\\)+on");

  switch (word) {
  | s when Str.string_match(rule1, s, 0) => [%string "%{s}es"]
  | s when Str.string_match(rule2, s, 0) => [%string "%{s}s"]
  | s when Str.string_match(rule3, s, 0) => Str.replace_first(rule3, "\\1ies", s)
  | s when Str.string_match(rule4, s, 0) => [%string "%{s}es"]
  | s when Str.string_match(rule5, s, 0) => Str.replace_first(rule5, "\\1es", s)
  | s when Str.string_match(rule6, s, 0) => Str.replace_first(rule6, "\\1a", s)
  | s => [%string "%{s}s"]
  }
};

let counter_name_filter = ("counterName", Jg_types.func_arg1_no_kw((value) => {
  Jg_runtime.string_of_tvalue(value)
  |> plural
  |> (s) => Jg_types.Tstr([%string "num%{s}"])
}));

let generate_directories = () => {
  // let f = (acc, path) => switch (acc) {
  //   | Ok(_) => Bos.OS.Dir.create(~path=true, Fpath.v(path))
  //   | Error(msg) => Error(msg)
  // };

  [
    "subgraph/abis",
    "subgraph/src/mappings"
  ]
  |> List.fold_left((res, path) => Result.bind(res, _ => Dir.create(~path=true, Fpath.v(path))), Result.ok(true))
  |> Result.map_error((`Msg(msg)) => {
      logger#error("Creating directories: %s", msg);
      msg
  })
};

type models = list((string, Jingoo.Jg_types.tvalue));

let generate = (template_path, dest_path, models) => {
  Jg_template.from_file(template_path, ~env={...Jg_types.std_env, filters: [uncapitalize_filter, counter_name_filter]}, ~models)
  |> File.write(Fpath.v(dest_path))
  |> Result.map_error((`Msg(msg)) => {
    logger#error("Writing file %s: %s", dest_path, msg);
    msg
  })
};

let single_file = (template_path, dest_path, f, sg) => f(sg)
  |> generate(template_path, dest_path);

let multi_file = (template_path, dest_path, f, sg) => f(sg)
  |> List.fold_left((res, (key, models)) => Result.bind(res, _ => generate(template_path, dest_path(key), models)), Result.ok());