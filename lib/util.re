open Rresult;

let logger = Easy_logging.Logging.get_logger("GraphGen");

let existing_path = (paths) =>
  List.fold_left((acc, path) => 
    logger#debug("Trying path %s", path) |> () =>
    switch (acc) {
    | Error(_) => 
      switch (Bos.OS.Dir.exists(Fpath.v(path))) {
      | Error(_) as err => err
      | Ok(false) => R.error_msg("Directory not found")
      | Ok(true) => Ok(path)
      }
    | Ok(_) as path => path
    },
    R.error_msg("Directory not found"),
    paths
  );