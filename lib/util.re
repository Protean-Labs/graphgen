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

let rec first_n = (l, n) =>
  switch (l, n) {
  | ([], _) => []
  | (_, n) when n == 0 => []
  | ([hd, ...rest], n) => 
    [hd, ...first_n(rest, n - 1)]
  };

let remove_last_n = (l, n) =>
  switch (List.length(l) - n) {
  | len when len <= 0 => []
  | len => first_n(l, len)
  };
