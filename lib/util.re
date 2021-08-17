open Rresult;

let existing_path = (paths) =>
  List.fold_left((acc, path) => 
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