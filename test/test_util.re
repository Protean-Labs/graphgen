open Rresult;

open Gg_script.Parsetree;
open Gg_script.Parsetree_util;

module File = Bos.OS.File;
module Dir = Bos.OS.Dir;

let read_file = (path) => 
  R.get_ok @@ File.read(Fpath.v(path));

let fix_abi_paths = (document) => 
  List.map((toplevel) => 
    switch (toplevel) {
    | ABI({name, file}) => ABI({name, file: Literal(String([%string {|%{Sys.getenv "TEST_DIR"}/%{lit_to_string file}|}]))})
    | toplevel => toplevel
    },
    document
  );