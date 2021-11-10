open Rresult;

// open Gg_script.Parsetree;
// open Gg_script.Parsetree_util;

open Solidity_parsing;

// let generate_document = (interfaces) => {
//   let defs_of_intf_element = (source, intf_element) =>
//     switch (intf_element) {
//     | FunctionDef(name, _, _, Some(GGHandler({name: ename, actions})), _) => []
//     | FallbackDef => []
//     | ReceiveDef  => []
//     | StructDef   => []
//     | EnumDef     => []
//     | EventDef(name, _, Some(GGHandler({name: ename, actions}))) => []
//     };
  
//   let defs_of_interface = (interface) => {
//     let abi = mk_abi(interface.raw_name, [%string "abis/%{interface.raw_name}.json"]);

//     switch (interface.tag) {
//     | Some(GGSource({name, instances: Some([]) | None, _})) => 
//       [abi, mk_template(Option.value(~default=interface.raw_name, name), interface.raw_name)]
      
//     | Some(GGSource({name, instances: Some(instances), _})) => 
//       List.cons(abi) @@ List.map(({address, startBlock}) =>
//         mk_data_source(
//           Option.value(~default=interface.raw_name, name), 
//           interface.raw_name,
//           address,
//           startBlock
//         ),
//         instances
//       )

//     | _ => [abi]
//     }
//   };

//   List.map(defs_of_interface, interfaces)
//   |> List.flatten;
// };

let parse = (source) => 
  try (Result.ok @@ Parser.document(Lexer.token, Lexing.from_string(source))) {
  // | Lexer.Parsing_error(err) => R.error_msg(err)
  | exn => R.error_msg(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  };

// let parse_file = (path) => parse(read_file(path));