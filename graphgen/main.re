open Ast;
open Subgraph;

// let () = {
//   let swap_event: event = {
//     name: "Swap",
//     fields: [
//       ("sender", AddressT),
//       ("amount0In", UintT(256)),
//       ("amount1In", UintT(256)),
//       ("amount0Out", UintT(256)),
//       ("amount1Out", UintT(256)),
//       ("to", AddressT)
//     ]
//   };

//   let mint_event: event = {
//     name: "Mint",
//     fields: [
//       ("sender", AddressT),
//       ("amount0", UintT(256)),
//       ("amount1", UintT(256))
//     ]
//   };

//   let burn_event: event = {
//     name: "Burn",
//     fields: [
//       ("sender", AddressT),
//       ("amount0", UintT(256)),
//       ("amount1", UintT(256))
//     ]
//   };

//   let subgraph = [
//     {
//       name: "UniswapV2Pair",
//       fields: [
//         ("token0", AddressT),
//         ("token1", AddressT),
//       ],
//       handlers: [
//         Event(swap_event, [StoreEvent(swap_event)]),
//         Event(mint_event, [StoreEvent(mint_event)]),
//         Event(burn_event, [StoreEvent(burn_event)]),
//       ]
//     }
//   ];

//   Schema.of_subgraph(subgraph)
//   |> print_endline
// }

let load_file = (filename) => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s
};

let () = {
  // Easy_logging.Handlers.set_level h Info;
  Printexc.record_backtrace(true);
  let interface = load_file("IUniswapV2Pair.sol");

  // let i = Parser.source_unit(Lexer.token, Lexing.from_string(interface));
  try (Ok(Parser.source_unit(Lexer.token, Lexing.from_string(interface)))) {
    | Lexer.ParsingError(err) => Error(err)
    // | Parser.Error => Error("Unhandled parser error")
    // | Parser.MenhirBasics.Error(err) => Error(err)
    | exn => Error(Printexc.to_string(exn) ++ ": " ++ Printexc.get_backtrace())
  }
  |> fun
    | Error(msg) => print_endline(msg)
    | Ok(ast) => Format.asprintf("%a", Ast.pp_subgraph, ast) |> print_endline;
};

// let () = {
//   Printexc.record_backtrace(true);
//   let test = load_file("test.yaml");

//   open Rresult;
//   Yaml.of_string(test)
//   |> R.bind(_, Ast.gg_source_params_of_yaml)
//   |> fun
//     | Error(`Msg(err)) => failwith(err)
//     // | Ok(params) => Format.asprintf("%a", Ast.pp_gg_source_params, params) |> print_endline
//     | Ok(params) => print_endline("Ok")
// };