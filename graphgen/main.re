open Solidity;
open Subgraph;

let () = {
  let swap_event: event = {
    name: "Swap",
    fields: [
      ("sender", AddressT),
      ("amount0In", UintT(256)),
      ("amount1In", UintT(256)),
      ("amount0Out", UintT(256)),
      ("amount1Out", UintT(256)),
      ("to", AddressT)
    ]
  };

  let mint_event: event = {
    name: "Mint",
    fields: [
      ("sender", AddressT),
      ("amount0", UintT(256)),
      ("amount1", UintT(256))
    ]
  };

  let burn_event: event = {
    name: "Burn",
    fields: [
      ("sender", AddressT),
      ("amount0", UintT(256)),
      ("amount1", UintT(256))
    ]
  };

  let subgraph = [
    {
      name: "UniswapV2Pair",
      fields: [
        ("token0", AddressT),
        ("token1", AddressT),
      ],
      triggers: [
        Event(swap_event, [StoreEvent(swap_event)]),
        Event(mint_event, [StoreEvent(mint_event)]),
        Event(burn_event, [StoreEvent(burn_event)]),
      ]
    }
  ];

  Schema.of_subgraph(subgraph)
  |> print_endline
}