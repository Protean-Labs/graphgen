// type toplevel = 
//   | Pragma(pragma_token, expr)
//   | Import(import, expr)
//   | Interface(interface, expr)
//   | Library(library, expr)
//   | Function(function_t, expr)
//   | Tag(args, expr)
//   | Const(const, expr)
//   | Struct(struct_t, expr)
//   | Enum(enum, expr)
//   | End
// ;

// let parse_dir: list(string) => list((string, toplevel));

// let expand_imports = (files) = fun
//   | Import(import, expr) => List.assoc(import, files) |> replace_end_node(_, expr)
//   | 
// ;

// type bytes_t = [
//   | `Bytes1T
//   | `Bytes2T
//   | `Bytes3T
//   | `Bytes4T
//   | `Bytes5T
//   | `Bytes6T
//   | `Bytes7T
//   | `Bytes8T
//   | `Bytes9T
//   | `Bytes10T
//   | `Bytes11T
//   | `Bytes12T
//   | `Bytes13T
//   | `Bytes14T
//   | `Bytes15T
//   | `Bytes16T
//   | `Bytes18T
//   | `Bytes19T
//   | `Bytes20T
//   | `Bytes21T
//   | `Bytes22T
//   | `Bytes23T
//   | `Bytes24T
//   | `Bytes25T
//   | `Bytes26T
//   | `Bytes27T
//   | `Bytes28T
//   | `Bytes29T
//   | `Bytes30T
//   | `Bytes31T
//   | `Bytes32T
// ];

// type int_t = [
//   // | `IntT    // 'int' is alias for 'int256' 
//   | `Int8T
//   | `Int16T
//   | `Int24T
//   | `Int32T
//   | `Int40T
//   | `Int48T
//   | `Int56T
//   | `Int64T
//   | `Int72T
//   | `Int80T
//   | `Int88T
//   | `Int96T
//   | `Int104T
//   | `Int112T
//   | `Int120T
//   | `Int128T
//   | `Int136T
//   | `Int144T
//   | `Int152T
//   | `Int168T
//   | `Int176T
//   | `Int184T
//   | `Int192T
//   | `Int200T
//   | `Int208T
//   | `Int216T
//   | `Int224T
//   | `Int232T
//   | `Int240T
//   | `Int248T
//   | `Int256T
// ];

// type uint_t = [
//   // | `UintT   // 'uint' is alias for 'uint256'
//   | `Uint8T
//   | `Uint16T
//   | `Uint24T
//   | `Uint32T
//   | `Uint40T
//   | `Uint48T
//   | `Uint56T
//   | `Uint64T
//   | `Uint72T
//   | `Uint80T
//   | `Uint88T
//   | `Uint96T
//   | `Uint104T
//   | `Uint112T
//   | `Uint120T
//   | `Uint128T
//   | `Uint136T
//   | `Uint144T
//   | `Uint152T
//   | `Uint168T
//   | `Uint176T
//   | `Uint184T
//   | `Uint192T
//   | `Uint200T
//   | `Uint208T
//   | `Uint216T
//   | `Uint224T
//   | `Uint232T
//   | `Uint240T
//   | `Uint248T
//   | `Uint256T
// ];


// FixedLengthBytes, UnsignedInt and Int types have an 
// int parameter indicating size
// E.g.: 'uint32' => UnsignedInt(32), 'bytes4' => FixedSizeBytes(4)
type typ = 
  | FixedSizeBytesT(int)
  | UintT(int)
  | IntT(int)
  | StringT
  | BoolT
  | AddressT
;

// foo.sol
//  
// function f(uint x) returns (unint);
// 
// => toplevel

// bar.sol
// 
// import foo.sol
// 
// 
// => toplevel

// expand_imports 