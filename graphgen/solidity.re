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