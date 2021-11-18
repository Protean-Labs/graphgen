open Parsetree;
open Typetree;

exception ABI_error(string);

let load_abi = (path) => {
  // Parse ABI types
  let parse_sol_type = (source) => {
    let split_postfix = (base, s) => 
      String.sub(s, String.length(base), String.length(s) - String.length(base));

    // logger#debug("Parsing type: %s", source);
    let fbytes_t = Str.regexp("bytes[0-9][0-9]?");
    let int_t = Str.regexp("int[0-9][0-9]?[0-9]?");
    let uint_t = Str.regexp("uint[0-9][0-9]?[0-9]?");

    switch (source) {
    | "address" => SOLAddress
    | "bool"    => SOLBool
    | "string"  => SOLString
    | "fixed"   => SOLFixed
    | "ufixed"  => SOLFixed
    | "bytes"   => SOLBytes
    | s when Str.string_match(fbytes_t, s, 0) => SOLFbytes(int_of_string(split_postfix("bytes", s)))
    | s when Str.string_match(int_t, s, 0)    => SOLInt(int_of_string(split_postfix("int", s)))
    | s when Str.string_match(uint_t, s, 0)   => SOLUint(int_of_string(split_postfix("uint", s)))
    | s => raise(ABI_error([%string "unknown type: %{s}"]))
    }
  };

  // Wrap List.assoc into a more friendly exception
  let get_assoc = (label, l) => 
    switch (List.assoc_opt(label, l)) {
    | Some(ele) => ele
    | None => raise(ABI_error([%string "assoc: %{label}"]))
    };

  // Same as [get_assoc], but two labels are tried (in order of priority)
  let get_assoc_either = (label1, label2, l) => 
    switch (List.assoc_opt(label1, l), List.assoc_opt(label2, l)) {
    | (Some(ele), _) 
    | (None, Some(ele)) => ele
    | (None, None) => raise(ABI_error([%string "assoc: %{label1} or %{label2}"]))
    };
    

  // Extracts a value of type [abi_typ] from an ABI event definition
  let type_of_abi_event = (fields) =>
    Yojson.Basic.(Util.to_string @@ get_assoc("name", fields), ABIEvent({
      params: List.map((param) => {
        let typ = parse_sol_type(Util.to_string @@ get_assoc_either("internalType", "type", param));

        (
          Util.to_string @@ get_assoc("name", param), 
          Util.to_bool @@ get_assoc("indexed", param) ? SOLIndexed(typ) : typ
        )
      },
      List.map(Util.to_assoc) @@ Util.to_list @@ get_assoc("inputs", fields))
    }));

  // Extracts a value of type [abi_typ] from an ABI function definition
  let type_of_abi_function = (fields) =>
    Yojson.Basic.(Util.to_string @@ get_assoc("name", fields), ABIFunction({
      mutability: 
        switch (Util.to_string @@ get_assoc("stateMutability", fields)) {
        | "pure" => Pure
        | "view" => View
        | "payable" => Payable
        | "nonpayable" => NonPayable
        | s => raise(ABI_error([%string "unknown state mutability: %{s}"]))
        },
      inputs: List.map((input) => {
        (
          Util.to_string @@ get_assoc("name", input), 
          parse_sol_type(Util.to_string @@ get_assoc_either("internalType", "type", input))
        )
      },
      List.map(Util.to_assoc) @@ Util.to_list @@ get_assoc("inputs", fields)),
      outputs: List.map((output) => {
        (
          Util.to_string @@ get_assoc("name", output), 
          parse_sol_type(Util.to_string @@ get_assoc_either("internalType", "type", output))
        )
      },
      List.map(Util.to_assoc) @@ Util.to_list @@ get_assoc("outputs", fields)),
    }));

  let type_of_abi = (ele) =>
    switch (ele) {
    | `Assoc(fields) =>
      switch (Yojson.Basic.Util.to_string @@ get_assoc("type", fields)) {
      | "constructor" => None
      | "event" => Some(type_of_abi_event(fields))
      | "function" => Some(type_of_abi_function(fields))
      | _ => raise(ABI_error("unknown type"))
      }
    | _ => raise(ABI_error("abi element"))
    };
  
  
  Yojson.Basic.from_file(path)
  |> Yojson.Basic.Util.to_list
  |> List.filter_map(type_of_abi)
};