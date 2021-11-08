open Parsetree;
open Parsetree_util;
open Typetree;

let logger = Easy_logging.Logging.make_logger("Database", Debug, [Cli(Debug)]);

exception Database_error(string);
exception ABI_error(string);

type interface_t = {
  fields: list((string, gql_type))
};

type entity_t = {
  interface: option(string),
  fields: list((string, gql_type))
};

type event_t = {
  params: list((string, sol_type))
};

type function_t = {
  inputs: list((string, sol_type)),
  outputs: list((string, sol_type))
};

type abi_t = {
  name: string,
  abi_path: string,
  events: list((string, event_t)),
  calls: list((string, function_t)),
  functions: list((string, function_t))
};

type data_source_t = {
  address: string,
  start_block: string,
  abi: string
};

type template_t = {
  abi: string
};

type t = {
  interfaces: list((string, interface_t)),
  entities: list((string, entity_t)),
  abis: list((string, abi_t)),
  data_sources: list((string, data_source_t)),
  templates: list((string, template_t))
};

let empty_contract = (name, abi_path) => {
  name,
  abi_path,
  events: [],
  calls: [],
  functions: []
};

let empty = {
  interfaces: [],
  entities: [],
  abis: [],
  data_sources: [],
  templates: []
};

let load_abi = (name, path) => {
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
  let parse_event = (fields) =>
    Yojson.Basic.(Util.to_string @@ get_assoc("name", fields), {
      params: List.map((param) => {
        let typ = parse_sol_type(Util.to_string @@ get_assoc_either("internalType", "type", param));

        (
          Util.to_string @@ get_assoc("name", param), 
          Util.to_bool @@ get_assoc("indexed", param) ? SOLIndexed(typ) : typ
        )
      },
      List.map(Util.to_assoc) @@ Util.to_list @@ get_assoc("inputs", fields))
    });

  // Extracts a value of type [abi_typ] from an ABI function definition
  let parse_function = (fields) =>
    Yojson.Basic.(
      Util.to_string @@ get_assoc("name", fields),
      switch (Util.to_string @@ get_assoc("stateMutability", fields)) {
      | "pure" => Pure
      | "view" => View
      | "payable" => Payable
      | "nonpayable" => NonPayable
      | s => raise(ABI_error([%string "unknown state mutability: %{s}"]))
      },
      {
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
      }
    );

  let add_abi_element = (contract, ele) =>
    switch (ele) {
    | `Assoc(fields) =>
      switch (Yojson.Basic.Util.to_string @@ get_assoc("type", fields)) {
      | "constructor" => contract
      | "event" => {...contract, events: [parse_event(fields), ...contract.events]}
      | "function" => 
        switch (parse_function(fields)) {
        | (name, Pure | View, func) => {...contract, functions: [(name, func), ...contract.functions]}
        | (name, Payable | NonPayable, func) => {...contract, calls: [(name, func), ...contract.calls]}
        }
      | s => raise(ABI_error([%string "unknown type %{s}"]))
      }
    | _ => raise(ABI_error("abi element"))
    };
  

  Yojson.Basic.from_file(path)
  |> Yojson.Basic.Util.to_list
  |> List.fold_left((acc, ele) => add_abi_element(acc, ele), empty_contract(name, path))
};

let make = (document) => {
  let update_db = (db, toplevel) =>
    switch (toplevel) {
    | Interface({name, fields}) => 
      {...db, interfaces: [(name, {fields: List.map(((name, typ)) => (name, typ), fields)}), ...db.interfaces]}
    | Entity({name, fields, interface}) => 
      {...db, entities: [(name, {interface, fields: List.map(((name, typ)) => (name, typ), fields)}), ...db.entities]}

    | DataSource({name, abi, address, start_block}) => 
      try (
        {...db, data_sources: [(name, 
          {
            address: lit_to_string(address), start_block: string_of_int(lit_to_int(start_block)), abi
          }), ...db.data_sources],
        }
      ) {
      | _ => raise(Database_error("data_source: address is not a string or address literal"))
      }

    | Template({name, abi}) =>
      {...db, templates: [(name, {abi: abi}), ...db.templates]}

    | ABI({name, file}) => 
      try (
        {...db, abis: [(name, load_abi(name, lit_to_string(file))), ...db.abis]}
      ) {
      | _ => raise(Database_error("contract: abi path is not a string literal"))
      }

    // | Event({name, params}) =>
    //   [(name, TEvent({params: List.map(((name, typ)) => (name, TSol(typ)), params)})), ...env]
    // | Call({name, inputs, outputs}) =>
    //   [(name, TCall({inputs: List.map(((name, typ)) => (name, TSol(typ)), inputs), outputs: List.map(((name, typ)) => (name, TSol(typ)), outputs)})), ...env]
    // Skip handlers as we want to build the environment before type checking them
    | EventHandler(_)
    | CallHandler(_) => db
    };

  List.fold_left((acc, toplevel) => update_db(acc, toplevel), empty, document);
};

let interface = ({interfaces, _}, name) =>
  switch (List.assoc_opt(name, interfaces)) {
  | Some(interface) => interface
  | None => raise(Database_error([%string "interface %{name} not found"]))
  };

let interface_opt = ({interfaces, _}, name) => 
  List.assoc_opt(name, interfaces);

let entity = ({entities, _}, name) =>
  switch (List.assoc_opt(name, entities)) {
  | Some(entity) => entity
  | None => raise(Database_error([%string "entity %{name} not found"]))
  };

let entity_opt = ({entities, _}, name) => 
  List.assoc_opt(name, entities);

let abi = ({abis, _}, name) =>
  switch (List.assoc_opt(name, abis)) {
  | Some(abi) => abi
  | None => raise(Database_error([%string "abi %{name} not found"]))
  };

let abi_opt = ({abis, _}, name) => 
  List.assoc_opt(name, abis);

let data_source = ({data_sources, _}, name) =>
  switch (List.assoc_opt(name, data_sources)) {
  | Some(data_source) => data_source
  | None => raise(Database_error([%string "data_source %{name} not found"]))
  };

let data_source_opt = ({data_sources, _}, name) => 
  List.assoc_opt(name, data_sources);

let template = ({templates, _}, name) =>
  switch (List.assoc_opt(name, templates)) {
  | Some(template) => template
  | None => raise(Database_error([%string "template %{name} not found"]))
  };

let template_opt = ({templates, _}, name) => 
  List.assoc_opt(name, templates);

let type_of = (db, name) =>
  switch (
    interface_opt(db, name),
    entity_opt(db, name),
    data_source_opt(db, name),
    template_opt(db, name),
    abi_opt(db, name)
  ) {
  | (Some(_), _, _, _, _)             => `Interface
  | (None, Some(_), _, _, _)          => `Entity
  | (None, None, Some(_), _, _)       => `DataSource
  | (None, None, None, Some(_), _)    => `Template
  | (None, None, None, None, Some(_)) => `ABI
  | (None, None, None, None, None)    => `Unknown
  };