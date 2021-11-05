open Parsetree;

// ================================================================
// Pretty-printing
// ================================================================ 
let rec string_of_gql_type = fun
  | GQLId                 => "ID"
  | GQLBytes              => "Bytes"
  | GQLString             => "String"
  | GQLInt                => "Int"
  | GQLBigInt             => "BigInt"
  | GQLFloat              => "Float"
  | GQLBigDecimal         => "BigDecimal"
  | GQLBoolean            => "Boolean"
  | GQLList(gql_type)     => [%string "[%{string_of_gql_type gql_type}]"]
  | GQLNonNull(gql_type)  => [%string "%{string_of_gql_type gql_type}!"]
  | GQLObject(name)       => name;

let rec string_of_sol_type = fun
  | SOLAddress      => "address"
  | SOLBool         => "bool"
  | SOLString       => "string"
  | SOLFixed        => "fixed"
  | SOLUfixed       => "ufixed"
  | SOLBytes        => "bytes"
  | SOLFbytes(n)    => [%string "bytes%{string_of_int n}"]
  | SOLInt(n)       => [%string "int%{string_of_int n}"]
  | SOLUint(n)      => [%string "uint%{string_of_int n}"]
  | SOLArray(typ)   => [%string "%{string_of_sol_type typ}[]"]
  | SOLIndexed(typ) => [%string "indexed %{string_of_sol_type typ}"];

let rec string_of_expr = (expr) =>
  switch (expr) {
  | Neg(e)        => [%string "-%{string_of_expr e}"]
  | Add(e1, e2)   => [%string "%{string_of_expr e1} + %{string_of_expr e2}"]
  | Sub(e1, e2)   => [%string "%{string_of_expr e1} - %{string_of_expr e2}"]
  | Mul(e1, e2)   => [%string "%{string_of_expr e1} * %{string_of_expr e2}"]
  | Div(e1, e2)   => [%string "%{string_of_expr e1} / %{string_of_expr e2}"]
  | Variable([], name) => name
  | Variable(path, name) =>
    let path_str = String.concat(".", path);
    [%string "%{path_str}.%{name}"]
  | Literal(lit) => 
    switch (lit) {
    | String(v)     => v
    | Bytes(v)      => v
    | Int(v)        => string_of_int(v)
    | BigInt(v)     => v
    | Float(v)      => string_of_float(v)
    | BigDecimal(v) => v
    | Bool(v)       => string_of_bool(v)
    | Address(v)    => v
    };
  | Index(e1, e2) => [%string "%{string_of_expr e1}[%{string_of_expr e2}]"]
  | Apply(e, args) => 
    let args_str = String.concat(", ") @@ List.map(string_of_expr, args);
    [%string "%{string_of_expr e}(%{args_str})"]
};

let string_of_field_mod = (field_mod) =>
  switch (field_mod) {
  | Increment(name) => [%string "    %{name}++"]
  | Decrement(name) => [%string "    %{name}--"]
  | PlusEq(name, expr) => [%string "    %{name} += %{string_of_expr expr}"]
  | MinusEq(name, expr) => [%string "    %{name} -= %{string_of_expr expr}"]
  | Assign(name, expr) => [%string "    %{name} = %{string_of_expr expr}"]
  };

let field_of_field_mod = (field_mod) =>
  switch (field_mod) {
  | Increment(fname)
  | Decrement(fname)
  | PlusEq(fname, _)
  | MinusEq(fname, _)
  | Assign(fname, _) => fname
  };

let string_of_action = (action) =>
  switch (action) {
  | NewEntity({name, id, values}) =>
    let values_str = String.concat("\n") @@ List.map(((name, expr)) => [%string "    %{name} =  %{string_of_expr expr}"], values);
    [%string "  NewEntity %{name}[%{string_of_expr id}]{\n%{values_str}\n  }"]

  | UpdateEntity({name, id, values}) =>
    let values_str = String.concat("\n") @@ List.map(string_of_field_mod, values);
    [%string "  UpdateEntity %{name}[%{string_of_expr id}]{\n%{values_str}\n  }"]

  | NewTemplate({name, address}) =>
    [%string "  NewTemplate %{name}[%{string_of_expr address}]"]
  }

let string_of_toplevel = (toplevel) =>
  switch (toplevel) {
  | Interface({name, fields}) =>
    let fields_str = String.concat("\n") @@ List.map(((name, typ)) => [%string "%{name}: %{string_of_gql_type typ}"], fields);
    [%string "Interface %{name} {\n%{fields_str}\n}"]

  | Entity({name, fields, interface}) =>
    let interface_str = Option.value(~default="") @@ Option.map(intf_name => [%string "implements %{intf_name} "], interface);
    let fields_str = String.concat("\n") @@ List.map(((name, typ)) => [%string "%{name}: %{string_of_gql_type typ}"], fields);
    [%string "Entity %{name} %{interface_str}{\n%{fields_str}\n}"]

  | DataSource({name, address, start_block, abi}) =>
    [%string "DataSource %{name} address: %{string_of_expr address}, start_block: %{string_of_expr start_block}, abi: %{string_of_expr abi}"]

  | Template({name, abi}) =>
    [%string "Template %{name} abi: %{string_of_expr abi}"]

  | EventHandler({event, source, actions}) =>
    let actions_str = String.concat("\n") @@ List.map(string_of_action, actions);
    [%string "EventHandler %{event} from %{source} {\n%{actions_str}\n}"]

  | CallHandler({call, source, actions}) =>
    let actions_str = String.concat("\n") @@ List.map(string_of_action, actions);
    [%string "CallHandler %{call} from %{source} {\n%{actions_str}\n}"]
  
  | _ => ""
  };

let string_of_document = (document) =>
  String.concat("\n") @@ List.map(string_of_toplevel, document);

// ================================================================
// Constructors
// ================================================================ 

// Toplevel
let mk_interface = (name, fields) =>
  Interface({name, fields});

let mk_entity = (~interface=?, name, fields) =>
  Entity({name, interface, fields});

let mk_data_source = (name, address, start_block, abi) =>
  DataSource({name, address: Literal(Address(address)), start_block: Literal(Int(start_block)), abi: Literal(String(abi))});

let mk_template = (name, abi) =>
  Template({name, abi: Literal(String(abi))});

let mk_event_handler = (event, source, actions) =>
  EventHandler({event, source, actions});

let mk_call_handler = (call, source, actions) =>
  CallHandler({call, source, actions});

// Actions
let mk_new_entity = (name, id, values) =>
  NewEntity({name, id, values});

let mk_update_entity = (name, id, values) =>
  UpdateEntity({name, id, values});

let mk_new_template = (name, address) =>
  NewTemplate({name, address});

// Expressions
let mk_var = (~path=[], name) =>
  Variable(path, name);

// ================================================================
// Unwrappers
// ================================================================ 
let expr_of_field_mod = 
  fun
  | Increment(_)
  | Decrement(_) => None
  | PlusEq(_, expr)
  | MinusEq(_, expr)
  | Assign(_, expr) => Some(expr);

let exprs_of_action = (action) =>
  switch (action) {
  | NewEntity({id, values, _}) =>
    [id, ...List.map(snd, values)]
  | UpdateEntity({id, values, _}) =>
    [id, ...List.filter_map(expr_of_field_mod, values)]
  | NewTemplate({address, _}) =>
    [address]
  };

let exprs_of_handler = (toplevel) => 
  switch (toplevel) {
  | EventHandler({actions, _}) 
  | CallHandler({actions, _}) =>
    List.flatten @@ List.map(exprs_of_action, actions)
  | _ => []
  };

let rec identifiers_of_expr = (expr) =>
  switch (expr) {
  | Neg(e) => identifiers_of_expr(e)
  | Add(e1, e2) 
  | Sub(e1, e2)
  | Mul(e1, e2)
  | Div(e1, e2)
  | Index(e1, e2) => List.flatten @@ [identifiers_of_expr(e1), identifiers_of_expr(e2)]
  | Variable(var_path, name) => [name, ...var_path];
  | Literal(_) => []
  | Apply(e, args) =>
    List.flatten @@ [
      identifiers_of_expr(e),
      ...List.map(identifiers_of_expr, args)
    ]
  };

let identifiers_of_action = (action) =>
  List.cons(
    switch (action) {
    | NewEntity({name, _})
    | UpdateEntity({name, _})
    | NewTemplate({name, _}) => name
    },
    List.flatten @@ List.map(
      identifiers_of_expr,
      exprs_of_action(action)
    )
  );

let identifiers_of_handler = (toplevel) =>
  switch (toplevel) {
  | EventHandler({actions, _}) 
  | CallHandler({actions, _}) =>
    List.flatten @@ List.map(identifiers_of_action, actions)
  | _ => []
  };

let identifiers_of_source = (document, source_name) =>
  List.sort_uniq(String.compare) @@ List.flatten @@ List.map((toplevel) =>
    switch (toplevel) {
    | EventHandler({source, _}) as handler
    | CallHandler({source, _}) as handler when source == source_name => identifiers_of_handler(handler)
    | _ => []
    },
    document
  );

let event_handlers_of_source = (document, source_name) => 
  List.filter_map((toplevel) =>
    switch (toplevel) {
    | EventHandler({source, _} as handler_data) when source == source_name => Some(handler_data)
    | _ => None
    },
    document
  );

let call_handlers_of_source = (document, source_name) => 
  List.filter_map((toplevel) =>
    switch (toplevel) {
    | CallHandler({source, _} as handler_data) when source == source_name => Some(handler_data)
    | _ => None
    },
    document
  );

// ================================================================
// Filter Constructors
// ================================================================ 
let mapping_filter = (handler_filter, document, name) =>
  List.filter_map((toplevel) =>
    switch (toplevel) {
    | EventHandler({source, actions, _}) 
    | CallHandler({source, actions, _}) when source == name =>
      Some(handler_filter(actions))
    | _ => None
    },
    document
  )
  |> List.flatten
  |> List.sort_uniq(String.compare);

let rec expr_filter = (filter, expr) =>
  switch (expr) {
  | Neg(e) => expr_filter(filter, e)
  | Add(e1, e2) 
  | Sub(e1, e2)
  | Mul(e1, e2)
  | Div(e1, e2) => List.flatten @@ [expr_filter(filter, e1), expr_filter(filter, e2)]
  | Variable(var_path, name) => 
    let items = List.filter(filter, var_path);
    filter(name) ? [name, ...items] : items;
  | Literal(_) => []
  | Index(e1, e2) => List.flatten @@ [expr_filter(filter, e1), expr_filter(filter, e2)]
  | Apply(e, args) =>
    List.flatten @@ [
      expr_filter(filter, e),
      ...List.map(expr_filter(filter), args)
    ]
  };

// ================================================================
// Entities filters
// ================================================================ 
let entities_of_expr = (db) =>
  expr_filter((name) => Database.entity_opt(db, name) != None);

let entities_of_handler = (db, actions) => 
  List.filter_map(
    fun
    | NewEntity({name, _}) as action => 
      List.cons(
        name, 
        List.flatten @@ List.map(entities_of_expr(db), exprs_of_action(action))
      )
      |> Option.some
    | UpdateEntity({name, values, _}) => 
      List.cons(
        name, 
        List.flatten @@ List.map((expr) => entities_of_expr(db, expr), List.filter_map(expr_of_field_mod, values))
      )
      |> Option.some
    | NewTemplate(_) => None,
    actions
  )
  |> List.flatten
  |> List.sort_uniq(String.compare);

let entities_of_mapping = (db) => mapping_filter(entities_of_handler(db));

// ================================================================
// Contract filters
// ================================================================ 

let contracts_of_expr = (db) =>
  expr_filter((name) => Database.contract_opt(db, name) != None);

let contracts_of_handler = (db, actions) => 
  List.filter_map(
    fun
    | NewEntity({name, values, _}) => 
      List.cons(
        name, 
        List.flatten @@ List.map(((_, expr)) => entities_of_expr(db, expr), values)
      )
      |> Option.some
    | UpdateEntity({name, values, _}) => 
      List.cons(
        name, 
        List.flatten @@ List.map((expr) => entities_of_expr(db, expr), List.filter_map(expr_of_field_mod, values))
      )
      |> Option.some
    | NewTemplate(_) => None,
    actions
  )
  |> List.flatten
  |> List.sort_uniq(String.compare);

// ================================================================
// Template filters
// ================================================================ 
let templates_of_handler = (actions) =>
  List.filter_map(
    fun
    | NewEntity(_)
    | UpdateEntity(_) => None
    | NewTemplate({name, _}) => Some(name),
    actions
  )
  |> List.sort_uniq(String.compare);

let templates_of_mapping = mapping_filter(templates_of_handler);