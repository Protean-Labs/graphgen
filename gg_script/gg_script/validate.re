open Parsetree;
open Parsetree_util;
open Typetree;

// let logger = Easy_logging.Logging.make_logger("Typetree", Debug, [Cli(Debug)]);

exception Type_error(string);
exception ABI_error(string);

// ================================================================
// Type Helpers
// ================================================================ 
let rec typ_of_sol_type = fun
  | SOLAddress              => TAddress
  | SOLBool                 => TBool
  | SOLString               => TString
  | SOLFixed                => TFloat
  | SOLUfixed               => TFloat
  | SOLBytes                => TBytes
  | SOLFbytes(_)            => TBytes
  | SOLInt(n) when n > 32   => TBigInt
  | SOLInt(_)               => TInt
  | SOLUint(n) when n > 32  => TBigInt
  | SOLUint(_)              => TInt
  | SOLArray(typ)           => TList(typ_of_sol_type(typ))
  | SOLIndexed(typ)         => typ_of_sol_type(typ);

let rec typ_of_gql_type = (~level=0, db, gql_typ) =>
  if (level > 6) {
    TStruct({fields: []})
  } else {
    switch (gql_typ) {
    | GQLId                 => TId
    | GQLBytes              => TBytes
    | GQLString             => TString
    | GQLInt                => TInt
    | GQLBigInt             => TBigInt
    | GQLFloat              => TFloat
    | GQLBigDecimal         => TBigDecimal
    | GQLBoolean            => TBool
    | GQLList(typ)          => TList(typ_of_gql_type(~level=level+1, db, typ))
    | GQLNonNull(typ)       => TNonNull(typ_of_gql_type(~level=level+1, db, typ))
    | GQLObject(name)       => 
      switch (Database.(entity_opt(db, name), interface_opt(db, name))) {
      | (Some({fields, _}), _)
      | (None, Some({fields, _})) =>
        TStruct({fields: List.map(((name, field)) => (name, typ_of_gql_type(~level=level+1, db, field)), fields)})
      | (None, None) =>
        raise(Type_error([%string "typ_of_gql_type: entity or interface %{name} not found"]))
      }
    }
  };

  let rec type_compatible = (target, typ) =>
    switch (target, typ) {
    // NonNull
    | (TNonNull(typ), typ') => type_compatible(typ, typ')
    
    // ID
    | (TId, TId | TInt | TBigInt | TString | TAddress | TBytes) => true

    // String
    | (TString, TInt | TBigInt | TFloat | TBigDecimal | TId | TString | TBytes | TAddress) => true

    // Bytes
    | (TBytes, TBytes | TAddress) => true

    // Int
    | (TInt, TInt) => true

    // BigInt
    | (TBigInt, TInt | TBigInt) => true

    // Float
    | (TFloat, TInt | TFloat) => true

    // BigDecimal
    | (TBigDecimal, TInt | TBigInt | TFloat | TBigDecimal) => true

    // Bool
    | (TBool, TBool) => true

    // TAddress
    | (TAddress, TAddress) => true

    // Object
    | (
      TStruct({fields}),
      TStruct({fields: fields'}) | TIndexable({fields: fields', _})
    ) => List.fold_left2((acc, (_, field), (_, field')) => acc && field == field', true, fields', fields)

    | _ => false
    };

let number_like = (typ) => 
  switch (typ) {
  | TInt | TBigInt | TFloat | TBigDecimal => true
  | _ => false
  };

let string_like = (typ) =>
  switch (typ) {
  | TString | TBytes | TAddress => true
  | _ => false
  };

let lookup = (env, path, name) => {
  // logger#debug("lookup: path = %s, name = %s", String.concat(".", path), name);
  let namespace = 
    List.fold_left((acc, name') => 
      switch (List.assoc_opt(name', acc)) {
      | Some(TStruct({fields}))
      | Some(TIndexable({fields, _})) => fields
      | Some(_) => raise(Type_error([%string "%{name'} is not a struct!"]))
      | None => raise(Type_error([%string "Unbound variable %{name'}"]))
      }
    , env, path);
  
  switch (List.assoc_opt(name, namespace)) {
  | Some(typ) => typ
  | None => raise(Type_error([%string "lookup: %{name} not found"]))
  };
};

let block_struct = 
  TStruct({fields: [
    ("hash", TBytes),
    ("parentHash", TBytes),
    ("unclesHash", TBytes),
    ("author", TAddress),
    ("stateRoot", TBytes),
    ("transactionsRoot", TBytes),
    ("receiptsRoot", TBytes),
    ("number", TBigInt),
    ("gasUsed", TBigInt),
    ("gasLimit", TBigInt),
    ("timestamp", TBigInt),
    ("difficulty", TBigInt),
    ("totalDifficulty", TBigInt),
    ("size", TBigInt)
  ]});

let tx_struct = 
  TStruct({fields: [
    ("hash", TBytes),
    ("index", TBigInt),
    ("from", TAddress),
    ("to", TAddress),
    ("value", TBigInt),
    ("gasLimit", TBigInt),
    ("gasPrice", TBigInt),
    ("input", TBytes)
  ]});

let struct_of_abi_event = (params) => 
  TStruct({fields: [
    ("params", TStruct({fields: List.map(((name, typ)) => (name, typ_of_sol_type(typ)), params)})),
    ("addr", TAddress),
    ("logIndex", TBigInt),
    ("transactionLogIndex", TBigInt),
    ("transaction", tx_struct),
    ("block", block_struct)
  ]});

let struct_of_abi_call = (inputs, outputs) => 
  TStruct({fields: [
    ("inputs", TStruct({fields: List.map(((name, typ)) => (name, typ_of_sol_type(typ)), inputs)})),
    ("outputs", TStruct({fields: List.map(((name, typ)) => (name, typ_of_sol_type(typ)), outputs)})),
    // TODO: call fields
    // ("address", TAddress,
    // ("logIndex", TBigInt,
    // ("transactionLogIndex", TBigInt,
    // ("transaction", tx_struct),
    // ("block", block_struct)
  ]});

let struct_of_entity = (fields) =>
  TStruct({fields: fields});

let env_of_db = (db) =>
  Database.(
    List.fold_left((acc, (name, {fields, _})) => 
      [(name, TIndexable({typ: `Entity, fields: List.map(((name, typ)) => (name, typ_of_gql_type(db, typ)), fields)})), ...acc], 
      [], 
      db.entities
    )
    |> List.fold_left((acc, (name, {contract: {functions, _}, _}: Database.data_source_t)) => 
      [(name, TStruct({
        fields: List.map(((name, {inputs, outputs})) => (name, TFun({
          inputs: List.map(((_, typ)) => typ_of_sol_type(typ), inputs), 
          outputs: List.map(((_, typ)) => typ_of_sol_type(typ), outputs)
        })), functions)
      })), ...acc], 
      _, 
      db.data_sources
    )
    |> List.fold_left((acc, (name, {contract: {functions, _}}: Database.template_t)) => 
      [(name, TIndexable({
        typ: `Contract, 
        fields: List.map(((name, {inputs, outputs})) => (name, TFun({
          inputs: List.map(((_, typ)) => typ_of_sol_type(typ), inputs), 
          outputs: List.map(((_, typ)) => typ_of_sol_type(typ), outputs)
        })), functions)
      })), ...acc], 
      _, 
      db.templates
    )
    |> List.fold_left((acc, (name, {functions, _})) => 
      [(name, TIndexable({
        typ: `Contract,
        fields: List.map(((name, {inputs, outputs})) => (name, TFun({
          inputs: List.map(((_, typ)) => typ_of_sol_type(typ), inputs), 
          outputs: List.map(((_, typ)) => typ_of_sol_type(typ), outputs)
        })), functions)
      })), ...acc], 
      _, 
      db.contracts
    )
  );


let rec tcheck_expr = (db, env, expr) =>
  switch (expr) {
  | Neg(e) => 
    switch (tcheck_expr(db, env, e)) {
    | (TInt | TBigInt | TFloat | TBigDecimal) as typ => typ
    | typ => raise(Type_error([%string "-%{string_of_expr e}: expected number type, got %{string_of_typ typ}"]))
    };

  | Add(e1, e2) => 
    switch (tcheck_expr(db, env, e1), tcheck_expr(db, env, e2)) {
    | (typ1, typ2) when string_like(typ1) && type_compatible(TString, typ2) => TString 
    | (typ1, typ2) when string_like(typ2) && type_compatible(TString, typ1) => TString
    | (typ1, typ2) when string_like(typ1) || string_like(typ2) => raise(Type_error([%string "%{string_of_expr expr}: expected %{string_of_typ typ1}, got %{string_of_typ typ2}"]))
    | (typ1, typ2) when number_like(typ1) && type_compatible(typ1, typ2) => typ1
    | (typ1, typ2) when number_like(typ2) && type_compatible(typ2, typ1) => typ2
    | (typ1, typ2) => raise(Type_error([%string "%{string_of_expr expr}: expected %{string_of_typ typ1}, got %{string_of_typ typ2}"]))
    }
  // TODO: Basic operators
  // | Sub(e1, e2) 
  // | Mul(e1, e2) 
  // | Div(e1, e2) => 
  //   switch (tcheck_expr(db, env, e1), tcheck_expr(db, env, e2)) {
  //   | TGql(GQLInt | GQLBigInt | GQLFloat | GQLBigDecimal) as typ => typ
  //   | typ => raise(Type_error("new_entity: unexpected type for id. expected GQL::Int-like got %{string_of_typ typ}"))
  //   }

  | Variable(path, name) => lookup(env, path, name)
  | Index(e1, e2) => 
    let typ = tcheck_expr(db, env, e2);
    if (!type_compatible(TId, typ)) {
      raise(Type_error("new_entity: unexpected type for id. expected GQL::ID got %{string_of_typ typ}"))
    } else {
      tcheck_expr(db, env, e1)
    };
  | e => raise(Type_error([%string "tcheck_expr: not implemented. expr = %{string_of_expr e}"]))
  };

let tcheck = (document) => {
  let rec tcheck = (db, env, toplevel) => 
    switch (toplevel) {
    | Interface(_)
    | Entity(_)
    | DataSource(_)
    | Template(_)
    | Event(_)
    | Call(_) => ()

    | EventHandler({event, source, actions}) =>
      switch Database.(data_source_opt(db, source), template_opt(db, source)) {
      | (Some({contract, _}), _)
      | (None, Some({contract})) =>
        switch (List.assoc_opt(event, contract.events)) {
        | Some({params}) => 
          List.iter(tcheck_action(db, [("event_", struct_of_abi_event(params)), ...env]), actions)
        | None => raise(Type_error([%string "event_handler: event %{event} missing in %{source} ABI"]))
        }
      | _ => raise(Type_error([%string "event_handler: %{source} is not a data source or template"]))
      }

    | CallHandler({call, source, actions}) => 
      switch Database.(data_source_opt(db, source), template_opt(db, source)) {
      | (Some({contract, _}), _)
      | (None, Some({contract})) =>
        switch (List.assoc_opt(call, contract.calls)) {
        | Some({inputs, outputs}) => 
          List.iter(tcheck_action(db, [("call_", struct_of_abi_call(inputs, outputs)), ...env]), actions)
        | None => raise(Type_error([%string "call_handler: function %{call} missing in %{source} ABI"]))
        }
      | _ => raise(Type_error([%string "event_handler: %{source} is not a data source or template"]))
      }
    }
  and tcheck_action = (db, env, action) =>
    switch (action) {
    | NewEntity({name, id, values}) => 
      // Check that the entity exists
      switch (Database.entity_opt(db, name)) {
      | Some({fields, _}) =>
        // Tcheck id expression
        let typ = tcheck_expr(db, env, id);
        if (!type_compatible(TId, typ)) {
          raise(Type_error("new_entity: unexpected type for id. expected GQL::ID got %{string_of_typ typ}"))
        };

        // Check that all required entity fields are set
        List.iter(((fname, typ)) => 
          if (fname != "id") {
            switch (typ) {
            | GQLNonNull(_) =>
              switch (List.assoc_opt(fname, values)) {
              | None => raise(Type_error([%string "new_entity: required field %{fname} of entity %{name} not assigned!"]))
              | Some(expr) => 
                let typ' = tcheck_expr(db, env, expr);
                if (!type_compatible(typ_of_gql_type(db, typ), typ')) {
                  raise(Type_error([%string "new_entity: invalid type for field %{fname}. expected %{string_of_typ (typ_of_gql_type db typ)}, got %{string_of_typ typ'}"]))
                }
              }
            | _ => ()
            }
          }, 
          fields
        );

        // Check that all fields assigned to a value belongs to the entity
        List.iter(((fname, _)) => 
          switch (List.assoc_opt(fname, fields)) {
          | None => raise(Type_error([%string "new_entity: field %{fname} does not belong to entity %{name}"]))
          | Some(_) => ()
          }, 
          values
        );

      | _ => raise(Type_error([%string "new_entity: %{name} is not an entity"]))
      }

    | UpdateEntity({name, id, values}) =>
      // Check that the entity exists
      switch (Database.entity_opt(db, name)) {
      | Some({fields, _}) =>
        // Tcheck id expression
        let typ = tcheck_expr(db, env, id);
        if (!type_compatible(TId, typ)) {
          raise(Type_error("update: unexpected type for id. expected GQL::ID got %{string_of_typ typ}"))
        };

        // Check that all modified fields belong to the entity and type check
        List.iter((fmod) => 
          switch (fmod) {
          | Increment(fname)
          | Decrement(fname) =>
            switch (Option.map(typ_of_gql_type(db)) @@ List.assoc_opt(fname, fields)) {
            | Some(TNonNull(TInt | TBigInt) | TInt | TBigInt) => ()
            | None      => raise(Type_error([%string "update: field %{fname} does not belong to entity %{name}"]))
            | Some(typ) => raise(Type_error([%string "update: invalid type for field %{fname} %{string_of_typ typ}"]))
            }

          | PlusEq(fname, expr)
          | MinusEq(fname, expr) =>
            switch (Option.map(typ_of_gql_type(db)) @@ List.assoc_opt(fname, fields), tcheck_expr(db, env, expr)) {
            | (Some((TInt | TBigInt | TFloat | TBigDecimal) as typ), typ') when type_compatible(typ, typ') => ()
            | (Some((TInt | TBigInt | TFloat | TBigDecimal) as typ), typ') => 
              raise(Type_error([%string "update: invalid type for field %{fname}. expected %{string_of_typ typ}, got %{string_of_typ typ'}"]))
            | (Some(_), _) => 
              raise(Type_error("update: += and -= operators only possible for number fields"))
            | (None, _) => 
              raise(Type_error([%string "update: field %{fname} does not belong to entity %{name}"]))
            }

          | Assign(fname, expr) =>
            switch (Option.map(typ_of_gql_type(db)) @@ List.assoc_opt(fname, fields), tcheck_expr(db, env, expr)) {
            | (Some(typ), typ') when type_compatible(typ, typ') => ()
            | (Some(typ), typ') => 
              raise(Type_error([%string "update: invalid type for field %{fname}. expected %{string_of_typ typ}, got %{string_of_typ typ'}"]))
            | (None, _) => 
              raise(Type_error([%string "update: field %{fname} does not belong to entity %{name}"]))
            }
          },
          values
        );

      | _ => raise(Type_error([%string "update: %{name} is not an entity"]))
      }


    // TODO: NewTemplate
    // | NewTemplate({name, address}) =>
    //   switch (lookup(env, [], name)) {
    //   | 
    //   }
    | _ => raise(Type_error("tcheck_action: not implemented"))
    };

  let db = Database.make(document);
  let env = env_of_db(db);
  List.iter(tcheck(db, env), document);
  (document, db, env)
};