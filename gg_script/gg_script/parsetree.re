type gql_type = 
  | GQLId
  | GQLBytes
  | GQLString
  | GQLInt
  | GQLBigInt
  | GQLFloat
  | GQLBigDecimal
  | GQLBoolean
  | GQLList(gql_type)
  | GQLNonNull(gql_type)
  | GQLObject(string);

type sol_type = 
  | SOLAddress
  | SOLBool
  | SOLString
  | SOLFixed
  | SOLUfixed
  | SOLBytes
  | SOLFbytes(int)
  | SOLInt(int)
  | SOLUint(int)
  | SOLArray(sol_type)
  | SOLIndexed(sol_type);

type literal = 
  | String(string)
  | Bytes(string)
  | Int(int)
  | BigInt(string)
  | Float(float)
  | BigDecimal(string)
  | Bool(bool)
  | Address(string);

type expr = 
  | Neg(expr)
  | Add(expr, expr)
  | Sub(expr, expr)
  | Mul(expr, expr)
  | Div(expr, expr)
  | Variable(list(string), string)
  | Literal(literal)
  | Index(expr, expr)         // e1[e2]
  | Apply(expr, list(expr));

type field_mod = 
  | Increment(string)
  | Decrement(string)
  | PlusEq(string, expr)
  | MinusEq(string, expr)
  | Assign(string, expr);

type action = 
  | NewEntity({
    name: string,
    id: expr,
    values: list((string, expr))
  })
  | UpdateEntity({
    name: string,
    id: expr,
    values: list(field_mod)
  })
  | NewTemplate({
    name: string,
    address: expr
  // })
  // | Def({
  //   name: string,
  //   value: expr
  });

type event_handler_t = {
  event: string,
  source: string,
  actions: list(action)
};

type call_handler_t = {
  call: string,
  source: string,
  actions: list(action)
};

type toplevel =
  // Graphql 
  | Interface({
    name: string,
    fields: list((string, gql_type))
  })
  | Entity({
    name: string,
    interface: option(string),
    fields: list((string, gql_type))
  })
  
  // Manifest
  | DataSource({
    name: string,
    abi: string,
    address: expr,
    start_block: expr,
  })
  | Template({
    name: string,
    abi: string
  })

  // Solidity
  | ABI({
    name: string, 
    file: expr
  })

  // Assemblyscript
  | EventHandler(event_handler_t)
  | CallHandler(call_handler_t);
  // })
  // // Actions to run on subgraph startup
  // | Init({
  //   actions: list(action)
  // })
  // | GlobalDef({
  //   name: string,
  //   value: expr
  

type document = list(toplevel);