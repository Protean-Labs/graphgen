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
  | SOLArray(sol_type);

type literal = 
  | Int(int)
  | Float(float)
  | Address(string)
  | String(string)
  | Bool(bool);

type expr = 
  | Neg(expr)
  | Add(expr, expr)
  | Sub(expr, expr)
  | Mul(expr, expr)
  | Div(expr, expr)
  | Variable(list(string), string)
  | Literal(literal)
  | NewEntity({
    name: string,
    id: expr,
    values: list((string, expr))
  })
  | UpdateEntity({
    name: string,
    id: expr,
    values: list((string, expr))
  })
  | NewTemplate({
    name: string,
    address: expr
  });

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
    address: expr,
    start_block: expr,
    abi: expr
  })
  | Template({
    name: string,
    abi: expr
  })
  // Solidity
  | Event({
    name: string,
    fields: list((string, sol_type))
  })
  | Call({
    name: string,
    inputs: list((string, sol_type)),
    outputs: list((string, sol_type))
  })
  // Assemblyscript
  | EventHandler({
    event: string,
    source: string,
    actions: list(expr)
  })
  | CallHandler({
    call: string,
    source: string,
    actions: list(expr)
  });