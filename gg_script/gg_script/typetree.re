open Parsetree;

type abi_typ = 
  | ABIEvent({
    params: list((string, sol_type))
  })
  | ABIFunction({
    inputs: list((string, sol_type)),
    outputs: list((string, sol_type))
  })
and mutability = 
  | 

type typ = 
  | Interface({
    fields: list((string, gql_type))
  })
  | Entity({
    fields: list((string, gql_type))
  })
  | DataSource({

  })

// let tcheck = ()