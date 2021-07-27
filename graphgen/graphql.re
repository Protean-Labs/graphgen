type typ =
  | BigInt
  | BigDecimal
  | String
  | Int
  | Float
  | Bytes
  | Boolean
  | List(typ)
  | NonNull(typ)
  | Object(string)
;

let rec convert_ast_type = Parsing.Ast.(fun
  | BytesT => Bytes
  | FbytesT(_) => Bytes
  | UintT(n) when n <= 32 => Int
  | UintT(_) => BigInt
  | IntT(n) when n <= 32 => Int
  | IntT(_) => BigInt
  | StringT => String
  | BoolT => Boolean
  | AddressT => Bytes
  | FixedT => Float
  | UfixedT => Float
  | ArrayT(t') => List(convert_ast_type(t'))
);

let rec string_of_typ = fun
  | BigInt => "BigInt"
  | BigDecimal => "BigDecimal"
  | String => "String"
  | Int => "Int"
  | Float => "Float"
  | Bytes => "Bytes"
  | Boolean => "Boolean"
  | List(t') => [%string "[%{string_of_typ t'}]"]
  | NonNull(t') => [%string "%{string_of_typ t'}!"]
  | Object(name) => name
;

let rec default_value = fun
  | BigInt => "BigIntZero"
  | BigDecimal => "BigDecimalZero"
  | String => "\"\""
  | Int => "0"
  | Float => "0.0"
  | Bytes => "\"\""
  | Boolean => "false"
  | List(t') => [%string "[%{default_value t'}]"]
  | NonNull(t') => [%string "%{default_value t'}!"]
  | Object(_) => "null"
;

let non_null = (t) => NonNull(t);
let list = (t) => List(t);
let non_null_list = (t) => NonNull(List(NonNull(t)));

