open Parsetree;

type abi_typ = 
  | ABIEvent({
    params: list((string, sol_type))
  })
  | ABIFunction({
    mutability: mutability,
    inputs: list((string, sol_type)),
    outputs: list((string, sol_type))
  })
and mutability =
  | Pure
  | View
  | Payable
  | NonPayable;

type typ = 
  | TStruct({
    fields: list((string, typ))
  })
  | TIndexable({
    typ: [`Contract | `Entity],
    fields: list((string, typ))
  })
  | TFun({
    inputs: list(typ),
    outputs: list(typ)
  })
  | TId
  | TString
  | TBytes
  | TInt
  | TBigInt
  | TFloat
  | TBigDecimal
  | TBool
  | TAddress
  | TList(typ)
  | TNonNull(typ);

type environment = list((string, typ));

let rec string_of_typ = (typ) => 
  switch (typ) {
  | TStruct(_)
  | TIndexable(_) => "struct not implemented"
  | TFun({inputs, outputs}) =>
    let inputs_str = String.concat(" => ") @@ List.map(string_of_typ, inputs);
    let outputs_str = String.concat(", ") @@ List.map(string_of_typ, outputs);
    [%string "%{inputs_str} => (%{outputs_str})"]
  | TId           => "ID"
  | TInt          => "int"
  | TBigInt       => "bigint"
  | TFloat        => "float"
  | TBigDecimal   => "bigdecimal"
  | TString       => "string"
  | TBytes        => "bytes"
  | TBool         => "bool"
  | TAddress      => "address"
  | TList(typ)    => [%string "[%{string_of_typ typ}]"]
  | TNonNull(typ) => [%string "%{string_of_typ typ}!"];
  };