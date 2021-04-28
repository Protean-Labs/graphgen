// get create
open Ast;
type field = {
  name: string,
  field_type: Ast.typ
};

let typescript_type_init_value = fun
  | IntT(256) => "BigIntZero"
  | StringT => "string"
  | _ => "placeholder"
;

let typescript_field_init = (field) => {
  [%string 
    "let entity.%{field.name} = %{ typescript_type_init_value(field.field_type) }\n"
  ];
};

let get_create = (name, fields) => {
  [%string
    "export function getCreate%{name}(id: String): %{name} {
      let entity = %{name}.load(id);
      if (entity == null) {
        entity = new %{name}(id);
        %{ fields |> List.map(typescript_field_init) |> String.concat("") }
        entity.save();
      } 
      return entity as %{name};
    }"
  ];
};

// event store