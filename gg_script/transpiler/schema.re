open Jingoo;

open Gg_script.Parsetree;

let interface_template = {|
interface {{ interface.name }} {
  {%- for field in interface.fields %}
  {{ field.name }}: {{ field.type }}
  {%- endfor %}
}
|};

let entity_template = {|
type {{ entity.name }}{% if entity.interface != None %} implements {{ entity.interface }}{% endif %} @entity {
  {%- for field in entity.fields %}
  {{ field.name }}: {{ field.type }}
  {%- endfor %}
}
|};

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

let model_of_interface = (name, fields) =>
  Jg_types.Tobj([
    ("name", Tstr(name)),
    ("fields", Tlist(List.map(
      ((name, typ)) => Jg_types.Tobj([
        ("name", Tstr(name)),
        ("type", Tstr(string_of_gql_type(typ)))
      ]),
      fields
    )))
  ]);

let model_of_entity = (name, fields, interface) =>
  Jg_types.Tobj([
    ("name", Tstr(name)),
    ("fields", Tlist(
      List.map(((name, typ)) => Jg_types.Tobj([
        ("name", Tstr(name)),
        ("type", Tstr(string_of_gql_type(typ)))
        ]),
        fields
      )
    )),
    ("interface", switch (interface) { | None => Tnull | Some(intf) => Tstr(intf)})
  ]);

let transpile = ((_, db, _)) =>
  Gg_script.Database.(
    []
    // Transpile interfaces
    |> List.fold_left(
      (acc, (name, {fields}: interface_t)) => 
        [Jg_template.from_string(interface_template, ~models=[("interface", model_of_interface(name, fields))]), ...acc],
      _,
      db.interfaces
    )
    // Transpile entities
    |> List.fold_left(
      (acc, (name, {fields, interface})) => 
        [Jg_template.from_string(entity_template, ~models=[("entity", model_of_entity(name, fields, interface))]), ...acc],
      _,
      db.entities
    )
  )
  // Concat everything
  |> String.concat("");