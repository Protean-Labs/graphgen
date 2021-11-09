open Jingoo;

open Gg_script;
open Database;
open Parsetree;
open Parsetree_util;

let logger = Easy_logging.Logging.make_logger("Transpiler.Manifest", Debug, [Cli(Debug)]);

let event_signature = (name, {params}) => 
  String.concat(",") @@ List.map(((_, typ)) => 
    string_of_sol_type(typ),
    params
  )
  |> (params) => [%string "%{name}(%{params})"];

let call_signature = (name, {inputs, _}) => 
  String.concat(",") @@ List.map(((_, typ)) => 
    string_of_sol_type(typ),
    inputs
  )
  |> (inputs) => [%string "%{name}(%{inputs})"];

let event_handlers_template = {|
      eventHandlers:
      {%- for event_handler in event_handlers %}
        - event: {{ event_handler.event_signature }}
          handler: handle{{ event_handler.event_name }}
      {%- endfor -%}
|};

let model_of_event_handlers = (events) =>
  Jg_types.(Tlist(
    List.map(((name, event)) => 
      Tobj([
        ("event_name", Tstr(name)),
        ("event_signature", Tstr(event_signature(name, event)))
      ]),
      events
    )
  ));

let call_handlers_template = {|
      callHandlers:
      {%- for call_handler in call_handlers %}
        - call: {{ call_handler.call_signature }}
          handler: handle{{ call_handler.call_name }}
      {%- endfor -%}
|};

let model_of_call_handlers = (calls) =>
  Jg_types.(Tlist(
    List.map(((name, call)) => 
      Tobj([
        ("call_name", Tstr(name)),
        ("call_signature", Tstr(call_signature(name, call)))
      ]),
      calls
    )
  ));

let mapping_template = {|
    mapping:
      kind: ethereum/events
      apiVersion: 0.0.5
      language: wasm/assemblyscript
      file: ./src/mappings/{{ mapping.contract_name }}.ts
      entities:
        {%- for entity_name in mapping.entities %}
        - {{ entity_name }}
        {%- endfor %}
      abis:
        {%- for abi in mapping.abis %}
        - name: {{ abi }}
          file: ./abis/{{ abi }}.json
        {%- endfor -%}
|};

let model_of_mapping = (source, entities, abis) =>
  Jg_types.(Tobj([
    ("contract_name", Tstr(source)),
    ("entities", Tlist(
      List.map((entity) => Tstr(entity), entities)
    )),
    ("abis", Tlist(
      List.map((abi) => Tstr(abi), abis)
    ))
  ]));

let data_source_template = {|
  - kind: ethereum/contract
    name: {{ data_source.name }}
    network: {{ data_source.network }}
    source:
      address: '{{ data_source.address }}'
      abi: {{ data_source.contract.abi }}
      startBlock: {{ data_source.start_block }}{% -%}
|};

let model_of_data_source = (name, network, address, abi, start_block) =>
  Jg_types.(Tobj([
    ("name", Tstr(name)),
    ("network", Tstr(network)),
    ("address", Tstr(address)),
    ("contract", Tobj([
      ("abi", Tstr(abi)),
    ])),
    ("start_block", Tstr(start_block))
  ]));

let template_template = {|
  - kind: ethereum/contract
    name: {{ template.name }}
    network: {{ template.network }}
    source:
      abi: {{ template.contract.name }}
|};

let model_of_template = (name, network, abi) =>
  Jg_types.(Tobj([
    ("name", Tstr(name)),
    ("network", Tstr(network)),
    ("abi", Tstr(abi)),
  ]));

// data_source or template
// mapping
// event_handlers
// call_handlers

let transpile = ((document, db, _)) => {
  // DataSources
  let data_sources = 
    List.map(
      ((name, data_source: data_source_t)) => 
        [
          // DataSource
          Option.some @@ {
            model_of_data_source(name, "mainnet", data_source.address, Database.abi(db, data_source.abi).name, data_source.start_block)
            |> (model) => Jg_template.from_string(data_source_template, ~models=[("data_source", model)])
          },

          // Mapping
          Option.some @@ {
            identifiers_of_source(document, name) |> (idents) => {
              let entities = 
                List.filter((ident) => switch (Database.type_of(db, ident)) { | `Entity => true | _ => false }, idents)

              // logger#debug("DataSource %s entities: %s", name, String.concat(", ", entities));

              let abis = 
                List.filter_map((ident) => 
                  switch (Database.type_of(db, ident)) { 
                  | `ABI  => Some(name)
                  | `DataSource => Some(Database.data_source(db, ident).abi)
                  | `Template => Some(Database.template(db, ident).abi)
                  | _ => None
                  }, 
                  idents
                )

              // logger#debug("DataSource %s abis: %s", name, String.concat(", ", abis));

              model_of_mapping(name, entities, [Database.data_source(db, name).abi, ...abis])
              |> (model) => Jg_template.from_string(mapping_template, ~models=[("mapping", model)])
            }
          },

          // Event handlers
          switch (event_handlers_of_source(document, name)) {
          | [] => None
          | events => 
            List.map(({event, _}) =>
              (event, List.assoc(event, Database.abi(db, data_source.abi).events)),
              events
            )
            |> model_of_event_handlers
            |> (model) => Jg_template.from_string(event_handlers_template, ~models=[("event_handlers", model)])
            |> Option.some
          },

          // Call handlers
          switch (call_handlers_of_source(document, name)) {
          | [] => None
          | calls => 
            List.map(({call, _}) =>
              (call, List.assoc(call, Database.abi(db, data_source.abi).calls)),
              calls
            )
            |> model_of_call_handlers
            |> (model) => Jg_template.from_string(call_handlers_template, ~models=[("call_handlers", model)])
            |> Option.some
          },
        ]
        |> List.filter_map(s => s)
        |> String.concat(""),
      db.data_sources
    );

  [%string {|dataSources:%{String.concat "\n" data_sources}|}];
};

let test = () => {
  open Rresult;

  R.get_ok @@ {
    File.read(Fpath.v("test/gravatar.gg")) >>= (gg_src) =>
    Gg_script.parse(gg_src) >>| (ast) =>
    Validate.tcheck(ast)    |> (document) =>
    print_endline @@ transpile(document)
  }
};