// open Jingoo;

// open Gg_script.Parsetree;

let event_handlers_template = {|
      eventHandlers:
      {%- for event_handler in event_handlers %}
        - event: {{ event_handler.event.signature }}
          handler: handle{{ event_handler.event.name }}
      {%- endfor -%}
      {%- endif %}
|};

let call_handlers_template = {|
      callHandlers:
      {%- for call_handler in call_handlers %}
        - call: {{ call_handler.call_.signature }}
          handler: handle{{ call_handler.call_.name }}
      {%- endfor -%}
      {%- endif %}
|};

let mapping_template = {|
    mapping:
      kind: ethereum/events
      apiVersion: 0.0.5
      language: wasm/assemblyscript
      file: ./src/mappings/{{ mapping.contract.name }}.ts
      entities:
        - {{ mapping.contract.name }}
        {%- for entity in mapping.relatedEntities %}
        - {{ entity.name }}
        {%- endfor %}
      abis:
        - name: {{ mapping.contract.name }}
          file: ./abis/{{ mapping.contract.name }}.json{% for contract in mapping.relatedContracts %}
        - name: {{ contract.name }}
          file: ./abis/{{ contract.name }}.json{% endfor %}
      {{ mapping.event_handlers_block }}
      {{ mapping.call_handlers_block }}
|};

let data_source_template = {|
  - kind: ethereum/contract
    name: {{ data_source.name }}
    network: {{ data_source.network }}
    source:
      address: {{ data_source.address }}
      abi: {{ data_source.contract.abi }}
      startBlock: {{ data_source.start_block }}
    {{ data_source.mapping_block }}
|};

let template_template = {|
  - kind: ethereum/contract
    name: {{ template.name }}
    network: {{ template.network }}
    source:
      abi: {{ template.contract.name }}
    {{ template.mapping_block }}
|};