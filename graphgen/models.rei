/** [manifest_models(sg)] returns Jingoo models generated from the subgraph [sg] 
    used to generate the subgraph manifest from the associated Jinja2 template. */
let manifest_models: Subgraph.t => Generator.models;

/** [schema_models(sg)] returns Jingoo models generated from the subgraph [sg]
    used to generate the GraphQL schema from the associated Jinja2 template. */
let schema_models: Subgraph.t => Generator.models;

/** [data_sources_models(sg)] returns a list of tuples [(key, models)], one 
    for each dataSource in the subgraph [sg]. [key] is a string indicating 
    the name of the dataSource. [models] is the Jingoo models generated from 
    each dataSource in the subgraph [sg] and required in the Jinja2 template. */
let data_sources_models: Subgraph.t => list((string, Generator.models));

/** [templates_models(sg)] returns a list of tuples [(key, models)], one 
    for each template in the subgraph [sg]. [key] is a string indicating 
    the name of the template. [models] is the Jingoo models generated from 
    each dataSource in the subgraph [sg] and required in the Jinja2 template. */
let templates_models: Subgraph.t => list((string, Generator.models));