/** [generate_directories(output_dir)] generates the subgraph directory structure at location [output_dir] and returns nothing. */
let generate_directories: string => result(bool, [> `Msg(string)]);

/** Type of Jingoo (Jinja2) models. */
type models = list((string, Jingoo.Jg_types.tvalue));

/** [single_file(template_path, dest_path, f, sg)] generates a new file at [dest_path]
    using the template located at [template_path]. The models required by the template are 
    generated from the {!Subgraph.t} [sg] using the function [f]. */
let single_file: string => string => (Subgraph.t => models) => Subgraph.t => result(unit, [> `Msg(string)]);

/** [multi_file(template_path, f_dest_path, f, sg)] generates a new file at 
    [f_dest_path(key)] for each tuple [(key, models)] returned by [f(sg)] using the 
    template located at [template_path]. */
let multi_file: string => ('a => string) => (Subgraph.t => list(('a, models))) => Subgraph.t => result(unit, [> `Msg(string)]);
