%{
  open Ast
%}

%token <string> EVENT
%token <string> FUNCTION

%token <string> GG GG_HANDLER
%token EOF

%start <Ast.t> document
%%

document: list(elements) EOF { List.filter_map (function s -> s) $1 }

elements:
  | gg = GG
    { Some (Definitions (Gg_script.parse_document (Scanf.unescaped gg))) }

  | gg = GG_HANDLER name = EVENT
    { Some (EventHandler (name, Gg_script.parse_actions (Scanf.unescaped gg))) }

  | gg = GG_HANDLER name = FUNCTION
    { Some (CallHandler (name, Gg_script.parse_actions (Scanf.unescaped gg))) }

  | EVENT       { None }
  | FUNCTION    { None }