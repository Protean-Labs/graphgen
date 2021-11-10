type element = 
  | CallHandler(string, list(Gg_script.Parsetree.action))
  | EventHandler(string, list(Gg_script.Parsetree.action))
  | Definitions(list(Gg_script.Parsetree.toplevel));

type t = list(element);

let string_of_ast = (ast) =>
  String.concat("\n") @@ List.map((element) => 
    switch (element) {
    | CallHandler(name, actions) =>
      let actions_str = String.concat("\n") @@ List.map(Gg_script.Parsetree_util.string_of_action, actions);
      [%string "CallHandler %{name}\n%{actions_str}"]
    | EventHandler(name, actions) =>
      let actions_str = String.concat("\n") @@ List.map(Gg_script.Parsetree_util.string_of_action, actions);
      [%string "EventHandler %{name}\n%{actions_str}"]
    | Definitions(defs) =>
      Gg_script.Parsetree_util.string_of_document(defs)
    },
    ast
  );