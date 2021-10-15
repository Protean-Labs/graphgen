%{
  open Parsetree

  exception Parsing_error of string

  let parse_int_postfix base s = 
    if (base = s) then 256
    else
      String.sub s (String.length base) (String.length s - String.length base)
      |> int_of_string

  (*
  let fold_vpath id var =
    match var with
    | Variable (path, id') -> Variable (id::path, id')
    | _ -> raise (Parsing_error "fold_vpath")
  *)

  let fmt_data_source name params = 
    DataSource {
      name;
      address=List.assoc "addr" params;
      start_block=List.assoc "start_block" params;
      abi=List.assoc "abi" params;
    }

  let fmt_template name params = 
    Template {
      name;
      abi=List.assoc "abi" params;
    }

  let fmt_new_template name params =
    NewTemplate {
      name;
      address=List.assoc "address" params;
    }
%}

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> ADDRESS

%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token DOT
%token COLON
%token EQUALS
%token BANG

%token ADD SUB
%token MUL DIV

%token INTERFACE
%token ENTITY
%token AS
%token DATA_SOURCE
%token TEMPLATE
%token EVENT_HANDLER
%token CALL_HANDLER
%token FROM
%token EVENT
%token CALL

%token NEW_ENTITY
%token UPDATE
%token NEW_TEMPLATE

%token SOL_ADDRESS
%token SOL_BOOL
%token SOL_STRING
%token SOL_FIXED
%token SOL_UFIXED
%token SOL_BYTES
%token <string> SOL_FBYTES
%token <string> SOL_INT
%token <string> SOL_UINT

%token GQL_ID
%token GQL_BYTES
%token GQL_STRING
%token GQL_INT
%token GQL_BIGINT
%token GQL_FLOAT
%token GQL_BIGDECIMAL
%token GQL_BOOL

%token <string> IDENT

%token EOF

%left ADD SUB
%left MUL DIV

%start <Parsetree.toplevel list> document
%%

document: list(toplevel) EOF { $1 }

toplevel:
  | INTERFACE id = IDENT LBRACE fields = structure(COLON, gql_type) RBRACE 
    { Interface {name=id; fields } }
  | entity = entity
    { entity }

  | DATA_SOURCE id = IDENT LBRACE params = structure(EQUALS, expr) RBRACE
    { fmt_data_source id params }
  | TEMPLATE id = IDENT LBRACE params = structure(EQUALS, expr) RBRACE
    { fmt_template id params }

  | EVENT_HANDLER eid = IDENT FROM sid = IDENT LBRACE actions = list(action) RBRACE
    { EventHandler {event=eid; source=sid; actions} }
  | CALL_HANDLER cid = IDENT FROM sid = IDENT LBRACE actions = list(action) RBRACE
    { CallHandler {call=cid; source=sid; actions} }

  | EVENT id = IDENT LBRACE fields = structure(COLON, sol_type) RBRACE 
    { Event {name=id; fields } }
  // | CALL id = IDENT LBRACE fields = structure(COLON, sol_type) RBRACE 
  //   { Event {name=id; fields } }

entity:
  | ENTITY id = IDENT LBRACE fields = structure(COLON, gql_type) RBRACE 
    { Entity {name=id; interface=None; fields } }
  | ENTITY id = IDENT AS intf = IDENT LBRACE fields = structure(COLON, gql_type) RBRACE 
    { Entity {name=id; interface=Some(intf); fields } }

action:
  | NEW_ENTITY name = IDENT LPAREN id = expr RPAREN 
    LBRACE values = structure(EQUALS, expr) RBRACE
    { NewEntity {name; id; values} }
  | UPDATE name = IDENT LPAREN id = expr RPAREN 
    LBRACE values = structure(EQUALS, expr) RBRACE
    { UpdateEntity {name; id; values} }
  | NEW_TEMPLATE name = IDENT 
    LBRACE params = structure(EQUALS, expr) RBRACE
    { fmt_new_template name params }

expr:
  | SUB e = expr                  { Neg e }
  | e1 = expr ADD e2 = expr       { Add (e1, e2) }
  | e1 = expr SUB e2 = expr       { Sub (e1, e2) }
  | e1 = expr MUL e2 = expr       { Mul (e1, e2) }
  | e1 = expr DIV e2 = expr       { Div (e1, e2) }
  | id = vpath                    { id }
  | lit = literal                 { Literal lit }

literal:
  | v = ADDRESS  { Address v }
  | v = BOOL     { Bool v }
  | v = INT      { Int v }
  | v = FLOAT    { Float v }
  | v = STRING   { String v }

vpath:
  | path = separated_nonempty_list(DOT, IDENT)
    {
      match List.rev path with
      | ident::rest -> Variable (rest, ident)
      | _ -> raise(Parsing_error("empty vpath"))
    }
  // | id1 = IDENT DOT id2 = IDENT       { Variable ([id1], id2) }
  // | id = IDENT                        { Variable ([], id) }
  // | id = IDENT rest = vpath           { fold_vpath id rest }

structure(separator, rhs):
  | elements = list(name = IDENT separator rhs = rhs { (name, rhs) }) 
    { elements }

sol_type:
  | typ = sol_type LBRACE RBRACK  { SOLArray typ}
  | SOL_ADDRESS                   { SOLAddress }
  | SOL_BOOL                      { SOLBool }
  | SOL_STRING                    { SOLString }
  | SOL_FIXED                     { SOLFixed }
  | SOL_UFIXED                    { SOLUfixed }
  | SOL_BYTES                     { SOLBytes }
  | v = SOL_FBYTES                { SOLFbytes (parse_int_postfix "fbytes" v) }
  | v = SOL_INT                   { SOLInt (parse_int_postfix "int" v) }
  | v = SOL_UINT                  { SOLUint (parse_int_postfix "uint" v) }

gql_type:
  | GQL_ID                          { GQLId }
  | GQL_BYTES                       { GQLBytes }
  | GQL_STRING                      { GQLString }
  | GQL_INT                         { GQLInt }
  | GQL_BIGINT                      { GQLBigInt }
  | GQL_FLOAT                       { GQLFloat }
  | GQL_BIGDECIMAL                  { GQLBigDecimal }
  | GQL_BOOL                        { GQLBoolean }
  | LBRACK typ = gql_type RBRACK    { GQLList (typ) }
  | typ = gql_type BANG             { GQLNonNull (typ) }
  | id = IDENT                      { GQLObject (id) }