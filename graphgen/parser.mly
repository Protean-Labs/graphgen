%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token <string> NON_EMPTY_STRING

%token PRAGMA
%token INTERFACE
%token FUNCTION
%token ADDRESS
%token EXTERNAL
%token PURE VIEW PAYABLE
%token RETURNS
%token IS
%token COMMA COLON 
%token SEMICOLON
%token LBRACK RBRACK
%token LBRACE RBRACE
%token LPAREN RPAREN

%start <Solidity.contract> prog
%%

source_unit:
  | PRAGMA; ptoken = list(pragma_token); SEMICOLON; src = source_unit;      { Pragma (ptoken, src) }
  | import_dir = import_directive; SEMICOLON; src = source_unit;            { Import (import_dir, src) }
  | contract_def = contract_definition; src = source_unit;                  { Contract (contract_def, src) }
  | interface_def = interface_definition; src = source_unit;                { Interface (interface_def, src) }
  | library_def = library_definition; src = source_unit;                    { Library (library_def, src) }
  | function_def = function_definition; src = source_unit;                  { Function (function_def, src) }
  | const_var_def = const_var_definition; src = source_unit;                { Const (const_var_def, src) }
  | struct_def = struct_definition; src = source_unit;                      { Struct (struct_def, src) }
  | enum_def = enum_definition; src = source_unit;                          { Enum (enum_def, src) }
  | EOF                                                                     { End }
;

import_directive:
  | IMPORT; p = path; SEMICOLON;                                            {}
  | IMPORT; p = path; AS; id = identifier;                                  {}
  | symb_alias = symbol_aliases; FROM; p = path;                            {}
  | STAR; AS; id = identifier; FROM; p = path;                              {}
;

path:
  | p = NON_EMPTY_STRING;                                                   { p }
;

symbol_aliases:
  | LBRACE; ids = separated_nonempty_list(COMMA, symbol_alias;); RBRACE;    { ids }
%inline symbol_alias:
  | id1 = identifier; AS; id2 = identifier; {}
  | id = identifier;                        {}
;


contract_definition:
;