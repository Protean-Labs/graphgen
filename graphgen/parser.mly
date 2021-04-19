%token PRAGMA PRAGMA_TOKEN
%token IMPORT
%token SOLIDITY
%token ABSTRACT
%token CONTRACT
%token INTERFACE
%token LIBRARY
%token CONSTRUCTOR
%token NEW
%token FUNCTION
%token MAPPING
%token PUBLIC
%token PRIVATE
%token EXTERNAL
%token INTERNAL
%token PURE
%token VIEW
%token PAYABLE
%token CONSTANT
%token IMMUTABLE
%token ANON
%token INDEXED
%token VIRTUAL
%token OVERRIDE
%token FALLBACK
%token ASSERT
%token REQUIRE
%token DELETE

%token <string> IDENTIFIER

// If-else
%token IF ELSE

// Loop
%token WHILE
%token DO
%token FOR
%token BREAK
%token CONTINUE

%token RETURN RETURNS

%token ADDRESS
%token RECEIVE
%token USING

// Events
%token EMIT EVENT

// Data location
%token MEMORY STORAGE CALLDATA

// Exceptions
%token TRY CATCH

// Alias
%token AS IS

// Elementary Type names
%token ENUM
%token ADDRESS_T
%token BYTES_T
%token BOOL_T
%token STRING_T
%token INT_T
%token UINT_T
%token FBYTES_T
%token FIXED_T
%token UFIXED_T

// Units
%token WEI
%token GWEI
%token ETHER
%token SECONDS
%token MINUTES
%token HOURS
%token DAYS
%token WEEKS
%token YEARS

%token <bool> BOOL
%token <string> VARIABLE STRING
%token <string> NON_EMPTY_STRING
%token <int> INT
%token FROM

// Symbols and operators
%token DOT
%token TRIPARROW
%token LSHIFTEQ
%token RSHIFTEQ
%token MAP
%token INC
%token PLUSEQ
%token MINUSEQ
%token TIMESEQ
%token DIVEQ
%token MODEQ
%token DEC
%token BITAND
%token BITOR
%token LTE
%token GTE
%token OREQ
%token XOREQ
%token ANDEQ
%token GT
%token LT
%token EQUAL
%token NOT
%token COND
%token COLON
%token PLUS MINUS
%token TIMES DIV
%token MOD
%token LPAREN RPAREN
%token LBRACK RBRACK
%token COMMA
%token SEMICOLON

%token EOF
%token CATCHALL

%start <int> prog
%%

source_unit:
  | PRAGMA; ptoken = list(PRAGMA_TOKEN); SEMICOLON; src = source_unit;      { 0 }
  | import_dir = import_directive; SEMICOLON; src = source_unit;            { 0 }
  | contract_def = contract_definition; src = source_unit;                  { 0 }
  | interface_def = interface_definition; src = source_unit;                { 0 }
  | library_def = library_definition; src = source_unit;                    { 0 }
  | function_def = function_definition; src = source_unit;                  { 0 }
  | const_var_def = const_var_definition; src = source_unit;                { 0 }
  | struct_def = struct_definition; src = source_unit;                      { 0 }
  | enum_def = enum_definition; src = source_unit;                          { 0 }
  | EOF                                                                     { 0 }
;

import_directive:
  | IMPORT; p = path; SEMICOLON;                                            { 1 }
  | IMPORT; p = path; AS; id = identifier;                                  { 1 }
  | symb_alias = symbol_aliases; FROM; p = path;                            { 1 }
  | TIMES; AS; id = identifier; FROM; p = path;                              { 1 }
;

path:
  | p = NON_EMPTY_STRING;                                                   { 2 }
;

symbol_aliases:
  | LBRACE; ids = separated_nonempty_list(COMMA, subrule); RBRACE;     { 3 }
%inline subrule:
  | id1 = identifier; AS; id2 = identifier;                                 { 3 }
  | id = identifier;                                                        { 3 }
;

contract_definition:
  | abstract = boption(ABSTRACT); CONTRACT; id = identifier; 
    IS; inheritance = separated_nonempty_list(COMMA, inheritance_specifier); 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 4 }
  | abstract = boption(ABSTRACT); CONTRACT; id = identifier; 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 4 }
;

interface_definition:
  | INTERFACE; id = identifier; 
    IS; inheritance = separated_nonempty_list(COMMA, inheritance_specifier); 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 5 }
  | INTERFACE; id = identifier; 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 5 }
;

library_definition:
  | LIBRARY; id = identifier; LBRACE; body = option(list(contract_body_element)); RBRACE;   { 6 }
;

inheritance_specifier:
  | id_path = identifier_path; call_args = option(call_argument_list);            { 7 }
;

contract_body_element:
  | constructor_def = constructor_definition;                                     { 8 }
  | fun_def = function_definition;                                                { 8 }
  | modifier_def = modifier_definition;                                           { 8 }
  | fallback_def = fallback_function_definition;                                  { 8 }
  | receive_def = receive_function_definition;                                    { 8 }
  | struct_def = struct_definition;                                               { 8 }
  | enum_def = enum_definition;                                                   { 8 }
  | state_var_decl = state_variable_declaration;                                  { 8 }
  | event_def = event_definition;                                                 { 8 }
  | using_dir = using_directive;                                                  { 8 }
;

call_argument_list:
  | LPAREN; args = subrule1; RPAREN;                                              { 9 }
%inline subrule1:
  |                                                                               { 9 }
  | exprs = separated_nonempty_list(COMMA, expression);                           { 9 }
  | LBRACE; separated_list(COMMA, subrule2); RBRACE                               { 9 }
%inline subrule2:
  | identifier; COLON; expression;                                                { 9 }
;

identifier_path:
  | separated_nonempty_list(DOT, identifier)                                      { 10 }
;

modifier_invocation:
  | id_path = identifier_path; args = option(call_argument_list);                 { 11 }
;

visibility:
  | INTERNAL;       { 12 }
  | EXTERNAL;       { 12 }
  | PRIVATE;        { 12 }
  | PUBLIC;         { 12 }
;

parameter_list:
  | params = separated_nonempty_list(COMMA, parameter_list_subrule);              { 13 }
%inline parameter_list_subrule:
  | typ_name = type_name; loc = option(data_location); id = option(identifier);   { 13 }
;

constructor_definition:
  | CONSTRUCTOR; LPAREN; params = option(parameter_list); RPAREN; list(constructor_definition_subrule); block; { 14 } 
%inline constructor_definition_subrule:
  | modifier_invocation;    { 15 }
  | PAYABLE;                { 15 }                                            
  | INTERNAL;               { 15 }
  | PUBLIC;                 { 15 }
;

state_mutability:
  | PURE;       { 16 }
  | VIEW;       { 16 }
  | PAYABLE;    { 16 }
;

override_specifier:
  | OVERRIDE;                                                                     { 17 }
  | OVERRIDE; LPAREN; separated_nonempty_list(COMMA, identifier_path); RPAREN;    { 17 }
;

// TODO: function_definition
// function_definition:
//   | FUNCTION; id = identifier; LPAREN; params = option(parameter_list); RPAREN;   { 18 }
//   | FUNCTION; FALLBACK; LPAREN; params = option(parameter_list); RPAREN;          { 18 }
//   | FUNCTION; RECEIVE; LPAREN; params = option(parameter_list); RPAREN;           { 18 }
// ;

// TODO: modifier_definition
// modifier_definition:
// ;

// TODO: fallback_function_definition
// fallback_function_definition:
// ;

// TODO: receive_function_definition
// receive_function_definition:
// ;

struct_definition:
  | STRUCT; id = identifier; LBRACE; list(struct_member); RBRACE;    { 1 }
;

struct_member:
  | type_name; identifier; SEMICOLON;     { 1 }
;

enum_definition:
  | ENUM; identifier; LBRACE; separated_list(COMMA, identifier); RBRACE;  { 2 }
;

state_variable_declaration:
  | type_name; list(state_variable_declaration_subrule1); identifier; option(state_variable_declaration_subrule2); SEMICOLON;   { 1 }
%inline state_variable_declaration_subrule1:
  | PUBLIC;               { 1 }
  | PRIVATE;              { 1 }
  | INTERNAL;             { 1 }
  | CONSTANT;             { 1 }
  | override_specifier;   { 1 }
  | IMMUTABLE;            { 1 }
%inline state_variable_declaration_subrule2:
  | EQUAL; expression;    { 1 }
;

constant_variable_declaration:
  | type_name; CONSTANT; identifier; EQUAL; expression; SEMICOLON;    { 1 }
;

event_parameter:
  | type_name; option(INDEXED); option(identifier);   { 1 }
;

event_definition:
  | EVENT; identifier; LPAREN; separated_list(COMMA, event_parameter); RPAREN; option(ANON); SEMICOLON;   { 1 }
;

using_directive:
  | USING; identifier_path; FOR; using_directive_subrule; SEMICOLON;    { 1 }
%inline using_directive_subrule:
  | TIMES;        { 1 }
  | type_name;    { 1 }
;

type_name:
  | elementary_type_name;                             { 1 }
  | function_type_name;                               { 1 }
  | mapping_type;                                     { 1 }
  | identifier_path;                                  { 1 }
  | type_name; LBRACK; option(expression); RBRACK;    { 1 }
;

elementary_type_name:
  | ADDRESS_T; option(PAYABLE);     { 1 }
  | BOOL_T;                         { 1 }
  | STRING_T;                       { 1 }
  | BYTES_T;                        { 1 }
  | INT_T;                          { 1 }
  | UINT_T;                         { 1 }
  | FBYTES_T;                       { 1 }
  | FIXED_T;                        { 1 }
  | UFIXED_T;                       { 1 }
;

function_type_name:
  | FUNCTION; LPAREN; option(parameter_list); RPAREN; list(function_type_name_subrule1); option(function_type_name_subrule2);   { 1 }
%inline function_type_name_subrule1:
  | visibility;         { 1 }
  | state_mutability;   { 1 }
%inline function_type_name_subrule2:
  | RETURNS; LPAREN; parameter_list; RPAREN;  { 1 }
;

variable_declaration:
  | type_name; option(data_location); identifier;   { 2 }
;

data_location:
  | MEMORY;       { 2 }
  | STORAGE;      { 2 }
  | CALLDATA;     { 2 }
;

// TODO: expression
// expression:
//   | 
// ;

tuple_expression:
  | LPAREN; separated_list(COMMA, expression); RPAREN;   { 1 }
;

inline_array_expression:
  | LPAREN; separated_nonempty_list(COMMA, expression); RPAREN;   { 1 }
;

identifier:
  | IDENTIFIER;   { 1 }
  | FROM;         { 1 }
;

literal:
  | string_literal;           { 1 }
  | number_literal;           { 1 }
  | b = BOOL;                 { b }
  | hex_string_literal;       { 1 }
  // | unicode_string_literal;   { 1 }
;

// boolean_literal:
//   | TRUE;   { true }
//   | FALSE;  { false }
// ;

string_literal:
  | strs = nonempty_list(STRING_LITERAL);             { String.concat "" strs }
;

hex_string_literal:
  | strs = nonempty_list(HEX_STRING_LITERAL);         { String.concat "" strs }
;

// unicode_string_literal:
//   | strs = nonempty_list(UNICODE_STRING_LITERAL);     { String.concat "" strs }
// ;

number_literal:
  | DECIMAL_NUMBER; option(NUMBER_UNIT);      { 1 }
  | HEX_NUMBER; option(NUMBER_UNIT);          { 1 }
;

// block:
//   | LBRACE; list(block_subrule); RBRACE;    { 1 }
// %inline block_subrule:
//   | statement;          { 1 }
//   | unchecked_block;    { 1 }
// ;

// unchecked_block:
//   | UNCHECKED; block;   { 1 }
// ;

block:
  | LBRACE; list(statement); RBRACE;    { 1 }
;

statement:
  | block;                  { 1 }
  | statement_subrule;      { 1 }
  | if_statement;           { 1 }
  | for_statement;          { 1 }
  | while_statement;        { 1 }
  | do_while_statement;     { 1 }
  | continue_statement;     { 1 }
  | break_statement;        { 1 }
  | try_statement;          { 1 }
  | return_statement;       { 1 }
  | emit_statement;         { 1 }
  | assembly_statement;     { 1 }
%inline statement_subrule:
  | variable_declaration_statement;   { 1 }
  | expression_statement;             { 1 }
;

if_statement:
  | IF; LPAREN; expression; RPAREN; statement; option(if_statement_subrule);   { 1 }
%inline if_statement_subrule:
  | ELSE; statement;      { 1 }
;

for_statement:
  | FOR; LPAREN; for_statement_subrule1; for_statement_subrule2; option(expression); RPAREN; statement;   { 1 }
%inline for_statement_subrule1:
  | variable_declaration_statement;     { 2 }
  | expression_statement;               { 2 }
  | SEMICOLON;                          { 2 }
%inline for_statement_subrule2:
  | expression_statement;               { 2 }
  | SEMICOLON;                          { 2 }
;

while_statement:
  | WHILE; LPAREN; expression; RPAREN; statement;     { 1 }
;

do_while_statement:
  | DO; statement; WHILE; LPAREN; expression; RPAREN; SEMICOLON;    { 1 }
;

continue_statement:
  | CONTINUE; SEMICOLON;    { 1 }
;

break_statement:
  | BREAK; SEMICOLON;       { 1 }
;

try_statement:
  | TRY; expression; option(try_statement_subrule); block; nonempty_list(catch_clause);   { 1 }
%inline try_statement_subrule:
  | RETURNS; LPAREN; parameter_list; RPAREN;    { 1 }
;

catch_clause:
  | CATCH; option(catch_clause_subrule); block;    { 1 }
%inline catch_clause_subrule:
  | option(identifier); LPAREN; parameter_list; RPAREN;   { 1 }
;

return_statement:
  | RETURN; option(expression); SEMICOLON;    { 1 }
;

emit_statement:
  | EMIT; expression; call_argument_list; SEMICOLON;    { 1 }
;

// TODO: assembly_statement
// assembly_statement:
//   | ASSEMBLY; option()
// ;

variable_declaration_tuple:
  | LPAREN; option(list(COMMA)); variable_declaration; list(variable_declaration_tuple_subrule); RPAREN;   { 1 }
%inline variable_declaration_tuple_subrule:
  | COMMA;                          { 1 }
  | COMMA; variable_declaration;    { 1 }
;

variable_declaration_statement:
  | variable_declaration; option(equal_expr); SEMICOLON;    { 1 }
  | variable_declaration_tuple; equal_expr; SEMICOLON;      { 1 }
%inline equal_expr:
  | EQUAL; expression;    { 1 }
;

expression_statement:
  | expression; SEMICOLON;  { 1 }
;

mapping_type:
  | MAPPING; LPAREN; mapping_key_type; MAP; type_name; RPAREN;  { 1 }
;

mapping_key_type:
  | elementary_type_name;   { 1 }
  | identifier_path;        { 1 }
;

// TODO: yul_statement
// yul_statement:
//   |
// ;

// TODO: yul_block
// yul_block:
//   |
// ;

// TODO: yul_variable_declaration
// yul_variable_declaration:
//   |
// ;

// TODO: yul_assignment
// yul_assignment:
//   |
// ;

// TODO: yul_if_statement
// yul_if_statement:
//   |
// ;

// TODO: yul_for_statement
// yul_for_statement:
//   |
// ;

// TODO: yul_switch_statement
// yul_switch_statement:
//   |
// ;

// TODO: yul_function_definition
// yul_function_definition:
//   |
// ;

// TODO: yul_path
// yul_path:
//   |
// ;

// TODO: yul_function_call
// yul_function_call:
//   |
// ;

// TODO: yul_boolean
// yul_boolean:
//   |
// ;

// TODO: yul_literal
// yul_literal:
//   |
// ;

// TODO: yul_expression
// yul_expression:
//   |
// ;
