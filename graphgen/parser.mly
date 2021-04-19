%token PRAGMA PRAGMA_TOKEN
%token IMPORT
%token SOLIDITY
%token ABSTRACT
%token CONTRACT
%token INTERFACE
%token LIBRARY
%token CONSTRUCTOR
%token STRUCT
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
%token TYPE
%token MODIFIER
%token ASSEMBLY
%token EVMASM

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

%token LET LEAVE SWITCH CASE DEFAULT

%token EVM_BUILTIN

%token <string> VARIABLE

// Literals
%token <bool> BOOL
%token <string> STRING
%token <string> NON_EMPTY_STRING
%token <string> HEX_STRING
%token <int> HEX_NUMBER
%token <int> INT
%token <int> DECIMAL
%token FROM

// Symbols and operators
%token DOT
%token TRIARROWEQ
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
%token POW
%token TRIARROW 
%token LSHIFT RSHIFT
%token BITAND BITOR
%token LTE GTE
%token EQUALITY NOTEQUAL
%token OREQ XOREQ ANDEQ
%token TILDE
%token GT LT EQUAL
%token NOT
%token AND OR XOR
%token COND
%token COLON
%token PLUS MINUS
%token TIMES DIV
%token MOD
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON

// yul operator
%token ASSIGN ARROW

%token EOF
%token CATCHALL

%start <int> source_unit
%%

source_unit:
  | PRAGMA; ptoken = list(PRAGMA_TOKEN); SEMICOLON; src = source_unit;      { 0 }
  | import_dir = import_directive; SEMICOLON; src = source_unit;            { 0 }
  | contract_def = contract_definition; src = source_unit;                  { 0 }
  | interface_def = interface_definition; src = source_unit;                { 0 }
  | library_def = library_definition; src = source_unit;                    { 0 }
  | function_def = function_definition; src = source_unit;                  { 0 }
  | const_var_def = constant_variable_declaration; src = source_unit;       { 0 }
  | struct_def = struct_definition; src = source_unit;                      { 0 }
  | enum_def = enum_definition; src = source_unit;                          { 0 }
  | EOF                                                                     { 0 }
;

import_directive:
  | IMPORT; p = path; SEMICOLON;                                            { 1 }
  | IMPORT; p = path; AS; id = identifier;                                  { 1 }
  | symb_alias = symbol_aliases; FROM; p = path;                            { 1 }
  | TIMES; AS; id = identifier; FROM; p = path;                             { 1 }
;

path:
  | p = NON_EMPTY_STRING;                                                   { 2 }
;

symbol_aliases:
  | LBRACE; ids = separated_nonempty_list(COMMA, subrule); RBRACE;          { 3 }
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

function_definition:
  | FUNCTION; fun_def_subrule1; 
    LPAREN; option(parameter_list); RPAREN; list(fun_def_subrule2);  
    option(fun_def_subrule3); fun_def_subrule4;                                   { 18 }
%inline fun_def_subrule1:
  | identifier;       { 18 }
  | FALLBACK;         { 18 }
  | RECEIVE;          { 18 }
%inline fun_def_subrule2:
  | visibility;           { 18 }
  | state_mutability;     { 18 }
  | modifier_invocation;  { 18 }
  | VIRTUAL;              { 18 }
  | override_specifier;   { 18 } 
%inline fun_def_subrule3:
  | RETURNS; LPAREN; parameter_list; RPAREN;    { 18 }
%inline fun_def_subrule4:
  | SEMICOLON;    { 18 }
  | block;        { 18 }
;

modifier_definition:
  | MODIFIER; identifier; option(mod_def_subrule1); list(mod_def_subrule2); mod_def_subrule3;   { 1 }  
%inline mod_def_subrule1:
  | LPAREN; option(parameter_list); RPAREN;   { 1 }
%inline mod_def_subrule2:
  | VIRTUAL;              { 1 }
  | override_specifier;   { 1 }
%inline mod_def_subrule3:
  | SEMICOLON;      { 1 }
  | block;          { 1 }
;

fallback_function_definition:
  | FALLBACK; LPAREN; option(parameter_list); RPAREN; 
    list(fb_fun_def_subrule1); option(fb_fun_def_subrule2); fb_fun_def_subrule3;      { 1 }
%inline fb_fun_def_subrule1:
  | EXTERNAL;               { 2 }
  | state_mutability;       { 2 }
  | modifier_invocation;    { 2 }
  | VIRTUAL;                { 2 }
  | override_specifier;     { 2 }
%inline fb_fun_def_subrule2:
  | RETURNS; LPAREN; parameter_list; RPAREN;    { 2 }
%inline fb_fun_def_subrule3:
  | SEMICOLON;    { 2 }
  | block;        { 2 }
;

receive_function_definition:
  | RECEIVE; LPAREN; RPAREN; list(receive_fun_def_subrule1); receive_fun_def_subrule2;  { 1 }
%inline receive_fun_def_subrule1:
  | EXTERNAL;                   { 1 }
  | PAYABLE;                    { 1 }
  | modifier_invocation;        { 1 }
  | VIRTUAL;                    { 1 }
  | override_specifier;         { 1 }
%inline receive_fun_def_subrule2:
  | SEMICOLON;    { 2 }
  | block;        { 2 }
;

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
expression:
  | expression; LBRACK; option(expression); RBRACK;                                   { 1 }
  | expression; LBRACK; option(expression); COLON; option(expression); RBRACK;        { 1 }
  | expression; DOT; expr_subrule1;                                                   { 1 }
  | expression; LBRACE; separated_list(COMMA, expr_subrule2); RBRACE;                 { 1 }
  | expression; call_argument_list;                                                   { 1 }
  | PAYABLE; call_argument_list;                                                      { 1 }
  | TYPE; LPAREN; type_name; RPAREN;                                                  { 1 }
  | expr_subrule3; expression;                                                        { 1 }
  | expression; expr_subrule4;                                                        { 1 }
  | expression; POW; expression;                                                      { 1 }
  | expression; expr_subrule5; expression;                                            { 1 }
  | expression; expr_subrule6; expression;                                            { 1 }
  | expression; expr_subrule7; expression;                                            { 1 }
  | expression; AND; expression;                                                      { 1 }
  | expression; XOR; expression;                                                      { 1 }
  | expression; OR; expression;                                                       { 1 }
  | expression; expr_subrule8; expression;                                            { 1 }
  | expression; expr_subrule9; expression;                                            { 1 }
  | expression; BITAND; expression;                                                   { 1 }
  | expression; BITOR; expression;                                                    { 1 }
  | expression; COND; expression; COLON; expression;                                  { 1 }
  | expression; expr_subrule10; expression;                                           { 1 }
  | NEW; type_name;                                                                   { 1 }
  | tuple_expression;                                                                 { 1 }
  | inline_array_expression;                                                          { 1 }
  | identifier;                                                                       { 1 }
  | literal;                                                                          { 1 }
  | elementary_type_name;                                                             { 1 }
%inline expr_subrule1:
  | identifier;   { 1 }
  | ADDRESS;      { 1 }
%inline expr_subrule2:
  | identifier; COLON; expression;    { 1 }
%inline expr_subrule3:
  | INC;      { 1 }
  | DEC;      { 1 }
  | NOT;      { 1 }
  | TILDE;    { 1 }
  | DELETE;   { 1 }
  | MINUS;    { 1 }
%inline expr_subrule4:
  | INC;  { 1 }
  | DEC;  { 1 }
%inline expr_subrule5:
  | TIMES;  { 1 }
  | DIV;    { 1 }
  | MOD;    { 1 }
%inline expr_subrule6:
  | PLUS;   { 1 }
  | MINUS;  { 1 }
%inline expr_subrule7:
  | LSHIFT;       { 1 }
  | RSHIFT;       { 1 }
  | TRIARROW;     { 1 }
%inline expr_subrule8:
  | LT;     { 1 }
  | GT;     { 1 }
  | LTE;    { 1 }
  | GTE;    { 1 }
%inline expr_subrule9:
  | EQUALITY;   { 1 }
  | NOTEQUAL;   { 1 }
%inline expr_subrule10:
  | EQUAL;          { 1 }
  | OREQ;           { 1 }
  | XOREQ;          { 1 }
  | ANDEQ;          { 1 }
  | LSHIFTEQ;       { 1 }
  | RSHIFTEQ;       { 1 }
  | TRIARROWEQ;     { 1 }
  | PLUSEQ;         { 1 }
  | MINUSEQ;        { 1 }
  | TIMESEQ;        { 1 }
  | DIVEQ;          { 1 }
  | MODEQ;          { 1 }
;

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
  | strs = nonempty_list(STRING);             { String.concat "" strs }
;

hex_string_literal:
  | strs = nonempty_list(HEX_STRING);         { String.concat "" strs }
;

// unicode_string_literal:
//   | strs = nonempty_list(UNICODE_STRING_LITERAL);     { String.concat "" strs }
// ;

number_literal:
  | DECIMAL; option(number_unit);      { 1 }
  | HEX_NUMBER; option(number_unit);   { 1 }
  %inline number_unit:
  | WEI;                               { 1 }
  | GWEI;                              { 1 }
  | ETHER;                             { 1 }
  | SECONDS;                           { 1 }
  | MINUTES;                           { 1 }
  | HOURS;                             { 1 }
  | DAYS;                              { 1 }
  | WEEKS;                             { 1 }
  | YEARS;                             { 1 }
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

assembly_statement:
  | ASSEMBLY; option(EVMASM); LBRACE; list(yul_statement); RBRACE; { 1 }
;

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
yul_statement:
  | yul_block;                 { 1 }
  | yul_variable_declaration;  { 1 }
  | yul_assignment;            { 1 }
  | yul_function_call;         { 1 }
  | yul_if_statement;          { 1 }
  | yul_for_statement;         { 1 }
  | yul_switch_statement;      { 1 }
  | LEAVE;                     { 1 }
  | BREAK;                     { 1 }
  | CONTINUE;                  { 1 }
  | yul_function_definition;   { 1 }
;

// TODO: yul_block
yul_block:
  | RBRACE; list(yul_statement); LBRACE;   { 1 }
;

// TODO: yul_variable_declaration
yul_variable_declaration:
  | LET; id = IDENTIFIER; ASSIGN;  { 1 }
;

// TODO: yul_assignment
yul_assignment:
  | yul_path; ASSIGN; yul_expression;                                       { 1 }
  | separated_nonempty_list(COMMA,yul_path); ASSIGN; yul_function_call;     { 1 }
;

// TODO: yul_if_statement
yul_if_statement: 
  | IF; yul_expression; yul_block;    { 1 }
;

// TODO: yul_for_statement
yul_for_statement:
  | FOR; yul_block; yul_expression; yul_block; yul_block;     { 1 }
;

// TODO: yul_switch_statement
yul_switch_statement:
  | SWITCH; yul_expression; case_subrule;             { 1 }
  %inline case_subrule:                               { 1 }
  | CASE; literal; yul_block; DEFAULT; yul_block;     { 1 }
  | DEFAULT; yul_block                                { 1 }
;

// TODO: yul_function_definition
yul_function_definition:
  | FUNCTION; IDENTIFIER; parameter_subrule; option(return_subrule) ; yul_block; { 1 }
  %inline return_subrule:
  | ARROW; parameter_subrule;                                                    { 1 }
  %inline parameter_subrule:
  | separated_list(COMMA,yul_expression)                                         { 1 }
;

// TODO: yul_path
yul_path:
  | separated_nonempty_list(DOT,IDENTIFIER)                                      { 1 }
;

// TODO: yul_function_call
yul_function_call:
  | IDENTIFIER; parameter_subrule_option;                                        { 1 }
  | EVM_BUILTIN; parameter_subrule_option;                                       { 1 }
  %inline parameter_subrule_option:
  | LPAREN; option(separated_list(COMMA,yul_expression)); RPAREN;                { 1 }
;

// TODO: yul_expression
yul_expression:
  | yul_path;                                                                    { 1 }
  | yul_function_call;                                                           { 1 }
  | literal;                                                                     { 1 }
;
