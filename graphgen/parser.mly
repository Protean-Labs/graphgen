%{
  open Easy_logging

  let logger = Logging.make_logger "Parser" Debug [Cli Debug]
%}


%token PRAGMA 
%token SOLIDITY EXPERIMENTAL
%token VERSION
%token IMPORT
// %token ABSTRACT
// %token CONTRACT
%token INTERFACE
%token STRUCT
%token FUNCTION
%token MAPPING
%token PUBLIC
%token PRIVATE
%token EXTERNAL
%token INTERNAL
%token PURE
%token VIEW
%token PAYABLE
// %token CONSTANT
// %token IMMUTABLE
%token ANON
%token INDEXED
%token VIRTUAL
%token OVERRIDE
%token FALLBACK

%token RETURNS
// %token RETURN RETURNS

// %token ADDRESS
%token RECEIVE
// %token USING
// %token TYPE
// %token MODIFIER
// %token ASSEMBLY
// %token EVMASM

// Events
%token EVENT
// %token EMIT EVENT

// Data location
%token MEMORY STORAGE CALLDATA

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


// Literals
%token <string> NON_EMPTY_STRING
%token FROM

// Symbols and operators
%token DOT
%token MAP
%token COLON
%token TIMES
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON

%token COMMENT_BLOCK_START COMMENT_BLOCK_END
%token <string> GG_SOURCE GG_HANDLER GG_FIELD
%token <string> COMMENT_BLOCK 

%token <string> IDENTIFIER

%token EOF

%start <int> source_unit
%%

source_unit:
  | pragma; SEMICOLON; source_unit;                   { logger#debug "Pragma"; 0 }
  | import_directive; SEMICOLON; source_unit;         { logger#debug "Import"; 0 }
  | gg_tag; source_unit;                              { logger#debug "GG_tag"; 0 }
  | comments; source_unit;                            { logger#debug "Comment block"; 0 }
  | interface_definition; source_unit;                { logger#debug "Interface definition"; 0 }
  | struct_definition; source_unit;                   { logger#debug "Struct definition"; 0 }
  | enum_definition; source_unit;                     { logger#debug "Enum definition"; 0 }
  | EOF                                               { logger#debug "EOF"; 0 }
;

pragma:
  | PRAGMA; SOLIDITY; VERSION;          { logger#debug "Pragma solidity"; 0 }
  | PRAGMA; EXPERIMENTAL; identifier;   { logger#debug "Pragma experimental"; 0 }

comments:
  | COMMENT_BLOCK;    { logger#debug "Comment without GG_TAG"; 1 }
;

gg_tag:
  | gg_params = GG_SOURCE;        { 1 }
  | gg_params = GG_HANDLER;       { 1 }
  | gg_params = GG_FIELD;         { 1 }
;

import_directive:
  | IMPORT; path; SEMICOLON;                                            { 1 }
  | IMPORT; path; AS; identifier;                                  { 1 }
  | symbol_aliases; FROM; path;                            { 1 }
  | TIMES; AS; identifier; FROM; path;                             { 1 }
;

path:
  | NON_EMPTY_STRING;                                                   { 2 }
;

symbol_aliases:
  | LBRACE; separated_nonempty_list(COMMA, subrule); RBRACE;          { 3 }
%inline subrule:
  | identifier; AS; identifier;                                 { 3 }
  | identifier;                                                        { 3 }
;

interface_definition:
  | INTERFACE; id = identifier; 
    IS; inheritance = separated_nonempty_list(COMMA, inheritance_specifier); 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 5 }
  | INTERFACE; id = identifier; 
    LBRACE; body = option(list(contract_body_element)); RBRACE;                   { 5 }
;

inheritance_specifier:
  | id_path = identifier_path;                                                    { 7 }
;

contract_body_element:
  | gg_tag; contract_body_element;                                                { 8 }
  | COMMENT_BLOCK; contract_body_element;                                         { 8 }
  | fun_def = function_definition;                                                { 8 }
  | fallback_def = fallback_function_definition;                                  { 8 }
  | receive_def = receive_function_definition;                                    { 8 }
  | struct_def = struct_definition;                                               { 8 }
  | enum_def = enum_definition;                                                   { 8 }
  | event_def = event_definition;                                                 { 8 }
;

identifier_path:
  | separated_nonempty_list(DOT, identifier)                                      { 10 }
;

visibility:
  | INTERNAL;       { 12 }
  | EXTERNAL;       { 12 }
  | PRIVATE;        { 12 }
  | PUBLIC;         { 12 }
;

parameter_list:
  | separated_nonempty_list(COMMA, parameter_list_subrule);              { 13 }
%inline parameter_list_subrule:
  | type_name; option(data_location); option(identifier);   { 13 }
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
  // | modifier_invocation;  { 18 }
  // | VIRTUAL;              { 18 }
  | override_specifier;   { 18 } 
%inline fun_def_subrule3:
  | RETURNS; LPAREN; parameter_list; RPAREN;    { 18 }
%inline fun_def_subrule4:
  | SEMICOLON;    { 18 }
  // | block;        { 18 }
;

fallback_function_definition:
  | FALLBACK; LPAREN; option(parameter_list); RPAREN; 
    list(fb_fun_def_subrule1); option(fb_fun_def_subrule2); fb_fun_def_subrule3;      { 1 }
%inline fb_fun_def_subrule1:
  | EXTERNAL;               { 2 }
  | state_mutability;       { 2 }
  // | modifier_invocation;    { 2 }
  | VIRTUAL;                { 2 }
  | override_specifier;     { 2 }
%inline fb_fun_def_subrule2:
  | RETURNS; LPAREN; parameter_list; RPAREN;    { 2 }
%inline fb_fun_def_subrule3:
  | SEMICOLON;    { 2 }
  // | block;        { 2 }
;

receive_function_definition:
  | RECEIVE; LPAREN; RPAREN; list(receive_fun_def_subrule1); receive_fun_def_subrule2;  { 1 }
%inline receive_fun_def_subrule1:
  | EXTERNAL;                   { 1 }
  | PAYABLE;                    { 1 }
  // | modifier_invocation;        { 1 }
  | VIRTUAL;                    { 1 }
  | override_specifier;         { 1 }
%inline receive_fun_def_subrule2:
  | SEMICOLON;    { 2 }
  // | block;        { 2 }
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

event_parameter:
  | type_name; option(INDEXED); option(identifier);   { 1 }
;

event_definition:
  | EVENT; identifier; LPAREN; separated_list(COMMA, event_parameter); RPAREN; option(ANON); SEMICOLON;   { 1 }
;

type_name:
  | elementary_type_name;                             { 1 }
  // | function_type_name;                               { 1 }
  // | mapping_type;                                     { 1 }
  // | identifier_path;                                  { 1 }
  | type_name; LBRACK; RBRACK;                        { 1 }
;

elementary_type_name:
  | ADDRESS_T;                      { 1 }
  | BOOL_T;                         { 1 }
  | STRING_T;                       { 1 }
  | FIXED_T;                        { 1 }
  | UFIXED_T;                       { 1 }
  | BYTES_T;                        { 1 }
  | FBYTES_T;                       { 1 }
  | INT_T;                          { 1 }
  | UINT_T;                         { 1 }
;

function_type_name:
  | FUNCTION; LPAREN; option(parameter_list); RPAREN; list(function_type_name_subrule1); option(function_type_name_subrule2);   { 1 }
%inline function_type_name_subrule1:
  | visibility;         { 1 }
  | state_mutability;   { 1 }
%inline function_type_name_subrule2:
  | RETURNS; LPAREN; parameter_list; RPAREN;  { 1 }
;

data_location:
  | MEMORY;       { 2 }
  | STORAGE;      { 2 }
  | CALLDATA;     { 2 }
;

identifier:
  | IDENTIFIER;   { 1 }
  | FROM;         { 1 }
;

mapping_type:
  | MAPPING; LPAREN; mapping_key_type; MAP; type_name; RPAREN;  { 1 }
;

mapping_key_type:
  | elementary_type_name;   { 1 }
  | identifier_path;        { 1 }
;