%{
  open Easy_logging
  open Ast

  let logger = Logging.make_logger "Parser" Info [Cli Debug]
  let make_interface name tags = {name; tags}
  let make_event_param typ indexed name = {typ; indexed; name}
  let make_fun_param typ data_loc name = {typ; data_loc; name}  
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

%start <Ast.subgraph> source_unit
%%

source_unit:
  | pragma; SEMICOLON; src = source_unit;                                       { logger#debug "Pragma"; src }
  | import_directive; SEMICOLON; src = source_unit;                             { logger#debug "Import"; src }
  | gg_params = gg_tag; intf = interface_definition; src = source_unit;         { logger#debug "GG_tag"; (gg_params, intf)::src }
  | comments; src = source_unit;                                                { logger#debug "Comment block"; src }
  | interface_definition; src = source_unit;                                    { logger#debug "Interface definition"; src }
  | struct_definition; src = source_unit;                                       { logger#debug "Struct definition"; src }
  | enum_definition; src = source_unit;                                         { logger#debug "Enum definition"; src }
  | EOF                                                                         { logger#debug "EOF"; [] }
;

pragma:
  | PRAGMA; SOLIDITY; VERSION;          { logger#debug "Pragma solidity"; 0 }
  | PRAGMA; EXPERIMENTAL; identifier;   { logger#debug "Pragma experimental"; 0 }

comments:
  | COMMENT_BLOCK;    { logger#debug "Comment without GG_TAG"; 1 }
;

gg_tag:
  | gg_params = GG_SOURCE;        { (GGSource gg_params) }
  | gg_params = GG_HANDLER;       { (GGHandler gg_params) }
  | gg_params = GG_FIELD;         { (GGField gg_params) }
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
  | identifier; AS; identifier;                                         { 3 }
  | identifier;                                                        { 3 }
;

interface_definition:
  | INTERFACE; id = identifier; 
    IS; inheritance = separated_nonempty_list(COMMA, inheritance_specifier); 
    LBRACE; body = list(contract_body_element); RBRACE;                   { (make_interface id (List.filter_map (function x -> x) body)) }
  | INTERFACE; id = identifier; 
    LBRACE; body = list(contract_body_element); RBRACE;                   { (make_interface id (List.filter_map (function x -> x) body)) }
;

inheritance_specifier:
  | id_path = identifier_path;                                                    { 7 }
;

contract_body_element:
  | tag = gg_tag; ele = element_subrule;                                      { Some (tag, ele) }
  | COMMENT_BLOCK;       element_subrule;                                     { None }
  | element_subrule;                                                          { None }
%inline element_subrule:
  // | COMMENT_BLOCK; def = element_subrule;                                     { def }
  | def = function_definition;                                                { def }
  | def = fallback_function_definition;                                       { def }
  | def = receive_function_definition;                                        { def }
  | def = struct_definition;                                                  { def }
  | def = enum_definition;                                                    { def }
  | def = event_definition;                                                   { def }
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
  | params = separated_nonempty_list(COMMA, parameter_list_subrule);              { params }
%inline parameter_list_subrule:
  | typ = type_name; data_loc = option(data_location); id = option(identifier);   { make_fun_param typ data_loc id }
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
  | FUNCTION; id = fun_def_subrule1; 
    LPAREN; inputs = loption(parameter_list); RPAREN; list(fun_def_subrule2);  
    outputs = loption(fun_def_subrule3); fun_def_subrule4;                                   { FunctionDef (id, inputs, outputs) }
%inline fun_def_subrule1:
  | id = identifier;       { id }
  | FALLBACK;              { "fallback" }
  | RECEIVE;               { "receive" }
%inline fun_def_subrule2:
  | visibility;           { 18 }
  | state_mutability;     { 18 }
  // | modifier_invocation;  { 18 }
  // | VIRTUAL;              { 18 }
  | override_specifier;   { 18 } 
%inline fun_def_subrule3:
  | RETURNS; LPAREN; params = parameter_list; RPAREN;    { params }
%inline fun_def_subrule4:
  | SEMICOLON;    { 18 }
  // | block;        { 18 }
;

fallback_function_definition:
  | FALLBACK; LPAREN; option(parameter_list); RPAREN; 
    list(fb_fun_def_subrule1); option(fb_fun_def_subrule2); fb_fun_def_subrule3;      { FallbackDef }
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
  | RECEIVE; LPAREN; RPAREN; list(receive_fun_def_subrule1); receive_fun_def_subrule2;  { ReceiveDef }
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
  | STRUCT; id = identifier; LBRACE; list(struct_member); RBRACE;    { StructDef }
;

struct_member:
  | type_name; identifier; SEMICOLON;     { 1 }
;

enum_definition:
  | ENUM; identifier; LBRACE; separated_list(COMMA, identifier); RBRACE;  { EnumDef }
;

event_parameter:
  | typ = type_name; indexed = boption(INDEXED); id = option(identifier);   { (make_event_param typ indexed id) }
;

event_definition:
  | EVENT; id = identifier; LPAREN; params = separated_list(COMMA, event_parameter); RPAREN; option(ANON); SEMICOLON;   { EventDef (id, params) }
;

type_name:
  | typ = elementary_type_name;              { typ }
  // | function_type_name;                               { 1 }
  // | mapping_type;                                     { 1 }
  // | identifier_path;                                  { 1 }
  | typ = type_name; LBRACK; RBRACK;         { ArrayT (typ) }
;

elementary_type_name:
  | ADDRESS_T;                      { AddressT }
  | BOOL_T;                         { BoolT }
  | STRING_T;                       { StringT }
  | FIXED_T;                        { FixedT }
  | UFIXED_T;                       { UfixedT }
  | BYTES_T;                        { BytesT }
  | FBYTES_T;                       { FbytesT }
  | INT_T;                          { IntT }
  | UINT_T;                         { UintT }
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
  | MEMORY;       { Memory }
  | STORAGE;      { Storage }
  | CALLDATA;     { Calldata }
;

identifier:
  | id = IDENTIFIER;   { id }
  | FROM;              { "from" }
;

mapping_type:
  | MAPPING; LPAREN; mapping_key_type; MAP; type_name; RPAREN;  { 1 }
;

mapping_key_type:
  | elementary_type_name;   { 1 }
  | identifier_path;        { 1 }
;
