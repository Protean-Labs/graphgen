%{
  open Easy_logging
  open Ast
  open Rresult

  let logger = Logging.make_logger "Parser" Info [Cli Debug]
  let make_interface (raw_name, elements) tag = {raw_name; elements; tag}
  let make_event_param typ indexed name = {typ; indexed; name}
  let make_fun_param typ data_loc name = {typ; data_loc; name}
  
  let regexp = Str.regexp {|\\n|}

  let parse_gg_source_params params =
    Str.global_replace regexp "\n" params
    |> Yaml.of_string 
    |> function x -> R.bind x Ast.gg_source_params_of_yaml
    |> function | Ok(params) -> params | Error(`Msg err) -> failwith(err)

  let parse_gg_handler_params params =
    Str.global_replace regexp "\n" params
    |> Yaml.of_string 
    |> function x -> R.bind x Ast.gg_handler_params_of_yaml
    |> function | Ok(params) -> params | Error(`Msg err) -> failwith(err)

  let parse_gg_field_params params =
    Str.global_replace regexp "\n" params
    |> function | "" | " " | "\n" -> {name = None; default = None}
      | s -> 
        Yaml.of_string s 
        |> function x -> R.bind x Ast.gg_field_params_of_yaml
        |> function | Ok(params) -> params | Error(`Msg err) -> failwith(err)
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
// %token MAPPING
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
// %token VIRTUAL
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
// %token MAP
// %token COLON
%token TIMES
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON

// %token COMMENT_BLOCK_START COMMENT_BLOCK_END
%token <string> GG_SOURCE GG_HANDLER GG_FIELD
%token <string> COMMENT_BLOCK 

%token <string> IDENTIFIER

%token EOF

%start <Ast.t> source_unit
%%

source_unit:
  | pragma; SEMICOLON; src = source_unit;                                       { logger#debug "Pragma"; src }
  | import_directive; SEMICOLON; src = source_unit;                             { logger#debug "Import"; src }
  | tag = gg_tag; intf = interface_definition; src = source_unit;               { logger#debug "GG_tag"; (make_interface intf (Some tag))::src }
  | comments; src = source_unit;                                                { logger#debug "Comment block"; src }
  | intf = interface_definition; src = source_unit;                             { logger#debug "Interface definition"; (make_interface intf None)::src }
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
  | gg_params = GG_SOURCE;        { (GGSource (parse_gg_source_params gg_params)) }
  | gg_params = GG_HANDLER;       { (GGHandler (parse_gg_handler_params  gg_params)) }
  | gg_params = GG_FIELD;         { (GGField (parse_gg_field_params gg_params)) }
;

import_directive:
  | IMPORT; path; SEMICOLON;                             { 1 }
  | IMPORT; path; AS; identifier;                        { 1 }
  | symbol_aliases; FROM; path;                          { 1 }
  | TIMES; AS; identifier; FROM; path;                   { 1 }
;

path:
  | NON_EMPTY_STRING;                                                   { 2 }
;

symbol_aliases:
  | LBRACE; separated_nonempty_list(COMMA, subrule); RBRACE;          { 3 }
%inline subrule:
  | identifier; AS; identifier;                                       { 3 }
  | identifier;                                                       { 3 }
;

interface_definition:
  | INTERFACE; id = identifier; 
    IS; separated_nonempty_list(COMMA, inheritance_specifier); 
    LBRACE; body = list(contract_body_element); RBRACE;                   { id, body }
  | INTERFACE; id = identifier; 
    LBRACE; body = list(contract_body_element); RBRACE;                   { id, body }
;

inheritance_specifier:
  | identifier_path;                                            { 7 }
;

contract_body_element:
  | tag = gg_tag; ele = element_subrule;                                      { ele (Some tag) }
  | COMMENT_BLOCK; ele = element_subrule;                                     { ele None }
  | ele = element_subrule;                                                    { ele None }
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
    outputs = loption(fun_def_subrule3); fun_def_subrule4;                                   { function tag -> FunctionDef (id, inputs, outputs, tag) }
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
  | FALLBACK; LPAREN; inputs = loption(parameter_list); RPAREN; 
    list(fb_fun_def_subrule1); outputs = loption(fb_fun_def_subrule2); fb_fun_def_subrule3;   { function tag -> FunctionDef ("fallback", inputs, outputs, tag) }
%inline fb_fun_def_subrule1:
  | EXTERNAL;               { 2 }
  | state_mutability;       { 2 }
  // | modifier_invocation;    { 2 }
  // | VIRTUAL;                { 2 }
  | override_specifier;     { 2 }
%inline fb_fun_def_subrule2:
  | RETURNS; LPAREN; params = parameter_list; RPAREN;    { params }
%inline fb_fun_def_subrule3:
  | SEMICOLON;    { 2 }
  // | block;        { 2 }
;

receive_function_definition:
  | RECEIVE; LPAREN; RPAREN; list(receive_fun_def_subrule1); receive_fun_def_subrule2;  { function tag -> FunctionDef ("receive", [], [], tag) }
%inline receive_fun_def_subrule1:
  | EXTERNAL;                   { 1 }
  // | PAYABLE;                    { 1 }
  // | modifier_invocation;        { 1 }
  // | VIRTUAL;                    { 1 }
  | override_specifier;         { 1 }
%inline receive_fun_def_subrule2:
  | SEMICOLON;    { 2 }
  // | block;        { 2 }
;

struct_definition:
  | STRUCT; identifier; LBRACE; list(struct_member); RBRACE;    { function _ -> StructDef }
;

struct_member:
  | type_name; identifier; SEMICOLON;     { 1 }
;

enum_definition:
  | ENUM; identifier; LBRACE; separated_list(COMMA, identifier); RBRACE;  { function _ -> EnumDef }
;

event_parameter:
  | typ = type_name; indexed = boption(INDEXED); id = option(identifier);   { (make_event_param typ indexed id) }
;

event_definition:
  | EVENT; id = identifier; LPAREN; params = separated_list(COMMA, event_parameter); RPAREN; option(ANON); SEMICOLON;   { function x -> EventDef (id, params, x) }
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
  | FBYTES_T;                       { FbytesT(32) }
  | INT_T;                          { IntT(256) }
  | UINT_T;                         { UintT(256) }
;

// function_type_name:
//   | FUNCTION; LPAREN; option(parameter_list); RPAREN; list(function_type_name_subrule1); option(function_type_name_subrule2);   { 1 }
// %inline function_type_name_subrule1:
//   | visibility;         { 1 }
//   | state_mutability;   { 1 }
// %inline function_type_name_subrule2:
//   | RETURNS; LPAREN; parameter_list; RPAREN;  { 1 }
// ;

data_location:
  | MEMORY;       { Memory }
  | STORAGE;      { Storage }
  | CALLDATA;     { Calldata }
;

identifier:
  | id = IDENTIFIER;   { id }
  | FROM;              { "from" }
;

// mapping_type:
//   | MAPPING; LPAREN; mapping_key_type; MAP; type_name; RPAREN;  { 1 }
// ;

// mapping_key_type:
//   | elementary_type_name;   { 1 }
//   | identifier_path;        { 1 }
// ;
