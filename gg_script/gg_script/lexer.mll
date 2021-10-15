{
  open Parser

  exception Parsing_error of string

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c
}

let bytes_t   = "bytes" ['0'-'9']['0'-'9']?
let int_t     = "int" ['0'-'9']?['0'-'9']?['0'-'9']?
let uint_t    = "uint" ['0'-'9']?['0'-'9']?['0'-'9']?

(* Helpers *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (digit|alpha) 

let int     = '-'? digit+
let float   = '-'? digit+ '.' digit*
let address = "0x" alphanum+
let string  = (alpha|digit|'_')*

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule token = parse
(* White space *)
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }

| "{"               { LBRACE }
| "}"               { RBRACE }
| "["               { LBRACK }
| "]"               { RBRACK }
| "("               { LPAREN }
| ")"               { RPAREN }
| "."               { DOT }
| ":"               { COLON }
| "="               { EQUALS }
| "!"               { BANG }

| "+"               { ADD }
| "-"               { SUB }
| "*"               { MUL }
| "/"               { DIV }

(* GraphQL *)
| "interface"       { INTERFACE }
| "entity"          { ENTITY }
| "as"              { AS }

(* Manifest *)
| "data_source"     { DATA_SOURCE }
| "template"        { TEMPLATE }

(* Typescript *)
| "event_handler"   { EVENT_HANDLER }
| "call_handler"    { CALL_HANDLER }
| "from"            { FROM }

(* Solidity *)
| "event"           { EVENT }
| "call"            { CALL }

(* Actions *)
| "new_entity"      { NEW_ENTITY }
| "update"          { UPDATE }
| "new_template"    { NEW_TEMPLATE }

(* Solidity types *)
| "address"         { SOL_ADDRESS }
| "bool"            { SOL_BOOL }
| "string"          { SOL_STRING }
| "fixed"           { SOL_FIXED }
| "ufixed"          { SOL_UFIXED }
| "bytes"           { SOL_BYTES }
| bytes_t as v      { SOL_FBYTES v }
| int_t as v        { SOL_INT v }
| uint_t as v       { SOL_UINT v }

(* GraphQL types *)
| "ID"              { GQL_ID }
| "Bytes"           { GQL_BYTES }
| "String"          { GQL_STRING }
| "Int"             { GQL_INT }
| "BigInt"          { GQL_BIGINT }
| "Float"           { GQL_FLOAT }
| "BigDecimal"      { GQL_BIGDECIMAL }
| "Boolean"         { GQL_BOOL }

| "true"            { BOOL (true) }
| "false"           { BOOL (false) }
| int as lit        { INT (int_of_string lit) }
| float as lit      { FLOAT (float_of_string lit) }
| address as lit    { ADDRESS lit }
| '"'               { read_string (Buffer.create 16) lexbuf }

| identifier as id  { IDENT (id) }

(* End *)
| eof               { EOF }
| _                 { raise (Parsing_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| '"'               { STRING (Buffer.contents buf) }
| '\\' (backslash_escapes as c) 
  { Buffer.add_char buf (char_for_backslash c);
    read_string buf lexbuf }
| [^ '"' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
| eof               { raise (Parsing_error ("String is not terminated")) }
| _                 { raise (Parsing_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
