{
  open Parser
  open Easy_logging
  exception ParsingError of string

  let logger = Logging.make_logger "Lexer" Debug [Cli Debug]
}

(* let pragma_token = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '.' ':' '^']+ *)
let version = ('^'| ">=" | '>')? ['0' - '9']+ '.' ['0' - '9']+ '.' ['0' - '9']+ 

let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

(* let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']* *)

let bytes = "bytes" ['0'-'9']['0'-'9']?
let int = "int" ['0'-'9']?['0'-'9']?['0'-'9']?
let uint = "uint" ['0'-'9']?['0'-'9']?['0'-'9']?

(* TODO semantic actions *)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
(* White space *)
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }

(* Comments *)
| "/*"              { logger#debug "Comment block start"; gg_tag lexbuf }
| "/**"             { logger#debug "Comment block start"; gg_tag lexbuf }
| "//"              { logger#debug "Comment line"; read_single_line_comment lexbuf }

(* Reserved key words *)
(* Pragma *)
| "pragma"          { logger#debug "pragma"; PRAGMA }
| "solidity"        { logger#debug "pragma solidity"; SOLIDITY }
| "experimental"    { logger#debug "pragma experimental"; EXPERIMENTAL }

| "import"          { logger#debug "import"; IMPORT }
| "interface"       { logger#debug "interface"; INTERFACE }
| "function"        { logger#debug "function"; FUNCTION }
(* | "mapping"         { logger#debug "mapping"; MAPPING } *)
| "public"          { logger#debug "public"; PUBLIC }
| "private"         { logger#debug "private"; PRIVATE }
| "external"        { logger#debug "external"; EXTERNAL}
| "internal"        { logger#debug "internal"; INTERNAL }
| "pure"            { logger#debug "pure"; PURE }
| "view"            { logger#debug "view"; VIEW }
| "payable"         { logger#debug "payable"; PAYABLE }
| "anonymous"       { logger#debug "anonymous"; ANON }
| "indexed"         { logger#debug "indexed"; INDEXED }
(* | "virtual"         { logger#debug "virtual"; VIRTUAL } *)
| "override"        { logger#debug "override"; OVERRIDE }
| "fallback"        { logger#debug "fallback"; FALLBACK }
| "returns"         { logger#debug "returns"; RETURNS }
| "receive"         { logger#debug "receive"; RECEIVE }
| "event"           { logger#debug "event"; EVENT }
| "struct"          { logger#debug "struct"; STRUCT }
| "enum"            { logger#debug "enum"; ENUM }

(* Data location *)
| "memory"          { logger#debug "memory"; MEMORY }
| "storage"         { logger#debug "storage"; STORAGE }
| "calldata"        { logger#debug "calldata"; CALLDATA }

(* Alias *)
| "as"              { logger#debug "as"; AS }
| "is"              { logger#debug "is"; IS }

(*Elementary Type names *)
| "address"         { logger#debug "address"; ADDRESS_T }
| "bool"            { logger#debug "bool"; BOOL_T }
| "string"          { logger#debug "string"; STRING_T }
| "fixed"           { logger#debug "fixed"; FIXED_T }
| "ufixed"          { logger#debug "ufixed"; UFIXED_T }
| "bytes"           { logger#debug "bytes"; BYTES_T }
| bytes             { logger#debug "Fixed bytes"; FBYTES_T }
| int               { logger#debug "Int"; INT_T }
| uint              { logger#debug "Uint"; UINT_T }

| "from"            { logger#debug "from"; FROM }
| '('               { logger#debug "LPAREN"; LPAREN }
| ')'               { logger#debug "RPAREN"; RPAREN }
| '['               { logger#debug "LBRACK"; LBRACK }
| ']'               { logger#debug "RBRACK"; RBRACK }
| '{'               { logger#debug "LBRACE"; LBRACE }
| '}'               { logger#debug "RBRACE"; RBRACE }
| ','               { logger#debug "COMMA"; COMMA }
| '.'               { logger#debug "DOT"; DOT }
| ';'               { logger#debug "SEMICOLON"; SEMICOLON }

| '"'               { logger#debug "String start"; read_string_dquote "" lexbuf }
| '\''              { logger#debug "String start"; read_string_squote "" lexbuf }

(* Identifiers *)
| identifier as id  { logger#debug "identifier: %s" id; IDENTIFIER (id) }
| version as ver    { logger#debug "version: %s" ver; VERSION }

(* End *)
| eof               { EOF }
| _                 { raise (ParsingError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
| newline           { Lexing.new_line lexbuf; token lexbuf }
| eof               { EOF }
| _                 { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
| "*/"              { Lexing.new_line lexbuf; token lexbuf }
| eof               { EOF }
| _                 { read_multi_line_comment lexbuf }

and gg_tag = parse
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; gg_tag lexbuf }
| newline           { Lexing.new_line lexbuf; gg_tag lexbuf }
| white             { gg_tag lexbuf }
| "@gg:source"      { logger#debug "@gg:source"; GG_SOURCE (read_gg_tag_block "" lexbuf) }
| "@gg:handler"     { logger#debug "@gg:handler"; GG_HANDLER (read_gg_tag_block "" lexbuf) }
| "@gg:field"       { logger#debug "@gg:field"; GG_FIELD (read_gg_tag_block "" lexbuf) }
| _                 { logger#debug "Not a GG_TAG"; read_multi_line_comment lexbuf }

and read_gg_tag_block buf = parse
(* GraphGen tags *)
|  "*/"             { logger#debug "Comment block end"; buf }
| _ as c            { read_gg_tag_block (buf ^ Char.escaped c) lexbuf }

and read_string_dquote buf = parse
| '"'               { STRING (buf) }
| '\\' 'n'          { read_string_dquote (buf ^ Char.escaped '\n') lexbuf }
| eof               { raise (ParsingError ("String is not terminated")) }
| _ as c            { read_string_dquote (buf ^ Char.escaped c) lexbuf }

and read_string_squote buf = parse
| '\''              { STRING (buf) }
| '\\' 'n'          { read_string_squote (buf ^ Char.escaped '\n') lexbuf }
| eof               { raise (ParsingError ("String is not terminated")) }
| _ as c            { read_string_squote (buf ^ Char.escaped c) lexbuf }
