{
  open Parser
  open Easy_logging
  exception ParsingError of string

  let logger = Logging.make_logger "Lexer" Info  [Cli Debug]
}

(* let pragma_token = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '.' ':' '^']+ *)
let version = ('^'| ">=" | '>')? ['0' - '9']+ '.' ['0' - '9']+ '.' ['0' - '9']+ 

let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

(* let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']* *)

let bytes_t   = "bytes" ['0'-'9']['0'-'9']?
let int_t     = "int" ['0'-'9']?['0'-'9']?['0'-'9']?
let uint_t    = "uint" ['0'-'9']?['0'-'9']?['0'-'9']?

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

| "function"        { logger#debug "function"; FUNCTION }
| "public"          { logger#debug "public"; PUBLIC }
| "private"         { logger#debug "private"; PRIVATE }
| "external"        { logger#debug "external"; EXTERNAL}
| "internal"        { logger#debug "internal"; INTERNAL }
| "pure"            { logger#debug "pure"; PURE }
| "view"            { logger#debug "view"; VIEW }
| "payable"         { logger#debug "payable"; PAYABLE }
| "anonymous"       { logger#debug "anonymous"; ANON }
| "indexed"         { logger#debug "indexed"; INDEXED }

| "override"        { logger#debug "override"; OVERRIDE }
| "fallback"        { logger#debug "fallback"; FALLBACK }
| "returns"         { logger#debug "returns"; RETURNS }
| "receive"         { logger#debug "receive"; RECEIVE }

| "event"           { logger#debug "event"; EVENT }

(* Data location *)
| "memory"          { logger#debug "memory"; MEMORY }
| "storage"         { logger#debug "storage"; STORAGE }
| "calldata"        { logger#debug "calldata"; CALLDATA }

(*Elementary Type names *)
| "address"         { SOL_ADDRESS }
| "bool"            { SOL_BOOL }
| "string"          { SOL_STRING }
| "fixed"           { SOL_FIXED }
| "ufixed"          { SOL_UFIXED }
| "bytes"           { SOL_BYTES }
| bytes_t as v      { SOL_FBYTES v }
| int_t as v        { SOL_INT v }
| uint_t as v       { SOL_UINT v }

(* Identifiers *)
| identifier as id  { logger#debug "identifier: %s" id; IDENTIFIER (id) }

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
| "@gg:handler"     { logger#debug "@gg:handler"; GG_HANDLER (read_gg_tag_handler "" lexbuf) }
(* | "@gg:source"      { logger#debug "@gg:source"; GG_SOURCE (read_gg_tag_block "" lexbuf) }
| "@gg:field"       { logger#debug "@gg:field"; GG_FIELD (read_gg_tag_block "" lexbuf) } *)
| "@gg"             { logger#debug "@gg"; }
| _                 { logger#debug "Not a GG_TAG"; read_multi_line_comment lexbuf }

and read_gg_tag_block buf = parse
(* GraphGen tags *)
|  "*/"             { logger#debug "GG block end"; buf }
| _ as c            { read_gg_tag_block (buf ^ Char.escaped c) lexbuf }

and read_gg_tag_handler buf = parse
(* GraphGen tags *)
|  "*/"             { logger#debug "GG handler end"; buf }
| _ as c            { read_gg_tag_block (buf ^ Char.escaped c) lexbuf }

and read_event_or_function = parse
| "event"           { logger#debug "EVENT"; EVENT }
| "function"        { logger#debug "FUNCTION"; FUNCTION }
| identifier as id  { logger#debug "identifier: %s" id; IDENTIFIER (id) }
| "("               { token lexbuf }

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
