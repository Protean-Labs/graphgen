{
  open Easy_logging
  open Parser

  let remove_prefix prefix s = 
    String.sub s (String.length prefix) (String.length s - String.length prefix)

  let logger = Logging.make_logger "Lexer" Info  [Cli Debug]
}

let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let event_def     = "event " identifier
let function_def  = "function " identifier

rule token = parse
(* White space *)
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }

(* Comments *)
| "/*"              { logger#debug "Comment block start"; comment_block lexbuf }
| "/**"             { logger#debug "Comment block start"; comment_block lexbuf }

| event_def as def  
  { logger#debug "event_def"; EVENT (remove_prefix "event " def) }

| function_def as def  
  { logger#debug "function_def"; FUNCTION (remove_prefix "function " def) }

(* End *)
| eof               { EOF }
| _                 { token lexbuf }

and comment_block = parse
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; comment_block lexbuf }
| newline           { Lexing.new_line lexbuf; comment_block lexbuf }
| white             { comment_block lexbuf }
| "@gg:handler"     { logger#debug "@gg:handler"; GG_HANDLER (read_gg_tag_block "" lexbuf) }
| "@gg"             { logger#debug "@gg"; GG (read_gg_tag_block "" lexbuf) }
| _                 { logger#debug "Not a GG_TAG"; read_multi_line_comment lexbuf }

and read_gg_tag_block buf = parse
(* GraphGen tags *)
|  "*/"             { logger#debug "GG block end"; buf }
| _ as c            { read_gg_tag_block (buf ^ Char.escaped c) lexbuf }

(* Not too important *)
and read_multi_line_comment = parse
| "*/"              { Lexing.new_line lexbuf; token lexbuf }
| eof               { EOF }
| _                 { read_multi_line_comment lexbuf }
