{
  open Parser
  exception ParsingError of string
}

let pragma_token = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '.' ':']+

let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

let escape_sequence = "\\" (['\'' '\"' '\\' "\\nrt" "\\n" "\\r"] | 'u' ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] |
'x' ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'])

let double_quoted_literal = ^['\"' '\\']

let single_quoted_literal = ^['\'' '\\']

let string_literal = '"' double_quoted_literal (escape_sequence? double_quoted_literal)* '"' | '\'' single_quoted_literal (escape_sequence? single_quoted_literal)* '\''

let int_literal = '-'? ['0'-'9'] ['0'-'9']*

let bool_literal = "true" | "false"

let version_literal = ['0' - '9']+ '.' ['0' - '9']+ '.' ['0' - '9']+ 

let hex_num_literal = "0x" ['a'-'f' 'A'-'F' '0'-'9']  (['_']? ['a'-'z' 'A'-'Z' '0'-'9'])*

let hex_string_literal = "hex" ('"' ['a'-'f' 'A'-'F' '0'-'9'] (['_']? ['a'-'f' 'A'-'F' '0'-'9'])*  '"' | '\'' ['a'-'f' 'A'-'F' '0'-'9'] (['_']? ['a'-'f' 'A'-'F' '0'-'9'])* '\'')

let dec_num =  (['0'-'9'] ['_']?)+ ('.' ['0'-'9'] ['_']?)+?  ('E'|'e')? '-'? (['0'-'9'] ['_']?)+? 

let fixed_bytes_type = "bytes1" | "bytes2" |"bytes3" |"bytes4" |"bytes5" |"bytes6" |"bytes7" |"bytes8" |"bytes9" |
"bytes10" |"bytes11" |"bytes12" |"bytes13" |"bytes14" |"bytes15" |"bytes16" | "bytes17" | "bytes18" | "bytes19" | "bytes20" |
"bytes21" |"bytes22" |"bytes23" |"bytes24" |"bytes25" |"bytes26" |"bytes27" | "bytes28" | "bytes29" | "bytes30" | "bytes31" | "bytes32"

let signed_int_type = "int" | "int8" | "int16" | "int24" | "int32" | "int40" | "int48" | "int56" | "int64" | "int72" | "int80" | "int88" | "int96" | 
 "int104" | "int112" | "int120" | "int128" | "int136" | "int144" | "int152" | "int160" | "int168" | "int176" | "int184" |  "int192" | "int200" | "int208" | "int216" |
  "int224" | "int232" | "int240" | "int248" | "int256"

let unsigned_int_type = "uint" | "uint8" | "uint16" | "uint24" | "uint32" | "uint40" | "uint48" | "uint56" | "uint64" | "uint72" | "uint80" | "uint88" | "uint96" | 
 "uint104" | "uint112" | "uint120" | "uint128" | "uint136" | "uint144" | "uint152" | "uint160" | "uint168" | "uint176" | "uint184" |  "uint192" | "uint200" | "uint208" | "uint216" |
  "uint224" | "uint232" | "uint240" | "uint248" | "uint256"


(* TODO Scientific notation, yul blocks, semantic actions *)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule token = parse
(* White space *)
 "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
| newline           { Lexing.new_line lexbuf; token lexbuf }
| white             { token lexbuf }

(* Reserved key words *)
| "pragma"          { PRAGMA }
| "import"          { IMPORT }
| "solidity"        { SOLIDITY }
| "abstract"        { ABSTRACT }
| "contract"        { CONTRACT }
| "interface"       { INTERFACE }
| "library"         { LIBRARY }
| "constructor"     { CONSTRUCTOR }
| "new"             { NEW }
| "function"        { FUNCTION }
| "mapping"         { MAPPING }
| "public"          { PUBLIC }
| "private"         { PRIVATE }
| "external"        { EXTERNAL }
| "internal"        { INTERNAL }
| "pure"            { PURE }
| "view"            { VIEW }
| "payable"         { PAYABLE }
| "constant"        { CONSTANT }
| "immutable"       { IMMUTABLE }
| "anonymous"       { ANON }
| "indexed"         { INDEXED }
| "virtual"         { VIRTUAL }
| "override"        { OVERRIDE }
| "fallback"        { FALLBACK }
| "assert"          { ASSERT }
| "require"         { REQUIRE }
| "delete"          { DELETE }
| "if"              { IF }
| "else"            { ELSE }
| "while"           { WHILE }
| "do"              { DO }
| "for"             { FOR }
| "break"           { BREAK }
| "continue"        { CONTINUE }
| "return"          { RETURN }
| "returns"         { RETURNS }
| "receive"         { RECEIVE }
| "event"           { EVENT }
| "emit"            { EMIT }
| "using"           { USING }
| "type"            { TYPE }
| "modifier"        { MODIFIER }
| "assembly"        { ASSEMBLY }
| "'evmasm'"        { EVMASM }

(* Data location *)
| "memory"          { MEMORY }
| "storage"         { STORAGE }
| "calldata"        { CALLDATA }

(* Exceptions *)
| "try"             { TRY }
| "catch"           { CATCH }

(* Alias *)
| "as"              { AS }
| "is"              { IS }

| pragma_token      { PRAGMA_TOKEN (Lexing.lexeme lexbuf) }
| identifier        { IDENTIFIER (Lexing.lexeme lexbuf) }

(*Elementary Type names *)
| "enum"            { ENUM }
| "address"         { ADDRESS_T }
| "bytes"           { BYTES_T }
| "bool"            { BOOL_T }
| "string"          { STRING_T }
| signed_int_type   { INT_T }
| unsigned_int_type { UINT_T }
| fixed_bytes_type  { FBYTES_T}
| "fixed"           { FIXED_T }
| "ufixed"          { UFIXED_T }

(* units *)
| "wei"             { WEI }
| "gwei"            { GWEI }
| "ether"           { ETHER }
| "seconds"         { SECONDS }
| "minutes"         { MINUTES }
| "hours"           { HOURS }
| "days"            { DAYS }
| "weeks"           { WEEKS }
| "years"           { YEARS }

(* Literals *)
| bool_literal      { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
| string_literal    { STRING (Lexing.lexeme lexbuf) }
| hex_string_literal{ HEX_STRING (Lexing.lexeme lexbuf)}
| hex_num_literal   { HEX_NUMBER (int_of_hex (Lexing.lexeme lexbuf))}
| identifier        { VARIABLE (Lexing.lexeme lexbuf) }
| int_literal       { INT (int_of_string (Lexing.lexeme lexbuf))  }
| dec_num_literal   { DECIMAL (int_of_string (Lexing.lexeme lexbuf)) }

| "from"            { FROM }

(* Symbols and operators *)
| '.'               { DOT }
| ">>>="            { TRIARROWEQ }
| ">>="             { LSHIFTEQ }
| "<<="             { RSHIFTEQ }
| "=>"              { MAP }
| "++"              { INC }
| "+="              { PLUSEQ }
| "-="              { MINUSEQ }
| "*="              { TIMESEQ }
| "/="              { DIVEQ }
| "%="              { MODEQ }
| "--"              { DEC }
| "**"              { POW }
| ">>>"             { TRIARROW }
| "<<"              { LSHIFT }
| ">>"              { RSHIFT }
| "&&"              { BITAND }
| "||"              { BITOR }
| "<="              { LTE }
| ">="              { GTE }
| "=="              { EQUALITY }
| "!="              { NOTEQUAL }
| "|="              { OREQ }
| "^="              { XOREQ }
| "&="              { ANDEQ }
| '~'               { TILDE }
| '>'               { GT }
| '<'               { LT }
| '='               { EQUAL }
| '!'               { NOT }
| '&'               { AND }
| '|'               { OR }
| '^'               { XOR }
| '?'               { COND }
| ':'               { COLON }
| '+'               { PLUS }
| '-'               { MINUS }
| '*'               { TIMES }
| '/'               { DIV }
| '%'               { MOD }
| '('               { LPAREN }
| ')'               { RPAREN }
| '['               { LBRACK }
| ']'               { RBRACK }
| '{'               { LBRACE }
| '}'               { RBRACE }
| ','               { COMMA }
| ';'               { SEMICOLON }

(* End *)
| eof               { EOF }
| _                 { CATCHALL }