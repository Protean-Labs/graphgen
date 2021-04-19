{
  open Parser
  exception ParsingError of string

   let lookup_table = Hashtbl.create 32
  let _ = 
    List.iter (fun (kwd,tok) -> Hashtbl.add keyword_table kwd tok) [
      "bytes1" , BYTES1;
      "bytes2" , BYTES2;
      "bytes3" , BYTES3;
      "bytes4" , BYTES4;
      "bytes5" , BYTES5;
      "bytes6" , BYTES6;
      "bytes7" , BYTES7;
      "bytes8" , BYTES8;
      "bytes9" , BYTES9;
      "bytes10", BYTES10;
      "bytes11", BYTES11;
      "bytes12", BYTES12;
      "bytes13", BYTES13;
      "bytes14", BYTES14;
      "bytes15", BYTES15;
      "bytes16", BYTES16;
      "bytes17", BYTES17;
      "bytes18", BYTES18;
      "bytes19", BYTES19;
      "bytes20", BYTES20; 
      "bytes21", BYTES21;
      "bytes22", BYTES22;
      "bytes23", BYTES23;
      "bytes24", BYTES24;
      "bytes25", BYTES25;
      "bytes26", BYTES26;
      "bytes27", BYTES27;
      "bytes28", BYTES28;
      "bytes29", BYTES29;
      "bytes30", BYTES30;
      "bytes31", BYTES31;
      "bytes32", BYTES32;
      "int", INT256;
      "int8",INT8;
      "int16",INT16;
      "int24",INT24;
      "int32",INT32;
      "int40",INT40;
      "int48",INT48;
      "int56",INT56;
      "int64",INT64;
      "int72",INT72;
      "int80",INT80;
      "int88",INT88;
      "int96",INT96;
      "int104",INT104;
      "int112",INT112;
      "int120",INT120;
      "int128",INT128;
      "int136",INT136;
      "int144",INT144;
      "int152",INT152;
      "int160",INT160;
      "int168",INT168;
      "int176",INT176;
      "int184",INT184;
      "int192",INT192;
      "int200",INT200;
      "int208",INT208;
      "int216",INT216;
      "int224",INT224;
      "int232",INT232;
      "int240",INT240;
      "int248",INT248;
      "int256",INT256;
      "uint", UINT256;
      "uint8",UINT8;
      "uint16",UINT16;
      "uint24",UINT24;
      "uint32",UINT32;
      "uint40",UINT40;
      "uint48",UINT48;
      "uint56",UINT56;
      "uint64",UINT64;
      "uint72",UINT72;
      "uint80",UINT80;
      "uint88",UINT88;
      "uint96",UINT96;
      "uint104",UINT104;
      "uint112",UINT112;
      "uint120",UINT120;
      "uint128",UINT128;
      "uint136",UINT136;
      "uint144",UINT144;
      "uint152",UINT152;
      "uint160",UINT160;
      "uint168",UINT168;
      "uint176",UINT176;
      "uint184",UINT184;
      "uint192",UINT192;
      "uint200",UINT200;
      "uint208",UINT208;
      "uint216",UINT216;
      "uint224",UINT224;
      "uint232",UINT232;
      "uint240",UINT240;
      "uint248",UINT248;
      "uint256",UINT256]
}

let pragma_token = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '.' ':']+

let identifier = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

(* "\\nrt" *)
let escape_sequence = "\\" (['\'' '\"' '\\' '\n' '\r' ] 
| 'u' ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'] 
| 'x' ['a'-'f' 'A'-'F' '0'-'9'] ['a'-'f' 'A'-'F' '0'-'9'])

let double_quoted_literal = [^'\"' '\\']

let single_quoted_literal = [^'\'' '\\']

let string_literal = '"' double_quoted_literal (escape_sequence? double_quoted_literal)* '"' | '\'' single_quoted_literal (escape_sequence? single_quoted_literal)* '\''

let int_literal = '-'? ['0'-'9'] ['0'-'9']*

let bool_literal = "true" | "false"

let version_literal = ['0' - '9']+ '.' ['0' - '9']+ '.' ['0' - '9']+ 

let hex_num_literal = "0x" ['a'-'f' 'A'-'F' '0'-'9']  (['_']? ['a'-'z' 'A'-'Z' '0'-'9'])*

let hex_string_literal = "hex" ('"' ['a'-'f' 'A'-'F' '0'-'9'] (['_']? ['a'-'f' 'A'-'F' '0'-'9'])*  '"' | '\'' ['a'-'f' 'A'-'F' '0'-'9'] (['_']? ['a'-'f' 'A'-'F' '0'-'9'])* '\'')

let dec_num_literal =  (['0'-'9'] ['_']?)+ ('.' ['0'-'9'] ['_']?)+?  ('E'|'e')? '-'? (['0'-'9'] ['_']?)+? 


let evm_builtin =  "stop" | "add"  | "sub"  | "mul"  | "div" | "sdiv" | "mod" | "smod" | "exp" | "not" | "lt" | "gt" | "slt" | "sgt" | "eq" | "iszero" | "and" 
| "or" | "xor" | "byte" | "shl" | "shr" | "sar" | "addmod" | "mulmod" | "signextend" | "keccack256" | "pop" | "mload" | "mstore" | "mstore8" | "sload" 
| "sstore" | "msize" | "gas" | "balance" | "selfbalance"  | "caller" | "callvalue" | "calldataload" | "calldatasize" | "calldatacopy" | "extcodesize" 
| "extcodecopy"  | "returndatasize" | "returndatacopy" | "extcodehash"  | "create" | "create2" | "call" | "callcode" | "delegatecall" | "staticcall"   | "revert" 
| "selfdestruct" | "invalid" | "log0" | "log1" | "log2" | "log3" | "log4"  | "chainid" | "origin" | "gasprice" | "blockhash" | "coinbase" 
| "timestamp" | "number" | "difficulty" | "gaslimit"


(* TODO semantic actions *)

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
| "struct"          { STRUCT }
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

(* Yul keywords*)
| "let"             { LET }
| "leave"           { LEAVE }
| "switch"          { SWITCH }
| "case"            { CASE }
| "default"         { DEFAULT }

(* Yul EVM Builtin *)
| evm_builtin       { EVM_BUILTIN }


(*Elementary Type names *)
| "enum"            { ENUM }
| "address"         { ADDRESS_T }
| "bytes"           { BYTES_T }
| "bool"            { BOOL_T }
| "string"          { STRING_T }
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


(* Identifiers *)
| pragma_token      { PRAGMA_TOKEN (Lexing.lexeme lexbuf) }
| identifier        { try
                        Hashtbl.find lookup_table identifier
                      with Not_found -> 
                        IDENTIFIER (Lexing.lexeme lexbuf) }

(* Literals *)
| bool_literal      { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
| string_literal    { STRING (Lexing.lexeme lexbuf) }
| hex_string_literal{ HEX_STRING (Lexing.lexeme lexbuf)}
| hex_num_literal   { HEX_NUMBER (int_of_hex (Lexing.lexeme lexbuf))}
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

(* Yul operators *)
| ":="              { ASSIGN }
| "->"              { ARROW }

(* End *)
| eof               { EOF }
| _                 { CATCHALL }