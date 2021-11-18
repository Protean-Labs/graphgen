%{
  open Easy_logging
  open Gg_script
  open Rresult
%}

// Function keywords
%token FUNCTION
%token PUBLIC
%token PRIVATE
%token EXTERNAL
%token INTERNAL
%token PURE
%token VIEW
%token PAYABLE
%token RETURNS

// Event keywords
%token ANON
%token INDEXED
%token EVENT

// Elementary Type names
// %token ENUM
%token SOL_ADDRESS
%token SOL_BOOL
%token SOL_STRING
%token SOL_FIXED
%token SOL_UFIXED
%token SOL_BYTES
%token <string> SOL_FBYTES
%token <string> SOL_INT
%token <string> SOL_UINT

%token <string> GG_BLOCK GG_HANDLER

%token <string> IDENT

%token EOF
%start <Parsetree.toplevel list> solidity
%%

solidity:
  | gg = GG_BLOCK
    { Gg_script.(Parser.document Lexer.token (Lexing.from_string gg)) }
  | gg = GG_HANDLER
    { Gg_script.(Parser.actions Lexer.token (Lexing.from_string gg)) }

event_definition:
  | EVENT id = IDENT LPAREN params = separated_list(COMMA, event_parameter) RPAREN option(ANON) SEMICOLON
    { (id, params) }

event_parameter:
  | typ = sol_type indexed = boption(INDEXED) id = option(IDENT)
    { (typ, indexed, id) }

sol_type:
  | typ = sol_type LBRACK RBRACK  { SOLArray typ}
  | INDEXED typ = sol_type        { SOLIndexed typ }
  | SOL_ADDRESS                   { SOLAddress }
  | SOL_BOOL                      { SOLBool }
  | SOL_STRING                    { SOLString }
  | SOL_FIXED                     { SOLFixed }
  | SOL_UFIXED                    { SOLUfixed }
  | SOL_BYTES                     { SOLBytes }
  | v = SOL_FBYTES                { SOLFbytes (parse_int_postfix "fbytes" v) }
  | v = SOL_INT                   { SOLInt (parse_int_postfix "int" v) }
  | v = SOL_UINT                  { SOLUint (parse_int_postfix "uint" v) }
