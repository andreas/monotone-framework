{
	open Parser
}

rule token = parse
    [' ' '\t' '\r']      { token lexbuf } (* whitespace *)
  | ['\n' '\012']        { token lexbuf } (* newlines *)
  | "//" [^ '\n']*       { token lexbuf } (* comment *)
	| ['~']?['0'-'9']* '.' ['0'-'9']+ as lxm { Parser.FLOAT(float_of_string lxm) }
  | ['~']?['0'-'9']+ as lxm { INT(int_of_string lxm) }
	| "int" { INTEGER }
	| "bool" { BOOLEAN }
	| "chan" { CHAN }
	| "true" { TRUE }
	| "false" { FALSE }
	| "skip" { SKIP }
	| "if" { IF }
	| "fi" { FI }
	| "do" { DO }
	| "od" { OD }
	| "pif" { PIF }
	| "fip" { FIP }
	| "process" { PROCESS }
	| "rand" { RAND }
  | (['a'-'z'] | ['A'-'Z'] | '_') (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')* as lxm { ID(lxm) }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { TIMES }
  | '/'                  { DIVIDE }
  | '('                  { LPAR }
  | ')'                  { RPAR }
  | '['                  { LBRACK }
  | ']'                  { RBRACK }
  | '{'                  { LBRACE }
  | '}'                  { RBRACE }
	| "~="								 { NEQ }
  | '='                  { EQ }
  | '~'                  { NOT }
	| '|'									 { PIPE } 
  | "&&"                 { AND }
  | "||"                 { OR }
  | '>'                  { GT }
  | '<'                  { LT }
  | ":="                 { ASSIGN }
  | ';'                  { SEMICOLON }
  | ':'                  { COLON }
  | '!'                  { SEND }
  | '?'                  { RECEIVE }
	| ','									 { COMMA }
  | eof                  { EOF }
	| _ as lxm             { failwith ("Did not recognize: " ^ Char.escaped lxm) } 