%{
	open Refract.AST
	
	let i = ref 0
	
	let label () =
		i := !i + 1;
		!i
	
	let proc_num = ref 0
	
	let new_proc _ = proc_num := !proc_num + 1
	
	let var x = x ^ "_" ^ string_of_int(!proc_num) 
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token IF FI DO OD PIF FIP
%token SKIP ASSIGN SEND RECEIVE
%token TRUE FALSE AND OR NOT EQ LT GT NEQ
%token PLUS MINUS TIMES DIVIDE RAND
%token PROCESS
%token PIPE LPAR RPAR LBRACE RBRACE LBRACK RBRACK SEMICOLON COLON EOF COMMA
%token INTEGER BOOLEAN CHAN

%right SEMICOLON
%right ASSIGN
%nonassoc EQ LT GT
%right OR
%right AND
%nonassoc NOT
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <Refract.AST.chan_decl list * Refract.AST.process list> model
%type <Refract.AST.bexpr> bexpr
%%

vtype:
  | INTEGER { Int }
	| BOOLEAN { Bool }

init_value:
	| ASSIGN INT { $2 }

var_decl:
	| vtype ID init_value? SEMICOLON { VarDecl($1, var $2, $3) }

chan_decl:
	| CHAN vtype LBRACK INT RBRACK ID SEMICOLON { ChanDecl($2, $6, $4) }

bexpr:
	| ID { BVar(var $1) }
	| TRUE { True }
	| FALSE { False }
	| LPAR bexpr RPAR { $2  }
	| NOT bexpr { Not($2) }
	| aexpr NEQ aexpr { Not(Eq($1, $3)) }
	| aexpr EQ aexpr  { Eq($1, $3) }
	| aexpr LT aexpr  { Lt($1, $3) }
	| aexpr GT aexpr  { Gt($1, $3) }
	| bexpr AND bexpr { And($1, $3) }
	| bexpr OR bexpr  { Or($1, $3) }

aexpr:
	| ID { AVar(var $1) }
	|	INT { IConst($1) }
	| PIPE ID PIPE { ChanSize($2) }
	| LPAR aexpr RPAR { $2  }
	| MINUS aexpr %prec UMINUS { UMinus($2) }
	| aexpr MINUS aexpr { Minus($1, $3) }
	| aexpr PLUS aexpr { Plus($1, $3) }
	| aexpr TIMES aexpr { Times($1, $3) }
	| aexpr DIVIDE aexpr { Divide($1, $3) }

guard:
  | bexpr { (label(), None, $1) }
	| bexpr AND ID RECEIVE ID { (label(), Some(Receive($3, var $5)), $1) }
	| bexpr AND ID SEND aexpr { (label(), Some(Send($3, $5)), $1) }

branch:
	| COLON COLON LPAR guard RPAR MINUS GT stm { ($4, $8) }

choice:
  | COLON COLON FLOAT MINUS GT stm { (label(), $3, $6) }

stm:
	| SKIP { Skip(label()) }
	| ID ASSIGN RAND LPAR separated_nonempty_list(COMMA, aexpr) RPAR { Choose(label(), List.map (fun aexp -> (label(), 1.0/.(float_of_int (List.length $5)), Assign(label(), var $1, aexp))) $5) }
	| ID ASSIGN aexpr { Assign(label(), var $1, $3) }
	| ID RECEIVE ID { Comm(label(), Receive($1, var $3)) }
	| ID SEND aexpr { Comm(label(), Send($1, $3)) }
	| IF branch+ FI { Select(label(), $2) }
	| DO branch+ OD { Repeat(label(), $2) }
	| PIF choice+ FIP { Choose(label(), $2) }
	| stm SEMICOLON stm { Seq($1, $3) }

process:
	| PROCESS ID LBRACE var_decl* stm RBRACE { new_proc $2; ($2, $4, $5) }

model: chan_decl* process+ EOF { ($1, $2) }