%{ open Ast %}

%token LPAREN RPAREN LBRACKET RBRACKET DOT ARROW
%token PLUS MINUS TIMES DIVIDE EOF PUTS COMMA ASSIGN SEMI
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE WHILE  CALLC
%token POUND DQUOTE INCLUDE 
%token <string> HEADER
%token <int> LITERAL
%token <string> STRLITERAL
%token <char> CHAR
%token <string> VARIABLE
%token <string> GETPTY
%token <string> SETPTY
%token <string> CREATE
%token <string> ACTION
%token SHOW
%token WINDOW 
%token GTKMAIN
%token RETURN

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
/*%left SETPTY CREATE ACTION*/
%left ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left PUTS
%left DOT ARROW
/*%left GETPTY*/

%start program
%type <Ast.program> program
/*%type <Ast.expr> expr */
/*%type <Ast.stmt> stmt */

%%

program: 
	preproc_list stmt {{preProcList=List.rev $1; progBody=$2}}

preproc_list:
    /* nothing */ { [] }
  | preproc_list preproc { $2 :: $1 } 
  
preproc:
      POUND INCLUDE LT HEADER GT {IncludeSys($4)}
    | POUND INCLUDE STRLITERAL {IncludeUser($3)} 

expr:
    /* no expr */   {NoExpr}
    | expr PLUS expr          { Binop($1, Add, $3) }
    | expr MINUS expr         { Binop($1, Sub, $3) }
    | expr TIMES expr         { Binop($1, Mul, $3) }
    | expr DIVIDE expr        { Binop($1, Div, $3) }
    | expr EQ     expr        { Binop($1, Equal, $3) }
    | expr NEQ    expr        { Binop($1, Neq,   $3) }
    | expr LT     expr        { Binop($1, Less,  $3) }
    | expr LEQ    expr        { Binop($1, Leq,   $3) }
    | expr GT     expr        { Binop($1, Greater,  $3) }
    | expr GEQ    expr        { Binop($1, Geq,   $3) }      
    | PUTS LPAREN expr RPAREN { Puts($3) }
    | LITERAL                 { Lit($1) }
    | STRLITERAL              { StrLit($1) }
    | CHAR                    { Char($1) }
    | VARIABLE                { Var($1) }    
    | VARIABLE ASSIGN expr    { Asn($1, $3) }
    | LPAREN expr RPAREN      { Paren($2) }
    | VARIABLE ASSIGN WINDOW LPAREN RPAREN   { Window($1) }
    | expr DOT GETPTY LPAREN actuals_opt RPAREN { GetPty($1, $3, $5) }
    | VARIABLE ASSIGN expr DOT CREATE LPAREN actuals_opt RPAREN { Create ($1, $3, $5, $7) }    
    | expr DOT SETPTY LPAREN actuals_opt RPAREN { SetPty($1, $3, $5) }
    | expr DOT ACTION LPAREN expr RPAREN { Action($1, $3, $5) }
    | expr DOT SHOW LPAREN RPAREN { Show ($1) }
    | GTKMAIN { GtkMain }
    | expr COMMA expr         { Seq($1, $3) }
    | IF LPAREN expr RPAREN LPAREN expr RPAREN ELSE LPAREN expr RPAREN { If ($3, $6, $10) }
    | IF LPAREN expr RPAREN LPAREN expr RPAREN %prec NOELSE { IfNoElse ($3, $6) }
    | WHILE LPAREN expr RPAREN LPAREN expr RPAREN { While ($3, $6) }
    | CALLC LPAREN STRLITERAL SEMI STRLITERAL SEMI actuals_opt RPAREN { CallC ($3, $5, $7) }

actuals_opt:
    actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list SEMI expr { $3 :: $1 }    

stmt:
      expr { Expr($1) }
    | RETURN LBRACKET expr RBRACKET { Return($3) }     
