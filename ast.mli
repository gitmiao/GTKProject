type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
     GetPty of expr * string * expr list
    | Binop of expr * operator * expr
    | Lit of int
    | StrLit of string
    | Char of char
    | Var of string
    | Seq of expr * expr
    | Asn of string * expr
    | Puts of expr
    | Paren of expr
    | Window of string
    (* variable name, container (window) variable name, widget type and argument list *)
    | Create of string * expr * string * expr list  
    | SetPty of expr * string * expr list
    (* expr(variable or getPty), action type, callback function *)
    | Action of expr * string * expr 
    | Show of expr
    | GtkMain
    | If of expr * expr * expr
    | IfNoElse of expr * expr
    | While of expr * expr
    (* return type, c function name, c function argument list *)
    | CallC of string * string * expr list
    | NoExpr

type stmt =
    Expr of expr
    | Return of expr   
    
type preProcess =
	  IncludeSys of string
	| IncludeUser of string

type program = 
{
	preProcList: preProcess list;
	progBody: stmt;
}	    

