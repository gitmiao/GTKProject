{ open Parser
  open String
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let punc = ['\\' '~' '`' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '+' '=' ',' '.' '?' '/' '<' '>' ':' ''' ';' '{' '}' '[' ']' '|' ' ']
let stringlit = '"' (letter | digit | punc )* '"'

rule token = parse 
     [' ' '\t' '\r' '\n'] { token lexbuf }
   | "(*"                 { comment lexbuf }
         | "->"     { ARROW } 
   | '+'      { PLUS }    | '*'      { TIMES } 
   | '-'      { MINUS }   | '/'      { DIVIDE }
   | "=="     { EQ }
   | "!="     { NEQ }
   | '<'      { LT }
   | "<="     { LEQ }
   | ">"      { GT }
   | ">="     { GEQ }   
   | '('      { LPAREN }  | ')'      { RPAREN } 
   | '='      { ASSIGN }  | ','      { COMMA }
   | '.'      { DOT }
   | '{'      { LBRACKET }
   | '}'      { RBRACKET }
   | '#'      { POUND }
   | '\"'     { DQUOTE }
   | "include" { INCLUDE }
   | "puts"   { PUTS }    | "return" { RETURN }
   | eof      { EOF }
   | "if"     { IF }
   | "else"   { ELSE }
   | "while"  { WHILE }
   | ';'      { SEMI }
   | "Window" { WINDOW }
   | "callC"  { CALLC }
   | "getText" | "getSelection" | "isSelected" | "getElement" as lit { GETPTY (lit) }
   | "setText" | "setSelected" as lit { SETPTY (lit) }
   | "Label" | "Button" | "TextEntry" 
   | "Combo" | "CheckBox" | "RadioButtonGroup" as lit { CREATE (lit) }
   | "click" as lit { ACTION (lit) }
   | "show"  { SHOW }
   | "gtkMain" { GTKMAIN }
   | ['0'-'9']+ as lit   { LITERAL(int_of_string lit) }
   | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as var { VARIABLE(var) }
   | (letter | '/' )+ ".h" as lex { HEADER(lex) }   
   | stringlit as strlit  
       { STRLITERAL(String.sub strlit 1 (String.length strlit - 2)) }
   | '''['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']''' as charlit
       { CHAR(charlit.[1]) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }       

and comment = parse
    "*)"      { token lexbuf }
   | _        { comment lexbuf }
