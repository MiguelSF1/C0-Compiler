{
module Parser where
import Lexer  
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

"if"                    { IF }
"else"                  { ELSE }
"while"                 { WHILE }
"break"                 { BREAK }
"continue"              { CONTINUE }
"int"                   { INT }
"bool"                  { BOOLEAN }
"string"                { STRING }
","                     { COMMA }
";"                     { SEMICOLON }
"++"                    { INCR }
"--"                    { DECR }
"="                     { ASSIGN }
"+"                     { PLUS }
"-"                     { MINUS }
"*"                     { MULT }
"/"                     { DIV }
"%"                     { MOD }
"=="                    { EQUAL }
"!="                    { NOTEQUAL }
"<"                     { LESSTHAN }
"<="                    { LESSTHANOREQUAL }
">"                     { GREATERTHAN }
">="                    { GREATERTHANOREQUAL }
"&&"                    { AND }
"||"                    { OR }
"!"                     { NOT }
"("                     { LPAREN }
")"                     { RPAREN } 
"{"                     { LBRACE }
"}"                     { RBRACE }
"scan_int"              { SCANINT }
"print_int"             { PRINTINT }
"print_str"             { PRINTSTR }
"return"                { RETURN } 
bool                    { BOOL $$ }
id                      { ID $$ }
num                     { NUM $$ }
str                     { STR $$ }


%nonassoc "==" "!=" "<" "<=" ">"  ">=" 
%left "+" "-"
%left "*" "/"
%left "%"
%left "!" "&&" "||" 


%%


Prog : Func                                           { [$1] }
     | Func Prog                                      { $1 : $2 }


Func : Type id "(" Args ")" "{" Stmts "}"             { Func $1 $2 $4 $7 }
     | Type id "(" ")"  "{" Stmts "}"                 { Func $1 $2 [] $6 }
     | Type id "(" Args ")" "{" "}"                   { Func $1 $2 $4 [] }
     | Type id "(" ")"  "{" "}"                       { Func $1 $2 [] [] }


Args : Type id "," Args                               { ($1, $2) : $4 }
     | Type id                                        { [($1, $2)] }


Stmts : Stmt                                          { [$1] }                         
      | Stmt  Stmts                                   { $1 : $2 }


Stmt : id "=" Expr ";"                                { AssignStmt $1 $3 }
     | id "++" ";"                                    { IncrStmt $1 }
     | id "--" ";"                                    { DecrStmt $1 }
     | "print_int" "(" Expr ")" ";"                   { SendIO $3 }
     | "print_str" "(" Expr ")" ";"                   { SendIO $3 } 
     | "if" "(" Expr ")" StmtType                     { IfStmt $3 $5 }
     | "if" "(" Expr ")" StmtType "else" StmtType     { IfElseStmt $3 $5 $7 }
     | "while" "(" Expr ")" StmtType                  { WhileStmt $3 $5 } 
     | "break" ";"                                    { BreakStmt }
     | "continue" ";"                                 { ContinueStmt }       
     | Type ids ";"                                   { DeclrStmt $1 $2 }
     | "return" Expr ";"                              { ReturnStmt $2 }
     | Expr ";"                                       { Expr $1 } 
     

StmtType : Stmt                                       { $1 }
         | "{" Stmts "}"                              { BlockStmt $2 }
         | "{" "}"                                    { BlockStmt [] }
         

Expr : Expr "+" Expr                                  { Op Plus $1 $3 }
     | Expr "-" Expr                                  { Op Minus $1 $3 }
     | Expr "/" Expr                                  { Op Div $1 $3 }
     | Expr "*" Expr                                  { Op Mult $1 $3 }
     | Expr "%" Expr                                  { Op Mod $1 $3 }
     | Expr "==" Expr                                 { Op Equal $1 $3 }
     | Expr "!=" Expr                                 { Op NotEqual $1 $3 }
     | Expr "<"  Expr                                 { Op LessThan $1 $3 }
     | Expr "<=" Expr                                 { Op LessThanOrEqual $1 $3 } 
     | Expr ">"  Expr                                 { Op GreaterThan $1 $3 }
     | Expr ">=" Expr                                 { Op GreaterThanOrEqual $1 $3 }
     | Expr "&&" Expr                                 { Op And $1 $3 }
     | Expr "||" Expr                                 { Op Or $1 $3 }
     | "!" Expr                                       { Not $2 }
     | num                                            { Num $1 } 
     | id                                             { Id $1 }
     | bool                                           { Boolean $1 }
     | str                                            { Str $1 }
     | "scan_int" "(" ")"                             { GetIO }
     | "(" Expr ")"                                   { $2 }
     | id "(" FuncTermArgs ")"                        { FuncTerm $1 $3 }
     | id "(" ")"                                     { FuncTerm $1 [] }



FuncTermArgs : Expr "," FuncTermArgs                  { $1 : $3 }
             | Expr                                   { [$1] }
     

ids : id                                              { [$1] }
    | id "," ids                                      { $1 : $3 }


Type : "int"                                          { Int }
     | "bool"                                         { Bool }
     | "string"                                       { String }


{

parseError :: [Token] -> a
parseError toks = error ("Parse error at " ++ show toks)  

}   

