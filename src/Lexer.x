{
module Lexer where
}

%wrapper "basic"

$digit =  [0-9] -- dar match a digitos
$alpha = [_a-zA-Z]  -- dar match a letras ou _ 

tokens :-

$white+    ;   -- ignorar carateres "brancos"
"/*" (. | \n | [^\/\*]) * "*/" ; -- ignorar comentarios multi linha
"//".*               ; -- ignorar comentarios uma linha

"if"                    { \_ -> IF }
"else"                  { \_ -> ELSE }
"while"                 { \_ -> WHILE }
"break"                 { \_ -> BREAK }
"continue"              { \_ -> CONTINUE }
"int"                   { \_ -> INT }
"bool"                  { \_ -> BOOLEAN }
"string"                { \_ -> STRING }
","                     { \_ -> COMMA }
";"                     { \_ -> SEMICOLON }
"++"                    { \_ -> INCR }
"--"                    { \_ -> DECR }
"="                     { \_ -> ASSIGN } 
"+"                     { \_ -> PLUS }
"-"                     { \_ -> MINUS }
"*"                     { \_ -> MULT }
"/"                     { \_ -> DIV }
"%"                     { \_ -> MOD }
"=="                    { \_ -> EQUAL }
"!="                    { \_ -> NOTEQUAL }
"<"                     { \_ -> LESSTHAN }
"<="                    { \_ -> LESSTHANOREQUAL }
">"                     { \_ -> GREATERTHAN }
">="                    { \_ -> GREATERTHANOREQUAL }
"&&"                    { \_ -> AND }
"||"                    { \_ -> OR }
"!"                     { \_ -> NOT }
"("                     { \_ -> LPAREN }
")"                     { \_ -> RPAREN }
"{"                     { \_ -> LBRACE }
"}"                     { \_ -> RBRACE }
"scan_int"              { \_ -> SCANINT } 
"print_int"             { \_ -> PRINTINT }
"print_str"             { \_ -> PRINTSTR }
"return"                { \_ -> RETURN }
"true"                  { \_ -> BOOL True }
"false"                 { \_ -> BOOL False }
$alpha($alpha|$digit)*  { \s -> ID s }
$digit+                 { \s -> NUM (read s) }
\"(\\. | [^\"])*\"      { \s -> STR s}


{

data Token
  = IF
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | INT
  | BOOLEAN
  | STRING
  | COMMA
  | SEMICOLON
  | INCR
  | DECR
  | ASSIGN
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | EQUAL
  | NOTEQUAL
  | LESSTHAN
  | LESSTHANOREQUAL
  | GREATERTHAN
  | GREATERTHANOREQUAL
  | AND
  | OR
  | NOT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE  
  | SCANINT
  | PRINTINT 
  | PRINTSTR
  | RETURN
  | BOOL Bool
  | ID String
  | NUM Int
  | STR String
  deriving (Eq, Show) 

}





