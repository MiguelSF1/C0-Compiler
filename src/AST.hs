module AST where

type Ident = String

data Func 
  = Func Type String [(Type, String)] [Stmt]
  deriving Show


data Stmt
  = AssignStmt String Expr
  | IncrStmt String
  | DecrStmt String
  | SendIO Expr 
  | IfStmt Expr Stmt
  | IfElseStmt Expr Stmt Stmt
  | WhileStmt Expr Stmt
  | DeclrStmt Type [String]
  | ReturnStmt Expr
  | BlockStmt [Stmt]
  | BreakStmt
  | ContinueStmt
  | Expr Expr
  deriving Show


data Expr 
  = Num Int 
  | Id String
  | Boolean Bool 
  | Str String
  | GetIO
  | FuncTerm String [Expr]
  | Op Op Expr Expr
  | Not Expr
  deriving Show



data Op
  = Plus
  | Minus
  | Div
  | Mult
  | Mod
  | Equal 
  | NotEqual 
  | LessThan 
  | LessThanOrEqual 
  | GreaterThan 
  | GreaterThanOrEqual
  | And 
  | Or
  deriving (Eq, Show)


data Type
  = Int
  | Bool 
  | String
   deriving (Show, Eq)
