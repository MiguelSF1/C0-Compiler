module CodeInter where

import AST

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State

type Temp  = String
type Label = String


data Instr
  = MOVE Temp Temp                
  | MOVEI Temp Int               
  | MOVES Temp String
  | OP Op Temp Temp Temp  
  | OPNOT Temp Temp    
  | OPI Op Temp Temp Int      
  | LABEL Label    
  | FUN Label [Temp]             
  | JUMP Label                   
  | COND Temp Op Temp Label Label  
  | CALL Temp Label [Temp]      
  | RETURN Temp
  | PRINT Temp           
  | SCAN      
  | BREAK
  | CONTINUE   
  deriving (Eq, Show)

type Table = Map Ident Temp

type Supply = (Int, Int)   



newTemp :: State Supply Temp
newTemp 
  = do (temps,labels) <- State.get
       State.put (temps+1, labels)
       return ("t"++show temps)

newLabel :: State Supply Label 
newLabel
  = do (temps,labels) <- State.get
       State.put (temps, labels+1)
       return ("L"++show labels)


newTemps :: Int -> State Supply [Temp]
newTemps n | n > 0 = do
               t <- newTemp
               ts <- newTemps (n-1)
               return (t:ts)
           | otherwise = return []


reuseTemps :: Int -> State Supply ()
reuseTemps n
  = do (temps, labels) <- State.get
       State.put (temps-n, labels)

---------------------------------------------------------------------------

transExpr :: Expr -> Table -> Temp -> State Supply [Instr]
transExpr (Id x) tabl dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "undefined variable"

transExpr (Num n) tabl dest 
  = return [MOVEI dest n]

transExpr (Boolean True) table dest 
  = return [MOVEI dest 1]

transExpr (Boolean False) table dest 
  = return [MOVEI dest 0]

transExpr (Str s) tabl dest
  = return [MOVES dest s] 

transExpr (Not expr) tabl dest
  = do temp1 <- newTemp
       code1 <- transExpr expr tabl temp1
       reuseTemps 1
       return (code1 ++ [OPNOT dest temp1 ])  

transExpr (Op op e1 e2) tabl dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr e1 tabl temp1 
       code2 <- transExpr e2 tabl temp2
       reuseTemps 2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])

transExpr (FuncTerm id args) tabl dest
  = do (code, temps) <- transArgs args tabl
       reuseTemps (length temps)
       return (code ++ [CALL dest id temps])

transExpr GetIO tabl dest = return [SCAN]

transArgs :: [Expr] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
      = do temp <- newTemp 
           code <- transExpr exp tabl temp 
           (code', temps') <- transArgs exps tabl
           return (code++code', temp:temps')

transStm :: Stmt -> Table -> State Supply [Instr]
transStm (AssignStmt var expr) tabl
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> transExpr expr tabl dest

transStm (DeclrStmt tp (x:xs)) table = return []
                                         

transStm (IncrStmt var) table
  = case Map.lookup var table of
      Nothing -> error "undefined variable"
      Just dest -> return [OPI Plus dest dest 1]

transStm (DecrStmt var) table
  = case Map.lookup var table of
      Nothing -> error "undefined variable"
      Just dest -> return [OPI Minus dest dest 1]
                      

transStm (IfStmt cond stm1) tabl 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond cond tabl ltrue lfalse 
       code1  <- transStm stm1 tabl
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])


transStm (IfElseStmt cond stm1 stm2) tabl
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond cond tabl  ltrue lfalse 
       code1 <- transStm stm1 tabl 
       code2 <- transStm stm2 tabl 
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++
               [LABEL lend])

transStm (WhileStmt cond stm) table
  = do loop <- newLabel
       lnext <- newLabel
       lend <- newLabel
       code1 <- transCond cond table lnext lend
       code2 <- transStm stm table
       code3 <- breakStm code2 lend 
       code4 <- continueStm code3 loop
       return ([LABEL loop] ++ code1 ++ [LABEL lnext] ++
         code4 ++ [JUMP loop, LABEL lend])

transStm (ReturnStmt expr) tabl =
  do dest <- newTemp
     code <- transExpr expr tabl  dest
     reuseTemps 1
     return (code ++ [RETURN dest])

transStm (BlockStmt stms) tabl =
  transStmList stms tabl 

transStm (Expr expr) tabl = 
  do dest <- newTemp
     code <- transExpr expr tabl dest
     reuseTemps 1
     return code

transStm (SendIO expr) tabl =
  do dest <- newTemp
     code <- transExpr expr tabl dest
     reuseTemps 1
     return (code ++ [PRINT dest])

transStm BreakStmt tabl = return [BREAK]

transStm ContinueStmt tabl = return [CONTINUE]

breakStm :: [Instr] -> Label -> State Supply [Instr]
breakStm code lend
  | BREAK `elem` code = replace BREAK (JUMP lend) code
  | otherwise = return code

continueStm :: [Instr] -> Label -> State Supply [Instr]
continueStm code loop
  | CONTINUE `elem` code = replace CONTINUE (JUMP loop) code
  | otherwise = return code

replace :: Instr -> Instr -> [Instr] -> State Supply [Instr]
replace _ _ [] = return []
replace stm1 stm2 (x:xs)
  | stm1 == x =
      do rest <- replace stm1 stm2 xs
         return (stm2 : rest)
  | otherwise =
      do rest <- replace stm1 stm2 xs
         return (x : rest)    
  
transCond :: Expr -> Table -> Label -> Label -> State Supply [Instr]
transCond (Op rel e1 e2) tabl ltrue lfalse 
  | rel == LessThan || rel == LessThanOrEqual || rel == Equal || rel == GreaterThan || rel == GreaterThanOrEqual || rel == NotEqual =
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr e1 tabl temp1
         code2 <- transExpr e2 tabl temp2
         reuseTemps 2
         return ( code1 ++ code2 ++
                  [COND temp1 rel temp2 ltrue lfalse] )

transCond (Not expr) tabl labelt labelf = do temp1 <- transCond expr tabl labelf labelt
                                             return temp1


transCond (Op And e1 e2) tabl labelt labelf = do uselssLabel <- newLabel
                                                 code1 <- transCond e1 tabl uselssLabel labelf
                                                 code2 <- transCond e2 tabl labelt labelf
                                                 return (code1++[LABEL uselssLabel]++code2)

transCond (Op Or e1 e2) tabl labelt labelf = do uselssLabel <- newLabel
                                                code1 <- transCond e1 tabl labelt uselssLabel
                                                code2 <- transCond e2 tabl labelt labelf
                                                return (code1++[LABEL uselssLabel]++code2)       

transCond expr tabl labelt labelf =
  do temp <- newTemp
     code <- transExpr expr tabl temp
     reuseTemps 1
     return (code ++ [COND temp Equal "1" labelt labelf])                                          


transStmList :: [Stmt] -> Table -> State Supply [Instr]
transStmList [] _ = return [] 
transStmList (stm:rest) tabl = do
  let temps1 = case stm of 
             (DeclrStmt ty vars) -> newTemps (length vars)
             _ -> newTemps 0
  temps <- temps1
  let table = case stm of
             (DeclrStmt ty vars) -> Map.union (Map.fromList (zip vars temps)) tabl
             _ -> tabl
  code1 <- transStm stm tabl
  code2 <- transStmList rest table
  return (code1 ++ code2)


transFunDef :: Func -> State Supply [Instr]
transFunDef (Func ty fun args body) 
  = do targs <- newTemps (length (args))   
       let table = Map.fromList (zip (map snd args) targs)
       code <- transStmList body table
       return (FUN fun targs : code)

transFuncList :: [Func] -> State Supply [Instr]
transFuncList [] = return []
transFuncList (func:rest) = do
  code1 <- transFunDef func 
  code2 <- transFuncList rest
  return (code1 ++ code2)  


codeinter :: [Func] -> [Instr]
codeinter p = State.evalState (transFuncList p) (0,0)
