module CodeMachi where
import AST
import CodeInter

makeFullMachiCode:: [Instr] -> IO ()
makeFullMachiCode instr = 
  do makeMachiCode instr
     getDefinedFunc

makeMachiCode:: [Instr] -> IO ()
makeMachiCode [] = return ()
makeMachiCode ((FUN fun args):xs) =
  do putStrLn (fun ++ ":")
     putStrLn ("\tsw $fp, -4($sp)")
     putStrLn ("\tsw $ra, -8($sp)")
     putStrLn ("\tla $fp, 0($sp)")
     putStrLn ("\tla $sp, " ++ show( (length args * (-4))) ++ "($sp)")
     getArgs args 0 
     makeMachiCode xs

makeMachiCode ((LABEL l):xs) = 
  do putStrLn (l ++ ":")
     makeMachiCode xs

makeMachiCode ((JUMP l):xs) = 
  do putStrLn("\tj " ++ l)
     makeMachiCode xs

makeMachiCode ((PRINT t):xs) =
  do putStrLn ("\tla $sp, -4($sp)")                 
     putStrLn ("\tjal print")                                          
     putStrLn ("\tla $sp, 4($sp)")                                         
     putStrLn ("\tmove $" ++ t ++ ", $v0")                                              
     makeMachiCode xs

makeMachiCode ((SCAN):xs) = 
  do putStrLn ("\tjal scan_int")
     makeMachiCode xs

makeMachiCode ((MOVE t1 t2):xs) = 
  do putStrLn("\tmove $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs 

makeMachiCode ((MOVEI t i):xs) = 
  do putStrLn("\tli $" ++ t ++ " " ++ (show i))
     makeMachiCode xs

makeMachiCode ((MOVES t s):xs) =
  do putStrLn("\tli $" ++ t ++ " " ++ s) 

makeMachiCode ((OPNOT dest t):xs) =
  do putStrLn("\tnor $" ++ dest ++ " $" ++ t ++ " $zero")

makeMachiCode ((OPI Plus dest t i):xs) =
  do putStrLn("\taddi $" ++ dest ++ " $" ++ t ++ " " ++ (show i))
     makeMachiCode xs

makeMachiCode ((OP Plus dest t1 t2):xs) = 
  do putStrLn("\tadd $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs 

makeMachiCode ((OP Minus dest t1 t2):xs) =
  do putStrLn("\tsub $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs

makeMachiCode ((OP Div dest t1 t2):xs) = 
  do putStrLn("\tdiv $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2) 
     makeMachiCode xs 

makeMachiCode ((OP Mult dest t1 t2):xs) = 
  do putStrLn("\tmul $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs

makeMachiCode ((OP Mod dest t1 t2):xs) = 
  do putStrLn("\tdiv $" ++ t1 ++ " $" ++ t2 ++ "mdhi $" ++ dest)
     makeMachiCode xs

makeMachiCode ((OP And dest t1 t2):xs) = 
  do putStrLn("\tand $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs 

makeMachiCode ((OP Or dest t1 t2):xs) = 
  do putStrLn("\tor $" ++ dest ++ " $" ++ t1 ++ " $" ++ t2)
     makeMachiCode xs 

makeMachiCode ((RETURN t):xs) = 
  do putStrLn("\tmove $v0 $" ++ t)
     putStrLn("\tla $sp, 0($fp)")
     putStrLn("\tlw $ra, -8($sp)")
     putStrLn("\tlw $fp, -4($sp)")
     putStrLn("\tjr $ra")
     makeMachiCode xs 

makeMachiCode ((CALL dest fun args):xs) =      
  do saveArgs (reverse args) 1
     putStrLn ("\tla $sp, " ++ (show ((-4)*(length args))) ++ "($sp)")   
     putStrLn ("\tjal " ++ fun)                                               
     putStrLn ("\tla $sp, " ++ (show (4*(length args))) ++ "($sp)")      
     putStrLn ("\tmove $" ++ dest ++ ", $v0")  
     makeMachiCode xs

makeMachiCode ((COND t1 Equal t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tbne $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs 
  | l == lf = 
    do putStrLn("\tbeq $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs 
  | otherwise = error ("invalid label")

makeMachiCode ((COND t1 NotEqual t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tbeq $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs 
  | l == lf = 
    do putStrLn("\tbne $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs 
  | otherwise = error ("invalid label")

makeMachiCode ((COND t1 LessThan t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tbge $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs 
  | l == lf = 
    do putStrLn("\tblt $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs
  | otherwise = error ("invalid label")

makeMachiCode ((COND t1 GreaterThan t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tble $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs
  | l == lf = 
    do putStrLn("\tbgt $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs
  | otherwise = error ("invalid label")

makeMachiCode ((COND t1 LessThanOrEqual t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tbgt $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs
  | l == lf = 
    do putStrLn("\tble $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs
  | otherwise = error ("invalid label")

makeMachiCode ((COND t1 GreaterThanOrEqual t2 lt lf):(LABEL l):xs)
  | l == lt = 
    do putStrLn("\tblt $" ++ t1 ++ " $" ++ t2 ++ " " ++ lf)
       makeMachiCode xs
  | l == lf = 
    do putStrLn("\tbge $" ++ t1 ++ " $" ++ t2 ++ " " ++ lt)
       makeMachiCode xs
  | otherwise = error ("invalid label")    


getArgs :: [Temp] -> Integer -> IO ()
getArgs [] _ = return ()
getArgs (a:as) n = 
  do putStrLn("\tlw $" ++ a ++ ", " ++ (show n) ++ "($fp)") 
     getArgs as (n+4)

saveArgs :: [Temp] -> Integer -> IO ()
saveArgs [] _ = return ()
saveArgs (arg:args) n = 
  do putStrLn ("\tsw $" ++ arg ++ ", " ++ (show (-4 * n)) ++ "($sp)")
     saveArgs args (n+1)


getDefinedFunc:: IO ()
getDefinedFunc = 
  do putStrLn ("print:")
     putStrLn ("\tli $v0, 1")
     putStrLn ("\tlw $a0, 0($sp)")
     putStrLn ("\tsyscall")
     putStrLn ("\tjr $ra")
     putStrLn ("scan_int:")
     putStrLn ("\tli $v0, 5")
     putStrLn ("\tsyscall")
     putStrLn ("\tjr $ra")