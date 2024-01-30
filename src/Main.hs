module Main where

import Lexer
import AST
import Parser
import CodeInter
import CodeMachi


printIR :: [Instr] -> IO ()
printIR = mapM_ print


main :: IO ()
main = do
  txt <- getContents
  print (parse $ alexScanTokens txt)
  printIR (codeinter $ parse $ alexScanTokens txt)
  makeFullMachiCode $ codeinter $ parse $ alexScanTokens txt


