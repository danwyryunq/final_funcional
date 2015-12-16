import System.Environment

import ParserLib
import LISRepresentation
import LISParser
import LISCompilation
import AssemblyOptimization
import AssemblyExecution


-- main

main = getArgs >>= run . head


-- run

run filename = do code <- readFile filename
                  let prog = parseLIS $ code
                      res = execProgram $
                            optimizeProgram $
                            compileProgram $ 
                            prog
                   in do print prog
                         putStrLn ""
                         print res


-- parseLIS

parseLIS = fst . (bestFirst lisParser) . lisLexer

