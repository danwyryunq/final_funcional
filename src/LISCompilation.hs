module LISCompilation (
    compileProgram
) where

import AssemblyRepresentation
import LISRepresentation
import StateMonad
import MayFail
import Data.Maybe

-- compileProgram

compileProgram :: Program -> AssemblyProgram
compileProgram (Program bl) = tryCatch (evalState (compileBlock bl) startState)
                                (\mns -> AssemblyProgram mns)
                                (\ex  -> case ex of
                                          InvalidCase -> error "Duplicated case in switch"
                                          Other msg   -> error msg
                                          otherwise   -> error "Unexpected error")
-- initialIx

type Memory = Int

startState :: Memory
startState = 0


-- compileBlock

compileBlock :: Block -> State Memory [Mnemonic]

compileBlock []         =   return []
compileBlock (cmd:cmds) =   compileComm cmd   >>= \asmBlock -> 
                            compileBlock cmds >>= \asmBlocks ->
                            return $ asmBlock ++ asmBlocks

-- compileComm

compileComm :: Command -> State Memory [Mnemonic]

compileComm (Skip)                  = return [NoOp]

compileComm (Assign name nexp)      = (compileNExp nexp) >>= \asmBlock -> 
                                      return $ asmBlock ++ [Pop A, Store A name] 

compileComm (If bexp block1 block2) = (compileBExp bexp)          >>= \asmBlockBExp -> 
                                      (compileBlock block1)       >>= \asmBlockThenBlock ->
                                      (compileBlock block2)       >>= \asmBlockElseBlock ->
                                      genNewLabel                 >>= \etiquetaElse -> 
                                      genNewLabel                 >>= \etiquetaFin -> 
                                      return $ asmBlockBExp ++ [ Pop A , JumpIfZ A etiquetaElse ] ++ asmBlockThenBlock ++ [ Jump etiquetaFin, Mark etiquetaElse ] ++ asmBlockElseBlock ++ [ Mark etiquetaFin ]


compileComm (While bexp block)      = genNewLabel             >>= \marcaWhile -> 
                                      (compileBExp bexp)      >>= \asmBlockWhileCond -> 
                                      (compileBlock block)    >>= \asmBlockWhileBlock -> 
                                      genNewLabel             >>= \marcaEndWhile ->
                                      return $ [ Mark marcaWhile ] ++ asmBlockWhileCond ++ [ Pop A, JumpIfZ A marcaEndWhile ] ++ asmBlockWhileBlock ++ [ Jump marcaWhile , Mark marcaEndWhile ]

compileComm (Switch nexp cases defaultb) = if hasDuplicateCases cases 
                                           then errState InvalidCase
                                           else compileCommSwitch nexp cases defaultb

hasDuplicateCases cases = hasDuplicateCases' $ map extractNumFromCase cases
hasDuplicateCases' [] = False 
hasDuplicateCases' (x:xs) = x `elem` xs || hasDuplicateCases' xs
extractNumFromCase (Case num _) = num

-- compileNExp

compileNExp :: NExp -> State Memory [Mnemonic]

compileNExp (Vble name) = return [ Read A name , Push A ]

compileNExp (NCte n)    = return [ Load A n, Push A ]

compileNExp (Add n1 n2) = (compileNExp n1) >>= \asmBlockN1 -> 
                          (compileNExp n2) >>= \asmBlockN2 -> 
                          return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop A, Pop B, ADD A B , Push A ]
                                                
compileNExp (Sub n1 n2) = (compileNExp n1) >>= \asmBlockN1 -> 
                          (compileNExp n2) >>= \asmBlockN2 -> 
                          return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop B, Pop A, SUB A B, Push A ]

compileNExp (Mul n1 n2) = (compileNExp n1) >>= \asmBlockN1 -> 
                          (compileNExp n2) >>= \asmBlockN2 -> 
                          return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop A, Pop B, MUL A B , Push A ]

compileNExp (Div n1 n2) = (compileNExp n1) >>= \asmBlockN1 -> 
                          (compileNExp n2) >>= \asmBlockN2 -> 
                          return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop B, Pop A, DIV A B , Push A ]

compileNExp (Mod n1 n2) = (compileNExp n1) >>= \asmBlockN1 -> 
                          (compileNExp n2) >>= \asmBlockN2 -> 
                          return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop B, Pop A, MOD A B , Push A ]


-- compileBExp

compileBExp :: BExp -> State Memory [Mnemonic]

compileBExp (BCte b)        = return [ Load A (delta b) , Push A ]

compileBExp (And b1 b2)     = (compileBExp b1) >>= \asmBlockB1 -> 
                              (compileBExp b2) >>= \asmBlockB2 -> 
                              return $ asmBlockB1 ++ asmBlockB2 ++ [ Pop B, Pop A, MUL A B, Push A ]

compileBExp (Cmp rop n1 n2) = compileBExpCmp rop n1 n2

-- PRECONDICION : delta True == 1
compileBExp (Not b)         = (compileBExp b) >>= \asmBlockB1 ->  
                              return $ asmBlockB1 ++ [ Pop A, Load B 1, ADDmod2 A B, Push A ] 

compileBExp (Or b1 b2)      = (compileBExp b1) >>= \asmBlockB1 -> 
                              (compileBExp b2) >>= \asmBlockB2 -> 
                              return $ asmBlockB1 ++ asmBlockB2 ++ [ Pop A, Pop B, ADD A B, Load B 0, CompGt A B, Push A  ] 

------------
-- SUBTAREAS
------------

compileBExpCmp :: ROp -> NExp -> NExp -> State Memory [Mnemonic]

compileBExpCmp Equal n1 n2          = (compileNExp n1) >>= \asmBlockN1 -> 
                                      (compileNExp n2) >>= \asmBlockN2 -> 
                                      return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop A, Pop B, CompEq A B, Push A ]

compileBExpCmp Greater n1 n2        = (compileNExp n1) >>= \asmBlockN1 -> 
                                      (compileNExp n2) >>= \asmBlockN2 -> 
                                      return $ asmBlockN1 ++ asmBlockN2 ++ [ Pop B, Pop A, CompGt A B, Push A ]
  
compileBExpCmp GreaterEqual n1 n2   = compileBExp (Or (Cmp Equal n1 n2) (Cmp Greater n1 n2))

compileBExpCmp NotEqual n1 n2       = compileBExp (Not (Cmp Equal n1 n2))

compileBExpCmp Less n1 n2           = compileBExp (Cmp Greater n2 n1)

compileBExpCmp LessEqual n1 n2      = compileBExp (Cmp GreaterEqual n2 n1)

compileCommSwitch  nexp [Case n b1] defaultb         = compileComm (If (Cmp Equal nexp (NCte n)) b1 defaultb)
compileCommSwitch  nexp ((Case n b1):cases) defaultb = compileComm (If (Cmp Equal nexp (NCte n)) b1 [] ) >>= \block1 ->
                                                       compileCommSwitch nexp cases defaultb             >>= \nextBlocks ->
                                                       return $ block1 ++ nextBlocks

-- Obtiene el nombre de la etiqueta e incrementa la memoria en 1
genNewLabel :: (State Memory Label)
genNewLabel = updState (+1)  >>= \_ -> 
              getState       >>= \curMem -> 
              return $ show curMem

              
