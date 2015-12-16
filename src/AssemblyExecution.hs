module AssemblyExecution (
    execProgram
) where

import AssemblyRepresentation
import Data.Maybe
import qualified Data.Map as Map
import qualified StackL as Stack


-- MVS Representation

type Registers = (Int, Int)
data MVS = MVS { ip :: Int
               , regs :: Registers
               , stack :: (Stack.Stack Int)
               , instmem :: (Map.Map Int Mnemonic)
               , datamem :: (Map.Map VarName Int) }

instance Show MVS where
    show mvs = "MVS Status: \n" ++
        "IP: "        ++ show (ip mvs)          ++ "\n" ++
        "Regs: \n"    ++ showRegs (regs mvs)    ++ "\n" ++
        "Stack: \n  " ++ showStack (stack mvs)  ++ "\n" ++
        "Prog: \n"    ++ showIMem (instmem mvs) ++
        "Data: \n"    ++ showDMem (datamem mvs)

showRegs regs = "  A: " ++ show (readReg A regs) ++ "\n" ++ "  B: " ++ show (readReg B regs)

showStack stk = if Stack.isEmpty stk then "[]" else show (Stack.top stk) ++ " ; " ++ showStack (Stack.pop stk)

showIMem imem = foldr (\(key, mn) xs -> "  " ++ show key ++ ": " ++ show mn ++ "\n" ++ xs) [] (Map.toList imem)

showDMem dmem = foldr (\(var, val) xs -> "  " ++ var ++ ": " ++ show val ++ "\n" ++ xs) [] (Map.toList dmem)


-- execProgram

execProgram :: AssemblyProgram -> MVS
execProgram (AssemblyProgram mns) = execMVS (initialMVS mns)


-- initial MVS

initialMVS mns = MVS initialIP initialRegs initialStk (loadMns initialIP mns) initialData
initialIP   = 0
initialRegs = (0, 0)
initialStk  = Stack.empty
initialData = Map.empty

loadMns k mns = foldr (uncurry Map.insert) Map.empty (addKeys k mns)

addKeys _ []     = []
addKeys k (x:xs) = (k, x):addKeys (k+1) xs


-- execMVS

execMVS :: MVS -> MVS
execMVS mvs = if ip mvs > fst (Map.findMax (instmem mvs))
               then mvs
               else case (Map.lookup (ip mvs) (instmem mvs)) of
                 Nothing -> error "This should not happend!"
                 Just mn -> execMVS (execMnemonics mn (increaseIP mvs))

increaseIP (MVS ip regs stk imem dmem) = MVS (ip+1) regs stk imem dmem


-- execMnemonics

execMnemonics NoOp            mvs                         = mvs
execMnemonics (Mark _)        mvs                         = mvs
execMnemonics (Load     r  n) (MVS ip regs stk imem dmem) =
   MVS ip (loadReg r n regs) stk imem dmem
execMnemonics (Read     r  x) (MVS ip regs stk imem dmem) = 
   case (Map.lookup x dmem) of
     Nothing -> error "Variable not found!"
     Just v  -> MVS ip (loadReg r v regs) stk imem dmem
execMnemonics (Store    r  x) (MVS ip regs stk imem dmem) = 
   let v = readReg r regs
    in MVS ip regs stk imem (Map.insert x v dmem)
execMnemonics (ADD     r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (v1+v2) regs) stk imem dmem
execMnemonics (ADDmod2 r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 ((v1+v2) `mod` 2) regs) stk imem dmem
execMnemonics (MUL     r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (v1*v2) regs) stk imem dmem
execMnemonics (SUB     r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (v1-v2) regs) stk imem dmem
execMnemonics (DIV     r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (v1 `div` v2) regs) stk imem dmem
execMnemonics (MOD     r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (v1 `mod` v2) regs) stk imem dmem
execMnemonics (CompEq  r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (delta (v1==v2)) regs) stk imem dmem
execMnemonics (CompGt  r1 r2) (MVS ip regs stk imem dmem) = 
   let v1 = readReg r1 regs
       v2 = readReg r2 regs
    in MVS ip (loadReg r1 (delta (v1>v2)) regs) stk imem dmem
execMnemonics (Push     r)    (MVS ip regs stk imem dmem) =
   let v = readReg r regs
    in MVS ip regs (Stack.push v stk) imem dmem
execMnemonics (Pop      r)    (MVS ip regs stk imem dmem) =
   if Stack.isEmpty stk
    then error "Pop in empty stack!"
    else let v = Stack.top stk
          in MVS ip (loadReg r v regs) (Stack.pop stk) imem dmem
execMnemonics (Jump     l)    (MVS ip regs stk imem dmem) = 
   let lip = findLabel l (Map.assocs imem)
    in MVS lip regs stk imem dmem
execMnemonics (JumpIfZ  r  l) (MVS ip regs stk imem dmem) = 
   let lip = findLabel l (Map.assocs imem)
       v = readReg r regs
    in if (v == 0) then (MVS lip regs stk imem dmem)
                   else (MVS  ip regs stk imem dmem)


-- Auxiliary functions

readReg A = fst
readReg B = snd

loadReg A v (_, b) = (v, b)
loadReg B v (a, _) = (a, v)

findLabel l = foldl (\j (i, mn) -> case mn of
                      Mark l' -> if (l == l') then i else j
                      mn'     -> j)
                    (error "Undefined label!")

