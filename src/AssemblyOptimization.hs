module AssemblyOptimization (
    optimizeProgram
) where

{-
 -------------------------------------
 -- Programacion Funcional - TPI - UNQ 
 -- TP Integrador - 2do semestre 2014
 --                 Daniel Wyrytowski
 -------------------------------------
 -}

import AssemblyRepresentation

-- Ejercicio 5 (Opcional): Proponer e implementar posibles optimizaciones.


-- optimizeAssProg

optimizeProgram :: AssemblyProgram -> AssemblyProgram
optimizeProgram (AssemblyProgram mns) = AssemblyProgram (optimizeMnemonics mns)


-- optimizeMnemonics: 
--  Algoritmo: 
--  Dada una lista de mnemonicos (mns) busca una primera sublista que matchee con un patron optimizable (isOptimizableCase sublista) 
--  y la optimiza (optimizeCase subliza)
--  Repite esta operacion mientras la lista siga siendo optimizable (isOptimizable mns), o sea, mientras se sigan encontrando 
--  sublistas optimizables 

optimizeMnemonics :: [Mnemonic] -> [Mnemonic]
--optimizeMnemonics  = id
optimizeMnemonics mns =  if isOptimizable mns
                         then optimizeMnemonics $ findAndApplyOptimization mns
                         else mns 
 
isOptimizable :: [Mnemonic] -> Bool
isOptimizable mns | (length mns) < minOptimizableLen =  False -- uns secuencia de menos de 2 mnemónicos no es optimizable
                  | otherwise                        =  case  findFirstOptimizableCase minOptimizableLen mns of 
                                                            Nothing  -> False 
                                                            (Just _) -> True

-- Denota el minimo numero de instrucciones que puede tener una secuencia que sea optimizable. Tiene sentido que sean al menos 2 
minOptimizableLen :: Int
minOptimizableLen = 2 

-- Dada una lista de mnemonicos 'mns' busca una secuencia optimizable de largo minimo 'len' y la devuelve
findFirstOptimizableCase :: Int -> [Mnemonic] -> Maybe ([Mnemonic], Int)
findFirstOptimizableCase len mns = case findFirstOptimizableCaseOfLen len 0 mns of 
                                        Nothing  -> if len >= length mns || len >= maxOptimizableLen
                                                    then Nothing
                                                    else findFirstOptimizableCase (len+1) mns 
                                        (Just x) -> Just x

-- Dada una lista de mnemonicos 'mns' busca una secuencia optimizable de largo 'len' dentro de ella, a partir de la posicion 'pos' 
findFirstOptimizableCaseOfLen :: Int -> Int -> [Mnemonic] -> Maybe ([Mnemonic],Int)
findFirstOptimizableCaseOfLen len pos [] = Nothing
findFirstOptimizableCaseOfLen len pos mns = if len > length mns 
                                            then Nothing
                                            else let taken = (take len mns) 
                                                 in if isOptimizableCase taken 
                                                    then Just (taken, pos)
                                                    else findFirstOptimizableCaseOfLen len (pos+1) (tail mns)

-- Dada una lista de mnemonicos 'mns' busca la primera secuencia optimizable y la reemplaza por la optimizada
-- PRECONDICION : isOptimizable mns
findAndApplyOptimization :: [Mnemonic] -> [Mnemonic]
findAndApplyOptimization mns = case findFirstOptimizableCase minOptimizableLen mns of 
                                (Just (notOptimizedCode, pos)) -> let optimizedCode = optimizeCase notOptimizedCode 
                                                                      before        = take pos mns
                                                                      after         = drop (pos + length notOptimizedCode) mns
                                                                   in before ++ optimizedCode ++ after

-- Denota la el largo maximo que puede tener una cadena optimizable. 
-- PRECONDICION: tiene que ser el largo de la lista mas larga entre los patrones matcheables en isOptimizableCase
maxOptimizableLen :: Int
maxOptimizableLen = 4

-- caso 1
-- [ Push A, Pop A ] --> [] 

-- caso 2
-- [ Load A 1, Push A Load A 1 ] --> [ Load A 1, Push A ]

-- caso 3 
-- [ Load A 3, Push A, Pop B ] --> [ Load B 3 ]

-- caso 4 
-- [ Read A "var", Push A, Pop B ] --> [ Read B "var" ]

-- caso 5
-- [ Push A, Load B 2, Pop A ] --> [ Load B 2 ]

-- caso 6
-- [ Push A, Read B "var" , Pop A ] --> [ Read B "var" ]

-- caso 7
-- [ Read A "a", Push A, Read A "facn", Pop B ] --> [ Read A "facn", Read B "a" ]

-- caso 8
-- [ Store A "numero", Read A "numero" ] --> [ Store A "numero" ]


--  Determina si una secuencia de operaciones ( una lista de mnemónicos ) tiene una optimización posible
isOptimizableCase :: [Mnemonic] -> Bool
isOptimizableCase [ Push r1 , Pop r2 ] = r1 == r2                                           -- caso 1
isOptimizableCase [ Load r1 v1, Push r2,  Load r3 v2 ] = r1 == r2 && r2 == r3 && v1 == v2   -- caso 2
isOptimizableCase [ Load r1 v, Push r2, Pop r3 ] = r1 == r2 && r1 /= r3                     -- caso 3
isOptimizableCase [ Read r1 v, Push r2, Pop r3 ] = r1 == r2 && r1 /= r3                     -- caso 4 
isOptimizableCase [ Push r1, Load r2 v, Pop r3 ] = r1 == r3 && r2 /= r1                     -- caso 5
isOptimizableCase [ Push r1, Read r2 v, Pop r3 ] = r1 == r3 && r2 /= r1                     -- caso 6
isOptimizableCase [ Read r1 v1, Push r2, Read r3 v3, Pop r4 ] = r1 == r2 && r1 == r3 && r1 /= r4  -- caso 7
isOptimizableCase [ Store r1 v1, Read r2 v2 ] = r1 == r2 && v1 == v2                        -- caso 8
isOptimizableCase xs = False

-- Dada una secuencia de operaciones optimizable, denota la secuencia de operaciones equivalente que la optimiza
-- PRECONDICION: para la lista de mnemonicos 'mns' : isOptimizableCase mns
-- (Aclaracion respecto de la precond: la funcion funciona igual si la precondicion no se cumple, pero los resultados
--  no seran los esperados)
optimizeCase :: [Mnemonic] -> [Mnemonic]
optimizeCase [ Push r1 , Pop r2 ] = []                                                      -- caso 1
optimizeCase [ Load r1 v1, Push r2,  Load r3 v2 ] = [ Load r1 v1, Push r1 ]                 -- caso 2
optimizeCase [ Load r1 v, Push r2, Pop r3 ] = [ Load r3 v ]                                 -- caso 3 
optimizeCase [ Read r1 v, Push r2, Pop r3 ] = [ Read r3 v ]                                 -- caso 4 
optimizeCase [ Push r1, Load r2 v, Pop r3 ] = [ Load r2 v ]                                 -- caso 5
optimizeCase [ Push r1, Read r2 v, Pop r3 ] = [ Read r2 v ]                                 -- caso 6
optimizeCase [ Read r1 v1, Push r2, Read r3 v3, Pop r4 ] = [ Read r3 v3, Read r4 v1 ]       -- caso 7 
optimizeCase [ Store r1 v1, Read r2 v2 ] = [ Store r1 v1 ]                                  -- caso 8
