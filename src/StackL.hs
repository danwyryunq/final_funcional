module StackL(
    Stack,

    empty,
    isEmpty,
    push,
    top,
    pop
) where

-- Ejercicio 1: Implementar el tipo abstracto Stack.

data Stack a = S [a]


empty :: Stack a
empty = S []


isEmpty :: Stack a -> Bool
isEmpty (S xs) = null xs


push :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)


top :: Stack a -> a
top (S (x:xs)) = x


pop :: Stack a -> Stack a
pop (S (x:xs)) = S xs

