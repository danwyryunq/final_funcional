{-
    Author: Ary Pablo Batista <arypbatista@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module LISParser (
    lisParser,
    lisLexer
) where

import Data.Char (ord)
import LISRepresentation
import ParserLib

-- skip

skip = token "skip" <@ const Skip


-- assign

assign = do
           v <- varname
           _ <- token ":="
           e <- nExpr
           return (Assign v e)


-- Variable name

varname = lowerId


-- if

iff = do
        token "if"
        bexp      <- parenthesized bExpr
        blockThen <- block
        blockElse <- optionDef iffElse []
        return (If bexp blockThen blockElse)
        
iffElse = do
            token "else"
            bf <- block
            return bf


-- while

while = do
            token "while"
            bexp <- parenthesized bExpr
            blk  <- block
            return (While bexp blk)


-- switch

switch = do
            token "switch"
            nexp <- parenthesized nExpr
            cs   <- cases
            token "default"
            def  <- block
            return (Switch nexp cs def)


-- Block
                                
block = braced commands

commands = do
             cs <- listOf command (option (token ";"))
             option (token ";")
             return cs

-- Case
cases = do
             cs <- listOf cases' (option (token ""))
             return cs

cases' = do
             token "case"
             n   <- integer <@ strToInt
             blk <- block
             return (Case n blk)

-- Command

command  =  skip
        <|> assign
        <|> iff
        <|> switch
        <|> while


-- Boolean expressions

bExpr    =  bAndTerm `chainl1` orop
bAndTerm =  bTerm `chainl1` andop
bTerm    =  do
              f   <- optionDef notop id
              b   <- bConstant
              return (f b)
        <|> nCmp
      
andop    =  op "&&" And 
orop     =  op "||" Or
notop    =  op "!"  Not

nCmp   = do
           n1 <- nExpr
           r  <- relop
           n2 <- nExpr
           return (Cmp r n1 n2)
          
relop  =  op ">"  Greater      
      <|> op ">=" GreaterEqual 
      <|> op "<"  Less
      <|> op "<=" LessEqual
      <|> op "==" Equal 
      <|> op "!=" NotEqual
          
bConstant  =  bTrue 
          <|> bFalse
bTrue      = tokenAs "True"  (BCte True )
bFalse     = tokenAs "False" (BCte False)


-- Numeric expressions

nExpr   =  term `chainl1` addop
term    =  factor `chainr1` mulop
factor  =  nConstant
       <|> variable 
       <|> parenthesized nExpr

addop   =  op "+" Add
       <|> op "-" Sub
mulop   =  op "*" Mul 
       <|> op "/" Div 
       <|> op "%" Mod

variable = varname <@ Vble

nConstant = integer <@ (NCte . strToInt)

strToInt (c:cs) = applySign c (toNat cs)
                  where
                    applySign s = case s of
                                    '+' -> (*1)
                                    '-' -> (* (-1))
                    toNat cs
                      = foldl (\a x -> a*10 + toDigit x) 0 cs
                    toDigit x = ord x - ord '0'


-- Program parser

lisParser = do 
              token "program"
              blk <- block
              return (Program blk)
              
              
-- Lexer

lisLexer = removeAllWhites . removeAllComments

removeAllWhites []  = []
removeAllWhites inp = takeWhile (not . isWhitespace) inp ++
                      (lisLexer . removeWhites) rest
                    where
                      rest = dropWhile (not . isWhitespace) inp

removeWhites = dropWhile isWhitespace

removeAllComments []            = []
removeAllComments ('-':'-':inp) = let inp2 = dropWhile (not . isLineBreak) inp
                                   in removeAllComments inp2
removeAllComments (c:inp)       = c : removeAllComments inp

isWhitespace = (flip elem) ['\n', '\t', ' ']
isLineBreak = (=='\n')

