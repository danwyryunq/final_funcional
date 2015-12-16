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

module ParserLib (

    -- Parser type
    Parser,
    
    -- Monads
    (<|>),
    failure,
    
    -- Parsers
    item,
    satisfy,
    symbol,
    token,
    many,
    manyBeginWith,
    many1,
    pack,
    parenthesized,
    bracketized,
    braced,
    optionDef,
    option,
    listOf,
    
    lower,
    upper,
    digit,
    letter,
    alphanum,
    word,
    alphanumWord,
    lowerId,
    upperId,
    natural,
    sign,
    integer,
    
    
    (<@),
    op,
    tokenAs,
    chainl1,
    chainr1,

    -- Helper functions
    parse,
    bestParse,
    bestFirst,
) where



{- Parser type -}


type Input = String
newtype Parser a = Parser (Input -> [(a, Input)])



{- Monads -}


class Monad m => MonadFailureOr m where
    failure  ::  m a
    (<|>)    ::  m a -> m a -> m a
    
instance Monad Parser where
    return v         =  Parser (\inp -> [(v, inp)])
    Parser p >>= f   =  Parser (\inp -> 
                          concat [ parse (f v)  out  | 
                                   (v, out) <- p inp ])

instance MonadFailureOr Parser where
    failure                    =  Parser(\inp -> [])
    (Parser p) <|> (Parser q)  =  Parser(\inp -> p inp ++ q inp)
    
    
    
{- Parsers -}


item = Parser (\inp -> case inp of 
                         []     -> []
                         (x:xs) -> [(x, xs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
              c <- item
              if p c then return c else failure

symbol :: Char -> Parser Char
symbol x = satisfy (==x)
    
token :: String -> Parser String
token ""     = return ""
token (c:cs) = do
                 symbol c
                 token cs
                 return (c:cs)

-- Repetition

many p  = (do
             x <- p
             xs <- many p
             return (x:xs)) 
       <|> return []

manyBeginWith p start 
    = do
        x  <- start
        xs <- many p
        return (x:xs)

many1 p = manyBeginWith p p

listOf p sep = do
                 x  <- p
                 xs <- rest p sep
                 return (x:xs)
               where
                 rest p sep
                     = many (ignoreFirst sep p)
                 ignoreFirst sep p 
                     = do
                         _ <- sep
                         x <- p
                         return x


-- Packing

pack open p close = do
                      open
                      x <- p
                      close
                      return x
                      
parenthesized p = pack (symbol '(') p (symbol ')')
bracketized p   = pack (symbol '[') p (symbol ']')
braced p        = pack (symbol '{') p (symbol '}')


-- Option

optionDef p def  =  p
                <|> return def

option p = optionDef (p <@ (:[])) []


-- Strings

lower    = satisfy (between 'a' 'z')
upper    = satisfy (between 'A' 'Z')
letter   = lower <|> upper
digit    = satisfy (between '0' '9')
alphanum = letter <|> digit

between low high = (\x -> low <= x && x <= high)

word = word' <|> return ""
       where
        word' = do
                  x  <- letter
                  xs <- word
                  return (x:xs)

alphanumWord = alphanumWord' <|> return ""
               where
                alphanumWord' = do
                          x  <- alphanum
                          xs <- alphanumWord
                          return (x:xs)
                          
lowerId = manyBeginWith alphanum lower
upperId = manyBeginWith alphanum upper

natural  =  many1 digit
sign     =  symbol '+' 
        <|> symbol '-'
integer  =  do
              s <- optionDef sign '+'
              n <- natural
              return (s:n)
-- Apply

p <@ f = do 
           x <- p
           return (f x)
           

-- Operators and expresion parsing 

op = tokenAs
tokenAs s v       = replaceResult (token s) v
replaceResult p v = p <@ const v

p `chainl1` op = do { x <- p;  rest x }
                 where rest x  = (do
                                    f <- op
                                    y <- p
                                    (rest (f x y))) 
                              <|> return x
                                 
p `chainr1` op = do 
                    x <- p
                    (do 
                        f <- op
                        y <- p `chainr1` op
                        return (f x y)) <|> return x



                



{- Helper functions -}


-- Parsing helpers

parse :: Parser a -> String -> [(a, String)]
parse (Parser x) = x

bestParse :: Parser a -> String -> [(a, String)]
bestParse p = (filter (\(v, inp') -> inp' == "")) . parse p

bestFirst :: Parser a -> String -> (a, String)
bestFirst p = \inp ->
                case bestParse p inp of
                  [] -> error "Parsing error"
                  xs -> head xs

