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

module LISRepresentation (
    Program (Program),
    
    Block,
    
    Command (
        Skip,
        Assign,
        If,
        Switch,
        While
    ),

    Case (Case),
    
    BExp (
        BCte,
        And,
        Cmp,
        Not,
        Or
    ),
    
    ROp (
        Equal,
        NotEqual,
        Greater,
        GreaterEqual,
        Less,
        LessEqual
    ),
    
    --VarName,
    
    NExp (
        Vble,
        NCte,
        Add,
        Sub,
        Mul,
        Div,
        Mod
    ),
) where

import Pretty


-- LIS Representation

data Program = Program Block

type Block   = [Command]

data Command = Skip
             | Assign VarName NExp
             | If BExp Block Block
             | Switch NExp [Case] Block
             | While BExp Block
             
data Case = Case Int Block

data BExp    = BCte Bool
             | And BExp BExp
             | Cmp ROp NExp NExp
             | Not BExp
             | Or BExp BExp
    
data ROp     = Equal
             | Greater
             | GreaterEqual
             | NotEqual
             | Less
             | LessEqual
         
type VarName = String

data NExp    = Vble VarName
             | NCte Int
             | Add NExp NExp
             | Sub NExp NExp
             | Mul NExp NExp
             | Div NExp NExp
             | Mod NExp NExp
          

-- LIS Show

instance Show Program where
  show (Program cs) = "LIS Program: \n" ++ (showBlock cs)

showBlock cs = fullRender PageMode 40 2.5 showText "" (ppBlock cs)

ppBlock cs = sep ([ text "{" ] 
              ++     punctuate (text ";") (map (nest 2 . ppComm) cs)
              ++ [ text "}" ])

ppComm Skip               = text "skip"
ppComm (Assign x ne)      = text x <+> text ":=" <+> ppNExp ne
ppComm (If be p1 p2)      = sep [ text "if (" <> ppBExp be <> text ")"
                                , nest 2 (text "then" <+> ppBlock p1)
                                , nest 2 (text "else" <+> ppBlock p2)
                                ]
ppComm (While be p)       = sep [ text "while (" <> ppBExp be <> text ") do"
                                , nest 2 (ppBlock p)
                                ]
ppComm (Switch ne cs def) = sep [ text "switch (" <> ppNExp ne <> text ")"
                                , nest 2 (ppCases cs)
                                , nest 2 (text "default" <+> ppBlock def)
                                ]

ppCases = sep . (foldr ((:).ppCase) [])

ppCase (Case n blk) = sep [ text "case " <> ppAtom (NCte n) <+> ppBlock blk ]

ppNExp e = ppSumm e

ppSumm (Add e1 e2) = ppSumm e1 <+> text "+" <+> ppSumm e2
ppSumm (Sub e1 e2) = ppSumm e1 <+> text "-" <+> ppSumm e2
ppSumm e = ppFact e

ppFact (Mul e1 e2) = ppFact e1 <+> text "*" <+> ppFact e2
ppFact (Div e1 e2) = ppFact e1 <+> text "/" <+> ppFact e2
ppFact (Mod e1 e2) = ppFact e1 <+> text "%" <+> ppFact e2
ppFact e = ppAtom e

ppAtom (NCte n) = text (show n)
ppAtom (Vble x) = text x
ppAtom e = parens (ppNExp e)

ppBExp e = ppDisy e

ppDisy (Or e1 e2) = ppDisy e1 <+> text "||" <+> ppDisy e2
ppDisy e = ppConj e

ppConj (And e1 e2) = ppConj e1 <+> text "&&" <+> ppConj e2
ppConj e = ppNot e

ppNot (Not e) = text "!" <+> ppBAtom e
ppNot e = ppBAtom e

ppBAtom (BCte b) = text (show b)
ppBAtom (Cmp rop e1 e2) = ppNExp e1 <+> ppOp rop <+> ppNExp e2
ppBAtom e = parens (ppBExp e)

ppOp Equal        = text "=="
ppOp NotEqual     = text "!="
ppOp Greater      = text ">"
ppOp GreaterEqual = text ">="
ppOp Less         = text "<"
ppOp LessEqual    = text "<="

showText (Chr c) s    = c:s
showText (Str s1) s2  = s1 ++ s2
showText (PStr s1) s2 = s1 ++ s2

