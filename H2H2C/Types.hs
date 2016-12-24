{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Types where

import FPPrac.Trees
import GHC.Generics

import Data.List
import Data.Maybe
import Debug.Trace

-- =================================================================================================
-- == Types ========================================================================================
-- =================================================================================================

type Grammar      = Alphabet -> [[Alphabet]]

type Token        = (Alphabet,String)     -- Alphabet: indicates the "syntactic category" to which
                                          --      the String belongs (to distinguish, a.o., between
                                          --      reserved words and identifiers in general),
                                          -- String: the token itself,

instance ToRoseTree Token where
        toRoseTree (nt,str) = RoseNode ("("++ show nt ++","++ str++")") []

type Tokengrammar = Alphabet -> [Alphabet]

data FsaState     = P | Q | R
                  | S | X                         -- Stop state, Error state
                  deriving (Eq,Show)

data ParseTree  = PLeaf Token
                | PNode Alphabet [ParseTree]
                | PError ParseTree [Alphabet] Alphabet String Int
                deriving (Eq,Show,Generic,ToRoseTree)

instance Ord ParseTree where
  PError _ _ _ _ k <  PError _ _ _ _ k' = k <  k'
  _                <  _                 = error "ordering only in case of parse-errors"

  PError _ _ _ _ k <= PError _ _ _ _ k' = k <= k'
  _                <= _                 = error "ordering only in case of parse-errors"


type ParseState = ( Alphabet       -- Non-terminal indicating the present subexpression
                  , [ParseTree]    -- The already produced trees within the present subexpression
                  , [(Int,Token)]  -- The remaining list of *indexed* input tokens
                  , [Alphabet]     -- List of non-terminals to check for left-recursiveness
                  )


data Alphabet     = Symbol     String             -- Token given ("char" specific for this example)
                  | TermSymb   String             -- A given string, but included in the parsetree
                  | SyntCat    Alphabet           -- A given string, but included in the parsetree
                  | CheckChar  (Char->Bool)       -- Character should have some property (for tokenizer)
                  | CheckToken (Token->Bool)      -- Token should have some property (for parser)

                  | Alt   [Alphabet] [Alphabet]   -- Try both
                  | Try   [Alphabet] [Alphabet]   -- If first doesn't work, try second
                  | Opt   [Alphabet]              -- Optional
                  | Rep0  [Alphabet]              -- Zero or more repetitions
                  | Rep1  [Alphabet]              -- One or more repetitions

                  | P_Idf                         -- Identifier
                  | P_Expr                        -- Expression
                  | P_BlnTerm                     -- Boolean term
                  | P_RelTerm                     -- Relational term
                  | P_AddTerm                     -- Additive term
                  | P_MulTerm                     -- Multiplicative term
                  | P_SelTerm                     -- Selection term
                  | P_ExpTerm                     -- Exponent term
                  | P_AppTerm                     -- Applicative term
                  | P_Def                         -- Let-definition
                  | P_BType                       -- Types
                  | P_Type                        -- Types
                  | P_Num                         -- Number
                  | P_Bln                         -- Boolean
                  | P_Op                          -- Operator
                  | P_Abstr                       -- Abstraction (lambda)
                  | P_Pattern                     -- Formal parameters in lambda expressions

                  | ResWord                       -- Reserved Word
                  | Sep                           -- Separator (comma, semicolon)
                  | Delim                         -- Delimiter (brackets)
                  | Space                         -- Spaces (spacebar,tab, newline)
                deriving (Eq,Show,Generic,ToRoseTree)

ps <>  qs       = Alt  ps qs
ps <<> qs       = Try  ps qs
(?:) ps         = Opt  ps
(*:) ps         = Rep0 ps
(+:) ps         = Rep1 ps


instance Eq (Char->Bool)        where f == g = True
instance Eq (Token->Bool)       where f == g = True
instance Eq (String->Bool)      where f == g = True

instance Show (Char->Bool)      where show f = "Char->Bool"
instance Show (Token->Bool)     where show f = "Token->Bool"
instance Show (String->Bool)    where show f = "String->Bool"

instance ToRoseTree (Char->Bool)      where toRoseTree f = RoseNode "f" []
instance ToRoseTree (Token->Bool)     where toRoseTree f = RoseNode "f" []
instance ToRoseTree (String->Bool)    where toRoseTree f = RoseNode "f" []

data Operation = Add | Sub | Mul                        -- REMOVE AGAIN; ONLY FOR REVIEW
               | And | Or | Eql | Gt                    -- IBID
               deriving (Eq,Show,Generic,ToRoseTree)


data Expr  = Idf String                         --

           | I   Int
           | F   Float
           | B   Bool

           | Numeric String Expr Expr
           | Boolean String Expr Expr
           | Compose String Expr Expr

           | Pair    Expr   Expr
           | Triple  Expr   Expr Expr

           | Null
           | Cons    Expr   Expr

           | IndSel  Expr   Expr
           | FldSel  Expr   Expr

           | IfE     Expr   Expr Expr

           | App     Expr   Expr

           | Def     Expr   Expr
           -- | Def     Expr   Expr Type      -- Only pattern as defined expr
           | Let     [Expr] Expr           -- Only Def-expressions in list
           | Lambda  Expr   Expr           -- Only pattern as formal parameter




           | Empty

           | Func    [Expr] [Stmnt] Expr
           
           | Const Int                                  -- REMOVE AGAIN; ONLY FOR REVIEW
           | List [Expr]                                -- IBID
           | LstSel Expr Expr                           -- IBID
           | BinInfix Operation Expr Expr               -- IBID
           deriving (Eq,Show,Generic,ToRoseTree)

{-
data Type  = NoType                        -- For cases where type is not relevant ... OR STILL HAS TO BE WORKED OUT
           | TVar String
           | INT
           | FLOAT
           | BOOL
           | Tuple2 Type Type
           | Tuple3 Type Type Type
           | List   Type
           | Array  Int Type
           | Fun    Type Type                                   deriving (Eq,Show,Generic,ToRoseTree)
           -- | PMFun  Type Type
-}

data Stmnt = Skip                               -- Do Nothing
           | Break                              -- Jump out of the loop
           | Assign  Expr Expr
           | IfS     Expr [Stmnt] [Stmnt]
           | For     Expr (Int,String,Int) [Stmnt]
           | While   Expr [Stmnt]                            deriving (Eq,Show,Generic,ToRoseTree)


instance ToRoseTree (Int,String,Int) where
        toRoseTree (i,str,n) = RoseNode ("("++ show i ++","++ str ++","++ show n ++")") []

data Colour = Red | Green | Blue | Yellow | Black | White    deriving (Show, Eq)

type Node = (String,(Float,Float))
type Edge = (String,String)     -- (Graph,Graph)

data Graph = Graph  { nodes     :: [Node]
                    , edges     :: [Edge]
                    , outs      :: [String]
                    }
                    deriving Show

class Subst a where
        (<<=)     ::  a  -> (Expr,Expr) -> a

        (<<=*)    ::  a  -> [(Expr,Expr)] -> a
        (<<=*)    = foldl (<<=)



class PrPr a where
        toString  ::  a  -> String
        toStrings ::  a  -> [String]
        prpr      ::  a  -> IO()

{-
instance Ord Type where

  TVar _       < _                = True
  INT          < FLOAT            = True

  Tuple2 a b   < Tuple2 a' b'     = a < a' && b < b'
  Tuple3 a b c < Tuple3 a' b' c'  = a < a' && b < b' && c< c'

  List a       < List b           = a < b
  Array m a    < Array n b        = m == n && a < b

  Fun a b      < Fun a' b'        = a' < a && b < b'

  _            < _                = False


  a <= b = a < b || a == b
-}

-- =================================================================================================

x ∈ xs          = x `elem` xs

subset xs ys    = and $ map (∈ ys) xs

xs!i            = IndSel xs i

f#x             = App f x

upLetters       = ['A' .. 'Z']
lowLetters      = ['a' .. 'z']
letters         = upLetters ++ lowLetters
digits          = ['0' .. '9']
idfChars        = letters ++ digits ++ "_'"
abstractors     = "\\"
opChars         = "+*-/%^:!#=><.~&|;"
separators      = ","
delimiters      = "(){}[]"
spaces          = " \t\n"

resWords        = [ "let", "in", "if", "then", "else", "Skip" ]
booleans        = [ "True", "False" ]

numOps          = ["+","-","*","/","^"]
relOps          = [">",">=","==","/=","<=","<"]
blnOps          = ["&&","||","##","=>"]         -- ## for exclusive or
cmpOps          = [".",";",";>","<;",";>>","<<;","<<<",">>>","<.",".>"]

-- =====================================

n_oper "+"  = (+)
n_oper "*"  = (*)
n_oper "-"  = (-)
-- n_oper "^"  = (^)
-- division defined locally
-- add %, /, //

r_oper ">"  = (>)
r_oper ">=" = (>=)
r_oper "==" = (==)
r_oper "/=" = (/=)
r_oper "<=" = (<=)
r_oper "<"  = (<)

b_oper "&&" = (&&)
b_oper "||" = (||)

-- =====================================


opr e = case e of
                Idf _           -> "@"
                I _             -> "@"
                F _             -> "@"
                App _ _         -> "@"
                Numeric o _ _   -> o
                Boolean o _ _   -> o
                IndSel _ _      -> "!"
                _               -> " "

prio o = case o of
                " "  -> 0
                "&&" -> 1
                "||" -> 1
                "+"  -> 2
                "-"  -> 2
                "*"  -> 3
                "/"  -> 3
                "!"  -> 4
                "^"  -> 5
                "@"  -> 6


fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

split n [] = []
split n xs = as : split n bs
        where
          (as,bs) = splitAt n xs

concatWith x []         = []
concatWith x [xs]       = xs
concatWith x (xs:xss)   = xs ++ x ++ concatWith x xss



