{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}


module TypesEtc where

import GHC.Generics
import FPPrac.Trees
import PolcaAlphabet
import Data.Char
import Debug.Trace

-- ===================================================================
-- data Alphabet = ... in Alphabet.hs ...

-- ===================================================================
-- Symbolic notation for EBNF constructors
{-
ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps
-}

-- ===================================================================

type Grammar   = Alphabet -> [[Alphabet]]

type Token     = (Alphabet,String)  -- Alphabet: indicates the "syntactic category" to which
                                  --      the String belongs (to distinguish, a.o., between
                                  --      reserved words and identifiers in general),
                                  -- String: the token itself,

data IOtype    = Input | Output | InOut
               deriving (Eq,Show,Generic,ToRoseTree)

data Platform  = MaxJ  | OpenCL | AVX
               deriving (Eq,Show,Generic,ToRoseTree)

data AlgStruct = Monoid
               | Group 
               | Ring
               deriving (Eq,Show,Generic,ToRoseTree)

data Operation = Add | Sub | Mul | Div | Mod | Exp
               | Neg | And | Or | Eql | NEq | Gt | Lt | GEq | LEq
               | Ptr | Sel | Apply | Abstr | NoOp                             -- <<== for priority settings
               deriving (Eq,Show,Generic,ToRoseTree)

data Expr      = Const Int
               | Idf String
               | Empty
               | Ref Expr
               | Deref Expr

               | IfE Expr Expr Expr
               | Tuple [Expr]
               | List [Expr]
               | Field String Expr              -- only meant for Rec-expressions, because of derivability of ToRoseTree
               | Rec [Expr]                     -- i.e., meant here:  Rec [Field nm e, Field nm' e', ...]
               | LstSel Expr Expr
               | RecSel Expr String
               | UpdLst Expr Expr Expr          -- meant for generating executable Haskell code
               | UpdRec Expr String Expr

               | BinInfix Operation Expr Expr
               | App Expr Expr
               | Lambda Expr Expr
               | Def Expr Expr
               | Let [Expr] Expr

               | Brackets Expr
               deriving (Eq,Show,Generic,ToRoseTree)

data AnnStmnt  = FunDef Expr Expr [AnnStmnt]
               | Return Expr
               | Assign Expr Expr                       -- First Expr limited: indexed or (de)referenced variable, 
               | IfS Expr [AnnStmnt] [AnnStmnt]
               | For LoopCntr [AnnStmnt]

               | PolcaFunc String String [Expr] Expr    -- PolcaFunc hof operName args target
               | PolcaDef String
               | PolcaIO String [Expr]                  -- String: "input", "output", "inout"
                                                        --         for "output": ONLY one Expr allowed (possibily a tuple)

               -- | PolcaMath
               -- | PolcaSmath             
               -- | PolcaKernel [Platform]
               -- | PolcaAdapt  [Platform]          -- <<=== Difference?
               -- | PolcaMem ...
               -- | PolcaVarinfo ...
               -- | PolcaHardwareIO ...
               -- | PolcaGuard ...
               deriving (Eq,Show,Generic,ToRoseTree)

data LoopCntr  = LoopCntr Expr Expr Expr Expr                -- in for-loops: LoopCntr var_i n_start n_end n_step
               deriving (Eq,Show,Generic,ToRoseTree)

type Program   = [AnnStmnt]


instance ToRoseTree Token where
  toRoseTree t = RoseNode (show t) []

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

-- ===================================================================
-- Pretty Printing

addSpace n = map ((replicate n ' ') ++)

addListNotation []                 =   [["["]]

addListNotation ([]:strss)         =   ["["]
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addListNotation ((str:strs):strss) =   (("["++str):strs)
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addEndBrack [strs]       = [ strs ++ ["]"] ]
addEndBrack (strs:strss) = strs : addEndBrack strss


-- =============================================================================================================
-- =============================================================================================================
-- =============================================================================================================

operator e = case e of
     Ref e           -> Ptr
     Deref e         -> Ptr
     BinInfix o e e' -> o
     LstSel e i      -> Sel
     RecSel e e'     -> Sel
     UpdLst xs i e   -> Apply
     UpdRec rec nm e -> Apply
     App f e         -> Apply
     Lambda x e      -> Abstr
     Let defs e      -> Abstr
     _               -> NoOp

priority o | o == NoOp                   = 100
           | o == Apply                  =  80
           | o == Ptr                    =  70
           | o == Sel                    =  60
           | o ∈ [Exp]                   =  50
           | o ∈ [Mul,Div,Mod]           =  40
           | o ∈ [Add,Sub]               =  30
           | o ∈ [Eql,NEq,Gt,Lt,GEq,LEq] =  20
           | o ∈ [Neg]                   =  10
           | o ∈ [And]                   =   8
           | o ∈ [Or]                    =   5
           | o == Abstr                  =   2


-- =============================================================================================================
-- =============================================================================================================
-- =============================================================================================================
-- =============================================================================================================

x ∈ xs = x `elem` xs

f#x             = App f x

xs!i            = LstSel xs i

n_oper Add  = (+)
n_oper Mul  = (*)
n_oper Sub  = (-)

tolower c | c ∈ ['A'..'Z'] = chr $ ord c + 32
          | otherwise      = c

concatWith x [] = []
concatWith x (y:ys) = y ++ concat (map (x:) ys)

concatWith2 x [] = []
concatWith2 x (y:ys) = y ++ concat (map (x++) ys)

{-
needBracks expr = case expr of

     Const n         ->
     Idf (c:str)     ->

     Empty           ->
     Ref e           ->
     Deref e         ->

     Tuple es        ->
     List es         ->
     Field str e     ->
     Rec es          ->
     LstSel e i      ->
     RecSel e e'     ->
     UpdLst xs i e   ->
     UpdRec rec nm e ->

     BinInfix o e e' ->

     App f e         ->
     Lambda x e      ->
     Def x e         ->
     Let defs e      ->
-}
