{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module PolcaAlphabet where

import GHC.Generics
import FPPrac.Trees

-- ===================================================================
-- Symbolic notation for EBNF constructors

ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps


data Alphabet = TermSymb String               -- Terminal symbol
              | SyntCat  Alphabet             -- Syntactic category to which a terminal belongs

              | Alt   [Alphabet] [Alphabet]   -- Try both
              | Opt   [Alphabet]              -- Optional
              | Rep0  [Alphabet]              -- Zero or more repetitions
              | Rep1  [Alphabet]              -- One or more repetitions

              | None                          -- For debug purposes

              | IOWord
              | AnnWord
              | PrgWord

              | IdfTkn
              | Nmbr                          -- Number
              | MulOp                         -- Multiplicative operation
              | AddOp                         -- Additive operation
              | BlnOp                         -- Boolean operation
              | Op                            -- Any operation
              | ResWord                       -- Reserved word   (for Tokenizer)
              | Sep                           -- Separator       (for Tokenizer)
              | Space                         -- Spaces          (for Tokenizer)
              | Bracket                       -- Brackets        (for Tokenizer)

              | P_Func_ann
              | P_Polca_ann
              | P_Def_ann
              | P_IO_ann

              | P_Expr
              | P_Term
              | P_Factor
              | P_Bln_Expr

              | P_Guard_ann

              | P_Guard_data
              | P_Guard_list
              | P_Guard_param
              | P_Guard_cond_list
              | P_Guard_stmnt_list
              | P_Guard_stmnt

              | P_Var0
              | P_Var
              | P_Number

              | P_Program
              | P_AnnStmnt
              | P_ITE_Test
              | P_LoopCntr
              | P_ParList
              | P_Body

              -- ============================================
              -- Below: removed
              -- | Func_parameter
              -- | Func_in_data
              -- | Func_out_data
              -- | Func_helper
              -- | Func
              -- | Fnc                           -- "Normal" function
              -- | HelpFnc                       -- splitEvery, zip, transpose, etc

              -- ============================================
              -- Below: only in Algebraic Structures (Ring)     -- Left out for now.
              -- | Add_op
              -- | Add_neutral
              -- | Add_inverse
              -- | Mult_op
              -- | Mult_neutral
              -- | Data_type
              -- | Mem_free

              -- ============================================
              -- Below: non-terminals, replaced by jk.
              -- | Polca_var_id         -- only idf
              -- | C_var                -- replaced by Var
              -- | C_number             -- replaced by Number

              -- ============================================
              -- Below: various non-terminals removed
              -- | Smath
              -- | Expr_list
              -- | C_expr
              -- | C_array
              -- | IO_var_list
              -- | C_location
              -- | HardwareIO_ann
              -- | Kernel_ann
              -- | Platform_list
              -- | Platform
              -- | Adapt_ann
              -- | Adapt_platform_list
              -- | Adapt_platform
              -- | Zip
              -- | Math_ann
              -- | Ring_ann
              -- | Mem_ann
              -- | Mem_parameter
              -- | Varinfo_ann
              -- | Type_size
              -- | Total_size

              -- | Unary_negation
              -- | Logical_and_expr
              -- | Logical_or_expr
              -- | Relational_expr_lt
              -- | Relational_expr_gt
              -- | Relational_expr_let
              -- | Relational_expr_get
              -- | Equality_expr
              -- | Inequality_expr

              deriving (Eq,Ord,Show,Generic,ToRoseTree)

{-
Uncovered non-terminals

HardwareIO_ann
INT, FLOAT
C_expr
Add_op, Add_neutral, Add_inverse, Mult_op, Mult_neutral
Data_type
Mem_free
-}

idf          = SyntCat  IdfTkn
nmbr         = SyntCat  Nmbr            -- INT                  <<=== TODO
float        = SyntCat  Nmbr            -- FLOAT

addOp        = SyntCat  AddOp
mulOp        = SyntCat  MulOp
blnOp        = SyntCat  BlnOp
otherOp      = SyntCat  Op

-- function     = SyntCat  Fnc
-- hof          = SyntCat  HOF

ioWord       = SyntCat  IOWord
annWord      = SyntCat  AnnWord
prgWord      = SyntCat  PrgWord

hash         = TermSymb "#"
pragma       = TermSymb "pragma"
polca        = TermSymb "polca"

def          = TermSymb "def"
kernel       = TermSymb "kernel"
maxj         = TermSymb "maxj"
opencl       = TermSymb "opencl"
avx          = TermSymb "avx"
x86_64       = TermSymb "x86-64"
arm_v8       = TermSymb "ARM-v8"
adapt        = TermSymb "adapt"
mpi          = TermSymb "mpi"
openmp       = TermSymb "openmp"
func         = TermSymb "func"
itnFnc       = TermSymb "itn"
mapFnc       = TermSymb "map"
zipWithFnc   = TermSymb "zipWith"
foldlFnc     = TermSymb "foldl"
scanlFnc     = TermSymb "scanl"
stencil1DFnc = TermSymb "stencil1D"
stencil2DFnc = TermSymb "stencil2D"
zipFnc       = TermSymb "zip"
splitEveryFnc= TermSymb "splitEvery"
transposeFnc = TermSymb "transposeFnc"
ring         = TermSymb "ring"
varinfo      = TermSymb "varinfo"
alloc        = TermSymb "alloc"
calloc       = TermSymb "calloc"
realloc      = TermSymb "realloc"
copy         = TermSymb "copy"
io           = TermSymb "io"
guard        = TermSymb "guard"
smath        = TermSymb "smath"
for          = TermSymb "for"
if_kw        = TermSymb "if"    -- for "if-keyword"
else_kw      = TermSymb "else"  -- for "else-keyword"
fundef       = TermSymb "fundef"
rtrn         = TermSymb "return"

negOp        = TermSymb "!"     -- needed?
refOp        = TermSymb "*"     -- needed?
derefOp      = TermSymb "&"     -- needed?
plusplus     = TermSymb "++"
eqSign       = TermSymb "="

lBrack       = TermSymb "("
rBrack       = TermSymb ")"
lCurly       = TermSymb "{"
rCurly       = TermSymb "}"
lSqBrack     = TermSymb "["
rSqBrack     = TermSymb "]"
comma        = TermSymb ","
semicolon    = TermSymb ";"
colon        = TermSymb ":"
underscore   = TermSymb "_"
dotdot       = TermSymb ".."
dot          = TermSymb "."


