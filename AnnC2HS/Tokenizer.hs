module Tokenizer where

import TypesEtc
import PolcaAlphabet

upLetters    = ['A' .. 'Z']
lowLetters   = ['a' .. 'z']
letters      = upLetters ++ lowLetters
digits       = ['0' .. '9']
idfChars     = letters ++ digits ++ "_"
opChars      = "*/^%+-:!#=><.~&"
separators   = ",;"
brackets     = "(){}[]"
spaces       = " \t\n"

syntcat x    | x ∈ letters      = IdfTkn
             | x ∈ digits       = Nmbr
             | x ∈ opChars      = Op
             | x ∈ separators   = Sep
             | x ∈ brackets     = Bracket
             | x ∈ spaces       = Space
             | otherwise        = error ("syntcat: Unrecognized character: " ++ [x])

-- =================================================================================================
-- Tokenizer using a finite state automaton
-- =================================================================================================

data FsaState = A | B | C
              | S | X | Z
              deriving (Eq,Show)

fsa cat = case cat of

  IdfTkn -> \s x -> case s of
                   A | x ∈ letters      -> B
                   B | x ∈ idfChars     -> B
                     | otherwise        -> S

  Nmbr  -> \s x -> case s of
                   A | x ∈ digits       -> A
                     | x == '.'         -> B
                     | otherwise        -> S
                   B | x ∈ digits       -> C
                     | x == '.'         -> Z            -- ad hoc solution to capture e.g. 1.. in [1..10] (see below)
                     | otherwise        -> X
                   C | x ∈ digits       -> C
                     | otherwise        -> S

  Op    -> \s x -> case s of
                   A | x ∈ opChars      -> A
                     | otherwise        -> S

  Bracket -> \s x -> case s of
                   A | x ∈ brackets     -> B
                   B -> S

  Sep   -> \s x -> case s of
                   A | x ∈ separators   -> B
                   B -> S

  Space -> \s x -> case s of
                   A | x ∈ spaces       -> A
                     | otherwise        -> S

-- =============================================
nexttoken fsa c s t  ""      = (t,"")
nexttoken fsa c s t (x:xs)   | s' == X       = error ("incorrect token: " ++ show t)
                             | s' == Z       = (init t, '.':x:xs)                       -- ad hoc solution to capture e.g. 1.. in [1..10] (see above)
                             | s' /= S       = nexttoken fsa c s' (t++[x]) xs
                             | otherwise     = (t, x:xs)
                        where
                          s' = fsa c s x

splitTokens fsa  ""    = []
splitTokens fsa (x:xs) = t : splitTokens fsa xs'
                where
                  c       = syntcat x
                  (t,xs') = nexttoken fsa c A "" (x:xs)

-- =============================================
-- functions     = [ "sqrt", "avg", "min", "max" ]
hofs          = [ "itn", "itnscan", "map", "zipWith", "foldl", "scanl", "stencil1D", "stenicel2D" ]
helpfncs      = [ "splitEvery", "zip", "transpose" ]
addOps        = [ "+", "-" ]
mulOps        = [ "*", "/", "^", "%" ]
blnOps        = [ "==", "!=", "!", "&&", "||", "<", ">", "<=", ">=" ]
ioWords       = [ "input", "output", "inout" ]
annWords      = [ "pragma", "polca", "func", "def" ]
prgWords      = [ "if","else","for","fundef","return" ]

lexer str = (cat,str)
        where
          cat   -- | str ∈ functions       = Fnc
                -- | str ∈ hofs            = HOF
                -- | str ∈ helpfncs        = HelpFnc
                | str ∈ addOps          = AddOp
                | str ∈ mulOps          = MulOp
                | str ∈ blnOps          = BlnOp
                | str ∈ ioWords         = IOWord
                | str ∈ annWords        = AnnWord
                | str ∈ prgWords        = PrgWord
                | otherwise             = syntcat (head str)

remSpaces = filter ((/=Space).fst)

-- =============================================
tokenize :: (Alphabet->FsaState->Char->FsaState) -> String -> [Token]

tokenize fsa xs = remSpaces $ map lexer $ splitTokens fsa xs

-- =============================================
tokTest = tokenize fsa
                "program   ;;(x56++>55)!)  + \n   5789.123-[[,876.123.456)) -10(((3+5Test25_90"





