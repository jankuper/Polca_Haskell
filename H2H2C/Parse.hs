{-# LANGUAGE FlexibleInstances #-}

module Parse where

import Types
import Data.List
import Debug.Trace

-- =================================================================================================
-- == Basic definitions ============================================================================
-- =================================================================================================

syntcat x       | x ∈ letters           = P_Idf
                | x ∈ digits            = P_Num
                | x ∈ abstractors       = P_Abstr
                | x ∈ opChars           = P_Op
                | x ∈ separators        = Sep
                | x ∈ delimiters        = Delim
                | x ∈ spaces            = Space
                | otherwise             = error (show x)


-- =================================================================================================
-- == Tokenizer ====================================================================================
-- =================================================================================================

fsa P_Idf       = \s x -> case s of
                           P | x ∈ letters      -> Q
                           Q | x ∈ idfChars     -> Q
                             | otherwise        -> S

fsa P_Num       = \s x -> case s of
                           P | x ∈ digits       -> P
                             | x == '.'         -> Q
                             | otherwise        -> S
                           Q | x ∈ digits       -> R
                             | otherwise        -> X
                           R | x ∈ digits       -> R
                             | otherwise        -> S

fsa P_Op        = \s x -> case s of
                           P | x ∈ opChars      -> P
                             | otherwise        -> S

fsa P_Abstr     = \s x -> case s of
                           P | x ∈ abstractors  -> P
                             | otherwise        -> S

fsa Delim       = \s x -> case s of
                           P | x ∈ delimiters   -> Q
                           Q                    -> S

fsa Sep         = \s x -> case s of
                           P | x ∈ separators   -> Q
                           Q                    -> S

fsa Space       = \s x -> case s of
                           P | x ∈ spaces       -> P
                             | otherwise        -> S

-- =============================================

foldlr fsa c (s,a) [] = [a]

foldlr fsa c (s,a) (x:xs)       | s' == X       = error ("incorrect token: " ++ a)
                                | s' /= S       = foldlr fsa c (s',a') xs
                                | otherwise     = a : foldlr fsa c' (P,"") (x:xs)
                                where
                                  s' = fsa c s x
                                  a' = a ++ [x]
                                  c' = syntcat x

tokenizer (x:xs) = foldlr fsa (syntcat x) (P,"") (x:xs)

lexer str = (cat,str)
        where
          cat | str ∈ booleans  = P_Bln
              | str ∈ resWords  = ResWord
              | otherwise       = syntcat (head str)

remSpaces = filter ((/=Space).fst)

-- =================================================================================================
-- == Grammar ======================================================================================
-- =================================================================================================

gramm nt = case nt of

        P_Expr          -> [[ P_BlnTerm, (*:)[blnOp,P_BlnTerm]                                          ]
                           ,[ P_AppTerm, (*:)[cmpOp,P_AppTerm]                                          ]]

        P_BlnTerm       -> [[ P_RelTerm, (?:)[relOp,P_RelTerm]                                          ]]

        P_RelTerm       -> [[ P_AddTerm, (*:)[addOp,P_AddTerm]                                          ]]

        P_AddTerm       -> [[ P_MulTerm, (*:)[mulOp,P_MulTerm]                                          ]]

        P_MulTerm       -> [[ P_SelTerm, (*:)[selOp,P_SelTerm]                                          ]]

        P_SelTerm       -> [[ P_ExpTerm, (*:)[expOp,P_ExpTerm]                                          ]]

        P_ExpTerm       -> [[ P_AppTerm, (*:)[P_AppTerm]                                                ]]

        P_AppTerm       -> [[ num                                                                       ]
                           ,[ bln                                                                       ]
                           ,[ idf                                                                       ]
                           ,[ empty                                                                     ]

                           ,[ lParenToken, op, rParenToken                                              ]       -- Sectioning
                           ,[ lParenToken, op, P_Expr, rParenToken                                      ]
                           ,[ lParenToken, P_Expr, op, rParenToken                                      ]

                           ,[ lParenToken, P_Expr, comma, P_Expr, rParenToken                           ]       -- Tuples
                           ,[ lParenToken, P_Expr, comma, P_Expr, comma, P_Expr, rParenToken            ]

                           ,[ lSqBracket, rSqBracket                                                    ]       -- Lists
                           ,[ lSqBracket, P_Expr, (*:)[comma,P_Expr], rSqBracket                        ]

                           ,[ lambda, (+:)[P_Pattern], arrow, P_Expr                                    ]       -- Lambda expressions

                           ,[ lett, P_Def, (*:)[semicolon,P_Def], inn, P_Expr                           ]       -- Let expressions

                           ,[ ifE, P_Expr, thenE, P_Expr, elseE, P_Expr                                 ]       -- if-then-else expressions

                           ,[ lParen, P_Expr, rParen                                                    ]]      -- Bracketed expressions

        P_Def           -> [[ P_Pattern, eq, P_Expr, (?:)[tpColon, P_Type]                                    ]]
                           -- ,[ P_Pattern, tpColon, P_Expr                                                ]]      -- Added for Type-defs for Bob Rubbens

        P_Pattern       -> [[ idf                                                                       ]
                           ,[ num                                                                       ]
                           ,[ lParenToken, P_Pattern, comma, P_Pattern, rParenToken                     ]
                           ,[ lParenToken, P_Pattern, comma, P_Pattern, comma, P_Pattern, rParenToken   ]
                           ,[ lSqBracket, rSqBracket                                                    ]
                           ,[ lSqBracket, P_Pattern, (*:)[comma, P_Pattern], rSqBracket                 ]]

        P_BType         -> [[ int                                                                       ]
                           ,[ float                                                                     ]
                           ,[ list, P_Type                                                              ]
                           ,[ array, num, P_Type                                                        ]
                           ,[ lParen, P_Type, rParen                                                    ]
                           ,[ lParen, P_Type, comma, P_Type, rParen                                     ]
                           ,[ lParen, P_Type, comma, P_Type, comma, P_Type, rParen                      ]]

        P_Type          -> [[ P_BType, (*:)[arrow, P_Type]                                              ]]      -- Parentheses to avoid left-recursiveness

lParen          = Symbol "("
rParen          = Symbol ")"
comma           = Symbol ","
semicolon       = Symbol ";"

lambda          = CheckChar (=='\\')
lParenToken     = CheckChar (=='(')
rParenToken     = CheckChar (==')')

colon           = TermSymb ":"

lett            = TermSymb "let"
inn             = TermSymb "in"
ifE             = TermSymb "if"
thenE           = TermSymb "then"
elseE           = TermSymb "else"
eq              = TermSymb "="
tpColon         = TermSymb "::"
empty           = TermSymb "empty"

int             = TermSymb "Int"
float           = TermSymb "Float"
array           = TermSymb "Array"
list            = TermSymb "List"

idf             = SyntCat P_Idf
num             = SyntCat P_Num
bln             = SyntCat P_Bln
op              = SyntCat P_Op

cmpOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ [".",";",";>","<;",";>>","<<;","<<<",">>>","<.",".>"])
blnOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["&&","||"])
relOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["<","<=","==","/=",">=",">"])
addOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["+","-"])
mulOp           = CheckToken (\(nt,s) -> nt==P_Op && s ∈ ["*","/"])
selOp           = CheckToken (\(nt,s) -> nt==P_Op && s == "!")
expOp           = CheckToken (\(nt,s) -> nt==P_Op && s == "^")
arrow           = CheckToken (\(nt,s) -> nt==P_Op && s == "->")
lSqBracket      = CheckToken (\(nt,s) -> nt==Delim && s == "[")
rSqBracket      = CheckToken (\(nt,s) -> nt==Delim && s == "]")


-- =================================================================================================
-- == Parser Generator =============================================================================
-- =================================================================================================

endSkip nt = case nt of
                Opt  _          -> True
                Rep0 _          -> True
                Alt  nts mts    -> all endSkip nts || all endSkip mts
                Try  nts mts    -> all endSkip nts || all endSkip mts
                Rep1 nts        -> all endSkip nts
                _               -> False


-- ==========================================================================================================
-- Parser Generator
-- ----------------
--      NOTE:
--      - Grammar gr is *function*
--      - nt is non-terminal; nt:rule is the rule under consideration
--      - nt0 is the father node
--      - ts is the list of subtrees under nt0 produced so far
--      - tokens here is the list of *indexed* input tokens
--      - lrecCheck is used for checking left-recursiveness of the grammar
-- ==========================================================================================================

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[(Int,Token)])]

parserGen gr []        (nt0,ts,tokens,lrecCheck) = [(PNode nt0 ts, tokens)]

parserGen gr (nt:rule) (nt0,ts,[],lrecCheck)     | endSkip nt    = parserGen gr rule (nt0,ts,[],lrecCheck)
                                                 | otherwise     = [(PError (PNode nt0 ts) (nt:rule) nt "end of input" 0, [])]

parserGen gr (nt:rule) (nt0,ts, allTokens@((k,(cat,str)):remTokens), lrecCheck)

    | nt ∈ lrecCheck        = error ("grammar is left-recursive. Chain: " ++ show (lrecCheck ++ [nt]))
    | otherwise             = case nt of

        -- ============================================================================================================
        -- Backus-Naur constructions

        Alt nts mts     ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,lrecCheck)
                           ++ parserGen gr (mts++rule)                (nt0,ts,allTokens,lrecCheck)

        Opt  nts        ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,lrecCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,lrecCheck)

        Rep0 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,lrecCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,lrecCheck)

        Rep1 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,lrecCheck)

        -- ============================================================================================================
        -- Terminal Symbols

        Symbol str'     | str==str'     -> parserGen gr rule (nt0,ts,remTokens,[])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        TermSymb str'   | str==str'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        SyntCat cat'    | cat==cat'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        CheckToken p    | p (cat,str)   -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        CheckChar p     | p (head str)  -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        -- ============================================================================================================
        -- Non-terminals

        _  ->  concat [ nextParses
                        | r <- gr nt
                        , let parses        = parserGen gr r (nt,[],allTokens, lrecCheck++[nt])
                        , let correctParses = filter (not.isPError.fst) parses

                        , let nextParses | null correctParses = [ (finalPError (nt0,ts) $ maximum $ map fst parses , []) ]

                                         | otherwise          = concat $ map (parserGen gr rule) nextParseStates
                                                              where
                                                                nextParseStates = [ (nt0,ts++[t],remTokens,[])
                                                                                    | (t,remTokens) <- correctParses ]
                      ]

-- ==================================================
-- Additional functions

isPError (PError _ _ _ _ _) = True
isPError _                  = False

finalPError (nt0,ts) (PError t rule nt str k) = PError (PNode nt0 (ts++[t])) rule nt str k

-- ==========================================================================================================
-- Top-level parse function

parseTinadic :: Grammar -> Alphabet -> [Token] -> ParseTree

parseTinadic gr s tokens | null correctParses = maximum $ map fst parses
                         | not $ null rest    = error ("tokenList not fully parsed. Still left: " ++ (show $ map snd rest))
                         | otherwise          = final
          where
            parses = [ (t,rem) | r <- gr s
                               , (t,rem) <- parserGen gr r (s,[],tokens',[])
                               ]

            tokens' = zip [0..] tokens                        -- indexed tokens

            correctParses = filter (not.isPError.fst) parses

            (final,rest)  = head correctParses


parse = parseTinadic gramm P_Expr . remSpaces . map lexer . tokenizer

-- =================================================================================================
-- == ParseTree -> AST =============================================================================
-- =================================================================================================

op2str (PLeaf (P_Op ,str)) = str
op2str (PLeaf (Delim,str)) = str
op2str x                   = error ("up2str:   " ++ show x)

prefixLam es  = PNode P_AppTerm $ PLeaf (P_Abstr,"\\")  : es
prefixLet es  = PNode P_AppTerm $ PLeaf (ResWord,"let") : es

-- ============================

pt2expr (PLeaf (nt,str)) = case nt of
  P_Idf                                                                         -> Idf str
  P_Num                                 | '.' ∈ str                             -> F   (read str)
                                        | otherwise                             -> I   (read str)
  P_Bln                                                                         -> B   (read str)
  _                                                                             -> error (show nt)

pt2expr (PNode nt subtrees) = case nt of

  P_Expr        -> case subtrees of
                [e]                                                             -> pt2expr e

                [f,o,g]                 | op2str o == ".>"                      -> Lambda (Idf "_x_") $ Lambda (Idf "_y_") $ pt2expr g # (pt2expr f # (Idf "_x_")) # (Idf "_y_")

                                        | op2str o == "<."                      -> Lambda (Idf "_x_") $ Lambda (Idf "_y_") $ pt2expr f # (Idf "_x_") # (pt2expr g # (Idf "_y_"))


                _                       | op2str o' ∈ cmpOps                    -> Lambda (Idf "x") $ App (Compose (op2str o')
                                                                                                          (pt2expr $ PNode P_Expr $ init $ init subtrees)
                                                                                                          (pt2expr $ last subtrees)
                                                                                                          ) (Idf "x")

                                        | otherwise                             -> Boolean (op2str o')
                                                                                           (pt2expr $ PNode P_Expr $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_BlnTerm     -> case subtrees of
                [e]                                                             -> pt2expr e
                [e0,o,e1]                                                       -> Boolean (op2str o) (pt2expr e0) (pt2expr e1)
                _                                                               -> error (show nt)

  P_RelTerm    -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_RelTerm $ init $ init subtrees)   -- to get left-associativity
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_AddTerm    -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_AddTerm $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_MulTerm -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> Numeric (op2str o')
                                                                                           (pt2expr $ PNode P_MulTerm $ init $ init subtrees)
                                                                                           (pt2expr $ last subtrees)
                                                                                where
                                                                                  o' = last $ init subtrees

  P_SelTerm -> case subtrees of
                [e]                                                             -> pt2expr e
                _                                                               -> IndSel (pt2expr $ PNode P_SelTerm $ init $ init subtrees)
                                                                                          (pt2expr $ last subtrees)

  P_ExpTerm  -> foldl (#) (pt2expr e) (map pt2expr es)
            where
              (e:es) = subtrees

  P_AppTerm -> case subtrees of
                [e]                                                             -> pt2expr e

                [PLeaf (Delim,"("), PLeaf (P_Op,o),     PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Numeric o (Idf "_u_") (Idf "_v_")
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Boolean o (Idf "_u_") (Idf "_v_")
                        | o ∈ cmpOps                                            -> Lambda (Idf "_u_") $ Lambda (Idf "_v_") $ Compose o (Idf "_u_") (Idf "_v_")
                        | otherwise                                             -> error ("pt2expr: " ++ show subtrees)

                [PLeaf (Delim,"("), e, PLeaf (P_Op,o), PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_v_") $ Numeric o (pt2expr e) (Idf "_v_")
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_v_") $ Boolean o (pt2expr e) (Idf "_v_")
                        | o ∈ cmpOps                                            -> Lambda (Idf "_v_") $ Compose o (pt2expr e) (Idf "_v_")
                        | otherwise                                             -> error ("pt2expr: " ++ show subtrees)

                [PLeaf (Delim,"("), PLeaf (P_Op,o), e, PLeaf (Delim,")")]
                        | o ∈ numOps                                            -> Lambda (Idf "_u_") $ Numeric o (Idf "_u_") (pt2expr e)
                        | o ∈ relOps || o ∈ blnOps                              -> Lambda (Idf "_u_") $ Boolean o (Idf "_u_") (pt2expr e)
                        | o ∈ cmpOps                                            -> Lambda (Idf "_u_") $ Compose o (Idf "_u_") (pt2expr e)
                        | otherwise                                             -> error ("pt2expr: " ++ show subtrees)

                [PLeaf (Delim,"("), e0, e1,             PLeaf (Delim,")")]      -> Pair (pt2expr e0) (pt2expr e1)
                [PLeaf (Delim,"("), e0, e1, e2,         PLeaf (Delim,")")]      -> Triple (pt2expr e0) (pt2expr e1) (pt2expr e2)

                [PLeaf (Delim,"["), PLeaf(Delim,"]")]                           -> Null
                (PLeaf (Delim,"[") : e : es)                                    -> Cons (pt2expr e) $ pt2expr $ PNode P_AppTerm $ PLeaf(Delim,"[") : es

                [PLeaf (P_Abstr,"\\") , PLeaf (P_Op,"->") , body]               -> pt2expr body
                (PLeaf (P_Abstr,"\\") : pattern : es)                           -> Lambda (pt2expr pattern) (pt2expr $ prefixLam es)

                [PLeaf (ResWord,"let") , PLeaf (ResWord,"in") , body]           -> Let [] $ pt2expr body

                (PLeaf (ResWord,"let")
                        : PNode P_Def [pattern, PLeaf (P_Op,"="), e]
                        : es)                                                   -> Let (Def   (pt2expr pattern) (pt2expr e) : defs) e' -- TODO
                            where
                              Let defs e' =  pt2expr $ prefixLet es

                (PLeaf (ResWord,"let")
                        : PNode P_Def [pattern, PLeaf (P_Op,"="), e, PLeaf (P_Op,"::"), tp]
                        : es)                                                   -> Let (Def   (pt2expr pattern) (pt2expr e) : defs) e' -- TODO
                            where
                              Let defs e' =  pt2expr $ prefixLet es





                [PLeaf (ResWord,"if")  , e0,
                 PLeaf (ResWord,"then"), e1,
                 PLeaf (ResWord,"else"), e2]                                    -> IfE (pt2expr e0) (pt2expr e1) (pt2expr e2)

                _                                                               -> error ("pt2expr: " ++ show subtrees)

  P_Pattern -> case subtrees of
                [PLeaf (P_Idf,str)]                                             -> Idf str
                [PLeaf (Delim,"("), e0, e1,     PLeaf (Delim,")")]              -> Pair (pt2expr e0) (pt2expr e1)
                [PLeaf (Delim,"("), e0, e1, e2, PLeaf (Delim,")")]              -> Triple (pt2expr e0) (pt2expr e1) (pt2expr e2)
                [PLeaf (Delim,"["), PLeaf (Delim,"]")]                          -> Null
                (PLeaf (Delim,"[") : e : es)                                    -> Cons (pt2expr e) $ pt2expr $ PNode P_Pattern $ PLeaf(Delim,"[") : es
                _                                                               -> error (show nt)

  _        -> error ("pt2expr -- Found: " ++ show nt ++ " --- Should not occur")

pt2expr nt = error (show nt)

-- =======================================================================================================

{-
pt2type (PNode P_BType ts) = case ts of

        [PLeaf (P_Idf,"Int")]                           -> INT
        [PLeaf (P_Idf,"Float")]                         -> FLOAT
        [PLeaf (P_Idf,"Bool")]                          -> BOOL
        [PLeaf (P_Idf,"List"), tp']                     -> List $ pt2type tp'
        [PLeaf (P_Idf,"Array"), PLeaf (P_Num,n), tp']   -> Array (read n) $ pt2type tp'
        [tp0]                                           -> pt2type tp0
        [tp0,tp1]                                       -> Tuple2 (pt2type tp0) (pt2type tp1)
        [tp0,tp1,tp2]                                   -> Tuple3 (pt2type tp0) (pt2type tp1) (pt2type tp2)
        _                                               -> traceShow ts NoType

pt2type (PNode P_Type [tp])                             = pt2type tp
pt2type (PNode P_Type [tp, PLeaf (P_Op,"->"), tp'])    = Fun (pt2type tp) (pt2type tp')
-}
