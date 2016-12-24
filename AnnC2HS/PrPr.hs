{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}


module PrPr where

import GHC.Generics
import FPPrac.Trees
import TypesEtc
import PolcaAlphabet
import Data.Char
import Debug.Trace


-- =============================================================================================================
-- =============================================================================================================
data Language = HS | C | Any
              deriving (Eq,Show,Generic,ToRoseTree)

class Show a => ToString a where
        toString :: Language -> a -> String

instance ToString Operation where
        toString _ o = case o of
                        Add     -> "+"
                        Sub     -> "-"
                        Mul     -> "*"
                        Div     -> "/"
                        Mod     -> "%"
                        Exp     -> "^"
                        Neg     -> "~"
                        And     -> "&&"
                        Or      -> "||"
                        Eql     -> "=="
                        NEq     -> "!="
                        Gt      -> ">"
                        Lt      -> "<"
                        GEq     -> ">="
                        LEq     -> "<="

instance ToString Expr where
        toString lng expr = case expr of
                Const   n        -> show n
                Idf (c:str)      -> tolower c : str
                Ref e            -> "*" ++ toString lng e
                Deref e          -> "&" ++ toString lng e
                BinInfix o e e'  -> toString lng e ++ " " ++ toString lng o ++ " " ++ toString lng e'

                Tuple (e:es)    -> "(" ++ toString lng e ++ concat [ "," ++ toString lng e' | e' <- es ] ++ ")"
                List  []        -> "[]"
                List  (e:es)    -> "[" ++ toString lng e ++ concat [ "," ++ toString lng e' | e' <- es ] ++ "]"
                Field nm e      -> nm ++ " = " ++ toString lng e
                Rec flds        -> "Rec { " ++ (concatWith2 ", " $ map (toString lng) flds) ++ " }"

                LstSel xs i     |  lng == C        -> toString lng xs ++ "[" ++ toString lng i ++ "]"
                                |  lng == HS       -> toString lng xs ++ "!!" ++ toString lng i
                                |  otherwise       -> error ("toString: strange LstSel argument: " ++ toString lng expr)
                RecSel e e'     |  lng == C        -> toString lng e ++ "." ++ e'
                                |  lng == HS       -> e' ++ " " ++ toString lng e
                                |  otherwise       -> error ("toString: strange RecSel argument: " ++ toString lng expr)

                UpdLst xs i e   |  lng == HS       -> toString lng xs  ++ " <~ (" ++ toString lng i ++ ", " ++ toString lng e ++ ")"
                                |  lng == C        -> toString lng xs ++ "[" ++ toString lng i ++ "] = " ++ toString lng e
                                |  otherwise       -> error ("toString: strange UpdLst argument: " ++ toString lng expr)

                UpdRec rec nm e |  lng == HS       -> toString lng rec ++ " { " ++ nm ++ " = " ++ toString lng e ++ " }"
                                |  lng == C        -> toString lng rec ++ "." ++ nm ++ " = " ++ toString lng e
                                |  otherwise       -> error ("toString: strange UpdRec argument: " ++ toString lng expr)

                Lambda x e      -> "\\" ++ toString lng x ++ " -> " ++ toString lng e
                App e0 e1       |  lng == HS    -> toString lng e0 ++ " " ++ toString lng e1
                                |  lng == C     -> toString lng e0 ++ "(" ++ toString lng e1 ++ ")"
                Def x e1        -> toString lng x ++ " = " ++ toString lng e1
                Let ds e1       -> (concat $ map (++";  ") $ map (toString lng) ds) ++ toString lng e1

                Brackets e      -> "(" ++ toString lng e ++ ")"

                _               -> error $ show expr

instance ToString AnnStmnt where
        toString lng annStmnt = case annStmnt of

                FunDef nm pars stmnts           -> "fundef " ++ toString lng nm ++ " " ++ toString lng pars ++ " {"
                                                           ++ (concat $ map ("\n"++) $ map (toString lng) stmnts)
                                                           ++ "}"

                Return e                        -> "return " ++ toString lng e ++ "; "

                Assign x e                      -> toString lng x ++ " = " ++ (toString lng $ addBrackets e) ++ ";\n"
                {-
                IfS e0 ps0 ps1                  -> "if " ++ toString lng e0 ++ " then " ++ (concat $ map ("\n"++) $ map (toString lng) ps0)
                                                                        ++ " else " ++ (concat $ map ("\n"++) $ map (toString lng) ps1)
                -}
                For loopCntr stmnts             -> "for (" ++ toString lng i ++ "=" ++ toString lng n0 ++ "; "
                                                           ++ toString lng i ++ "<" ++ toString lng ne ++ "; "
                                                           ++ toString lng i ++ "++) {"
                                                           ++ (concat $ map ("\n"++) $ map (toString lng) stmnts)
                                                           ++ "}"
                                                where
                                                  LoopCntr i n0 ne s = loopCntr

                PolcaFunc hof f es e            -> "Function toString not defined for Annotations (yet?)"
                PolcaDef name                   -> "Function toString not defined for Annotations (yet?)"
                PolcaIO iostr es                -> "Function toString not defined for Annotations (yet?)"

-- =============================================================================================================
-- =============================================================================================================
class PrPr a where
  toStrings :: Language -> a -> [String]

  prpr :: Language -> a -> IO ()
  prpr lng = putStr . ('\n':) . (++"\n") . unlines . toStrings lng

  prprList :: Language -> [a] -> IO ()
  prprList lng = putStr . ('\n':) . unlines . concat . map (++[""]) . map (toStrings lng)

instance PrPr ParseTree where
  toStrings lng tree = case tree of
     PLeaf t                 -> ["PLeaf " ++ show t]
     PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 4 $ concat $ addEndBrack $ addListNotation $ map (toStrings lng) ts)
                             where

     PError tr rule nt str k -> [ "==========="
                                , "Parse Error"
                                , "==========="
                                , "Recognized:"
                                , "-----------"
                                ]
                                ++ toStrings lng tr ++
                                [ "-----------"
                                , "Still to go:      " ++ show rule
                                , "Expected:         " ++ show nt
                                , "Found:            " ++ str
                                , "At position:      " ++ show k
                                , "==========="
                                ]

instance PrPr RoseTree where
  toStrings lng (RoseNode str [RoseNode str' []]) = [str ++ " " ++ str']
  toStrings lng (RoseNode str ts)                 = str : (addSpace 4 $ concat $ map (toStrings lng) ts)

appTerms e = case e of
        App f x    -> appTerms f ++ appTerms x
        Brackets x -> appTerms x
        _          -> [e]

instance PrPr Expr where
  toStrings lng expr = case expr of
     Const n          -> [show n]
     Idf (c:str)      -> [tolower c : str]

     Empty            -> ["Empty"]
     Ref e            -> ["*" ++ toString lng e]
     Deref e          -> ["&" ++ toString lng e]

     IfE t th_br e_br |  lng == C        -> ["if (" ++ toString lng t ++ ") { "]
                                                  ++ (addSpace 2 $ toStrings lng th_br)
                                                  ++ ["} else {"]
                                                  ++ (addSpace 2 $ toStrings lng e_br)
                                                  ++ ["}"]
                      |  lng == HS       -> ["if " ++ toString lng t]
                                                  ++ ["  then"]
                                                  ++ (addSpace 4 $ toStrings lng th_br)
                                                  ++ ["  else"]
                                                  ++ (addSpace 4 $ toStrings lng e_br)
                      |  otherwise       -> error ("toStrings: strange If argument: " ++ toString lng expr)
     Tuple es         -> ["(" ++ (concatWith ',' $ map (toString lng) es) ++ ")"]
     List es          -> ["[" ++ (concatWith ',' $ map (toString lng) es) ++ "]"]
     Field str e      -> [str ++ "=" ++ toString lng e]
     Rec es           -> ["Rec { " ++ (concatWith2 (" ,") $ map (toString lng) es) ++ " }"]

     LstSel xs i      |  lng == C        -> [toString lng xs ++ "[" ++ toString lng i ++ "]"]
                      |  lng == HS       -> [toString lng xs ++ "!!" ++ toString lng i]
                      |  otherwise       -> error ("toStrings: strange LstSel argument: " ++ toString lng expr)
     RecSel e e'      |  lng == C        -> [toString lng e ++ "." ++ e']
                      |  lng == HS       -> [e' ++ " " ++ toString lng e]
                      |  otherwise       -> error ("toStrings: strange RecSel argument: " ++ toString lng expr)
     UpdLst xs i e    |  lng == HS       -> [toString lng xs  ++ " <~ (" ++ toString lng i ++ ", " ++ toString lng e ++ ")"]
                      |  lng == C        -> [toString lng xs ++ "[" ++ toString lng i ++ "] = " ++ toString lng e]
                      |  otherwise       -> error ("toStrings: strange UpdLst argument: " ++ toString lng expr)
     UpdRec rec nm e  |  lng == HS       -> [toString lng rec ++ " { " ++ nm ++ " = " ++ toString lng e ++ " }"]
                      |  lng == C        -> [toString lng rec ++ "." ++ nm ++ " = " ++ toString lng e]
                      |  otherwise       -> error ("toStrings: strange UpdRec argument: " ++ toString lng expr)

     BinInfix o e e'  -> [toString lng e ++ " " ++ toString lng o ++ " " ++ toString lng e']

     App f e          |  -- trace ("TEST APP: " ++ show expr)
                         lambdas /= []    -> -- trace "NOT EMPTY"
                                             toStrings lng f ++ (addSpace 2 $ toStrings lng e) 
                      |  otherwise        -> [toString lng f ++ " " ++ toString lng e]             -- TODO
                      where
                        lambdas = [ 1 | Lambda _ _ <- appTerms expr ]

     Lambda x e       -> -- trace "lambda" $
                         ["\\" ++ toString lng x ++ " -> "]  ++ (addSpace 3 $ toStrings lng e)

     Def x e          -> [lhs ++ str] ++ defBody ++ skipLine
                      where
                        str:strs = toStrings lng e
                        lhs = toString lng x ++ " = "
                        defBody = addSpace (length lhs) strs
                        skipLine | length defBody > 1 = [""]
                                 | otherwise          = []

     Let defs e       -> ["let"] ++ (addSpace 2 $ concat $ map (toStrings lng) defs) ++ ["in"] ++ (addSpace 2 $ toStrings lng e)

     Brackets e       -> (init $ str0 : strs) ++ [strl ++ ")"]
                      where
                        str:strs = toStrings lng $ addBrackets e
                        str0     = '(' : str
                        strl     = last $ str0 : strs




instance PrPr AnnStmnt where
  toStrings C annStmnt = case annStmnt of

     FunDef nm pars stmnts   ->    ["", "fundef " ++ toString C nm ++ " " ++ toString C pars ++ " {"]
                                ++ (addSpace 2 $ concat $ map (toStrings C) stmnts)
                                ++ ["}"]

     Return e                -> ["return " ++ toString C e ++ ";"]

     Assign x e              -> [toString C x ++ " = " ++ (toString C $ addBrackets e) ++ ";"]

     IfS e0 ps0 ps1          -> ["if"] ++ addSpace 2 [toString C e0] ++                                                         -- NOT correct
                                                   ["then {"] ++ addSpace 2 (concat $ map (toStrings C) ps0) ++ ["}"] ++
                                                   ["else {"] ++ addSpace 2 (concat $ map (toStrings C) ps1) ++ ["}"]

     For loopCntr stmnts     -> ("for (" ++ toString C i ++ "=" ++ toString C n0 ++ "; "
                                         ++ toString C i ++ "<" ++ toString C ne ++ "; "
                                         ++ toString C i ++ "++) {")
                                         -- ++ (concat $ map ("\n"++) $ map (toString C) stmnts))
                                         :  (addSpace 2 $ concat $ map (toStrings C) stmnts)
                                         ++ ["}"]
                             where
                               LoopCntr i n0 ne s = loopCntr

     PolcaFunc hof f es e    -> ["<* Function toStrings not defined for Annotations (yet?) *>"]
     PolcaDef name           -> ["<* Function toStrings not defined for Annotations (yet?) *>"]
     PolcaIO iostr es        -> ["<* Function toStrings not defined for Annotations (yet?) *>"]

addBrackets expr = case expr of

     Const n          -> Const n
     Idf str          -> Idf str

     Empty            -> Empty
     Ref e            -> Ref e
     Deref e          -> Deref e

     IfE t th_br e_br -> IfE (addBrackets t) (addBrackets th_br) (addBrackets e_br)
     Tuple es         -> Tuple $ map addBrackets es
     List es          -> List  $ map addBrackets es
     Field str e      -> Field str $ addBrackets e
     Rec es           -> Rec $ map addBrackets es
     LstSel e i       |  (priority $ operator e) < priority Sel   -> LstSel (Brackets $ addBrackets e) i
                      |  otherwise                                -> LstSel (addBrackets e) i
     RecSel e e'      |  (priority $ operator e) < priority Sel   -> RecSel (Brackets $ addBrackets e) e'
                      |  otherwise                                -> RecSel (addBrackets e) e'
     UpdLst xs i e    -> UpdLst xs i e           -- Possibly not sufficient
     UpdRec rec nm e  -> UpdRec rec nm e         -- Possibly not sufficient

     BinInfix o e e'  -> BinInfix o br_e br_e'
                      where
                        br_e  | priority o > (priority $ operator e)     = Brackets $ addBrackets e
                              | otherwise                                = addBrackets e
                        br_e' | priority o > (priority $ operator e')    = Brackets $ addBrackets e'
                              | otherwise                                = addBrackets e'


     App f e          -> App br_f br_e
                      where
                        br_f | (priority $ operator f) < priority Apply  = Brackets $ addBrackets f
                             | otherwise                                 = addBrackets f
                        br_e | (priority $ operator e) < priority NoOp   = Brackets $ addBrackets e
                             | otherwise                                 = addBrackets e

     Lambda x e       -> -- trace "lambda s" $
                         Lambda x $ addBrackets e
     Def x e          -> Def x $ addBrackets e
     Let defs e       -> Let (map addBrackets defs) (addBrackets e)

     Brackets e       -> Brackets $ addBrackets e

-- =============================================================================================================
-- =============================================================================================================
-- =============================================================================================================

pyShowOp o = case o of
    Add         -> "Idf(\"+\")"
    Sub         -> "Idf(\"-\")"
    Mul         -> "Idf(\"*\")"
    Div         -> "Idf(\"/\")"
    _           -> error ("pyShowOp: --- " ++ show o ++ " --- not defined.")

pyShows []     = ""
pyShows [e]    = pyShow e
pyShows (e:es) = pyShow e ++ ", " ++ pyShows es

pyShow expr = case expr of
    Const n             -> "Constant(" ++ show n ++ ")"            -- quotation marks around n?
    Idf str             -> "Idf(" ++ show str ++ ")"
    Empty               -> "Empty"                              -- niet nodig
    Ref e               -> "Ref(" ++ pyShow e ++ ")"            -- niet nodig
    Deref e             -> "Deref(" ++ pyShow e ++ ")"          -- niet nodig

    IfE tst th_br e_br  -> "If(" ++ pyShow tst ++ ", " ++ pyShow th_br ++ ", " ++ pyShow e_br ++ ")"    -- ??
    Tuple es            -> "Tuple([" ++ pyShows es ++ "])"
    List  es            -> "List([" ++ pyShows es ++ "])"       -- niet nodig ??
    -- Field nm e          ->
    Rec es              -> "Tuple([" ++ pyShows py_es ++ "])"      -- !! Records into tuples
                        where
                          py_es = [ e | Field nm e <- es ]
    -- LstSel xs i         ->
    RecSel rec nm       -> pyShow $ App (Idf nm) rec            -- !! RecSel as application
    -- UpdLst xs i e       ->
    UpdRec rec nm e     -> "Idf(\"<<<RecUpd>>>\")"                       -- TODO -- important for SDF-analysis: complexity of e

    BinInfix o e0 e1    -> "App( App(" ++ pyShowOp o ++ ", " ++ pyShow e0 ++ "), " ++ pyShow e1 ++ ")"
    App f x             -> "App(" ++ pyShow f ++ ", " ++ pyShow x ++ ")"
    Lambda x body       -> "Lambda([" ++ pyShow x ++ "], " ++ pyShow body ++ ")"
    Def x e             -> "Def(" ++ pyShow x ++ ", " ++ pyShow e ++ ")"
    Let defs e          -> "Let([" ++ pyShows defs ++ "], " ++ pyShow e ++ ")"
    Brackets e          -> pyShow e
    _                   -> error ("pyShow: --- " ++ show expr ++ " --- not defined")

tps 1 = pyShow $ Def (Idf "dx") (BinInfix Sub (RecSel (Idf "p_1") "x") (RecSel (Idf "p_2") "x"))

tps 2 = pyShow $ (Let [Def (Idf "f_3'3")
                           (UpdRec (Idf "f_3") "x" (Const 0))
                                   ,Def (Idf "f_3'4")
                           (UpdRec (Idf "f_3'3") "y" (Const 0))
                                   ,Def (Idf "f_3'5")
                           (UpdRec (Idf "f_3'4") "z" (Const 0))
                                   ,Def (Idf "f_3'6")
                           (App
                             (App
                               (App
                                 (Idf "foldl")
                                 (Lambda (Idf "f_0")
                                         (Lambda (Idf "p_1")
                                                 (Let [Def (Idf "dx")
                                                           (BinInfix Sub (RecSel (Idf "p_1") "x") (RecSel (Idf "p_2") "x"))
                                                      ,Def (Idf "dy")
                                                           (BinInfix Sub (RecSel (Idf "p_1") "y") (RecSel (Idf "p_2") "y"))
                                                      ,Def (Idf "dz")
                                                           (BinInfix Sub (RecSel (Idf "p_1") "z") (RecSel (Idf "p_2") "z"))
                                                      ,Def (Idf "distSqr")
                                                           (BinInfix Add (BinInfix Mul (Idf "dx") (Idf "dx"))
                                                                                       (BinInfix Add (BinInfix Mul (Idf "dy") (Idf "dy"))
                                                                                                     (BinInfix Add (BinInfix Mul (Idf "dz") (Idf "dz"))
                                                                                                                   (Idf "SOFTENING"))))
                                                      ,Def (Idf "invDist")
                                                           (BinInfix Div (Const 1) (App (Idf "sqrt") (Idf "distSqr")))
                                                      ,Def (Idf "invDist3")
                                                           (BinInfix Mul (Idf "invDist")
                                                                         (BinInfix Mul (Idf "invDist")
                                                                                       (Idf "invDist")))
                                                      ,Def (Idf "f_0'0")
                                                           (UpdRec (Idf "f_0") "x" (BinInfix Add (RecSel (Idf "f_0") "x")
                                                                                                 (BinInfix Mul (Idf "dx")
                                                                                                               (Idf "invDist3"))))
                                                      ,Def (Idf "f_0'1")
                                                           (UpdRec (Idf "f_0'0") "y" (BinInfix Add (RecSel (Idf "f_0'0") "y")
                                                                                                   (BinInfix Mul (Idf "dy")
                                                                                                                 (Idf "invDist3"))))
                                                      ,Def (Idf "f_0'2")
                                                           (UpdRec (Idf "f_0'1") "z" (BinInfix Add (RecSel (Idf "f_0'1") "z")
                                                                                                   (BinInfix Mul (Idf "dz")
                                                                                                                 (Idf "invDist3"))))
                                                      ]
                                                      (Idf "f_0'2")))))
                                                     (Idf "f_3'5"))
                                                   (Idf "pos"))
                                            ]
                                            (Idf "f_3'6"))








nBody = pyShow $

      Let [Def (Idf "dt")
               (Const 1)
          ,Def (Idf "SOFTENING")
               (Const 1)

          ,Def (Idf "vAdd")
               (Lambda (Tuple [Idf "x", Idf "y", Idf "z"])
                       (Lambda (Tuple [Idf "x'", Idf "y'", Idf "z'"])
                               (Tuple [BinInfix Add (Idf "x")
                                                    (Idf "x'")
                                      ,BinInfix Add (Idf "y")
                                                    (Idf "y'")
                                      ,BinInfix Add (Idf "z")
                                                    (Idf "z'")
                                      ])))

          ,Def (Idf "vSub")
               (Lambda (Tuple [Idf "x", Idf "y", Idf "z"])
                       (Lambda (Tuple [Idf "x'", Idf "y'", Idf "z'"])
                               (Tuple [BinInfix Sub (Idf "x")
                                                    (Idf "x'")
                                      ,BinInfix Sub (Idf "y")
                                                    (Idf "y'")
                                      ,BinInfix Sub (Idf "z")
                                                    (Idf "z'")
                                      ])))

          ,Def (Idf "sMul")
               (Lambda (Idf "a")
                       (Lambda (Tuple [Idf "x", Idf "y", Idf "z"])
                               (Tuple [BinInfix Mul (Idf "a")
                                                    (Idf "x")
                                      ,BinInfix Mul (Idf "a")
                                                    (Idf "y")
                                      ,BinInfix Mul (Idf "a")
                                                    (Idf "z")
                                      ])))

          ,Def (Idf "dotPr")
               (Lambda (Tuple [Idf "x", Idf "y", Idf "z"])
                       (Lambda (Tuple [Idf "x'", Idf "y'", Idf "z'"])
                               (BinInfix Add (BinInfix Mul (Idf "x")
                                                           (Idf "x'"))
                                             (BinInfix Add (BinInfix Mul (Idf "y")
                                                                         (Idf "y'"))
                                                           (BinInfix Mul (Idf "z")
                                                                         (Idf "z'"))))))

          ,Def (Idf "incrF")
               (Lambda (Idf "p_2")
                       (Lambda (Idf "f_0")
                               (Lambda (Idf "p_1")
                                       (Let [Def (Idf "d")
                                                 (App
                                                     (App (Idf "vSub")
                                                          (Idf "p_1"))
                                                     (Idf "p_2"))
                                            ,Def (Idf "distSqr")
                                                 (BinInfix Add (App
                                                                   (App (Idf "dotPr")
                                                                        (Idf "d"))
                                                                   (Idf "d"))
                                                               (Idf "SOFTENING"))
                                            ,Def (Idf "invDist")
                                                 (BinInfix Div (Const 1)
                                                               (App
                                                                   (Idf "sqrt")
                                                                   (Idf "distSqr")))
                                            ,Def (Idf "invDist3")
                                                 (BinInfix Mul (Idf "invDist")
                                                               (BinInfix Mul (Idf "invDist")
                                                                             (Idf "invDist")))
                                            ,Def (Idf "f_0'")
                                                 (App
                                                     (App
                                                         (Idf "vAdd")
                                                         (Idf "f_0"))
                                                     (App
                                                         (App
                                                             (Idf "sMul")
                                                             (Idf "invDist3"))
                                                         (Idf "d")))
                                            ]
                                            (Idf "f_0'")))))

          ,Def (Idf "calcF")
               (Lambda (Idf "pos")
                       (Lambda (Idf "p_2")
                               (App
                                   (App
                                       (App
                                           (Idf "foldl")
                                           (App
                                               (Idf "incrF")
                                               (Idf "p_2")))
                                       (Tuple [Const 0, Const 0, Const 0]))
                                   (Idf "pos"))))


          ,Def (Idf "updV")
               (Lambda (Idf "v_0")
                       (Lambda (Idf "f_1")
                               (App
                                   (App
                                       (Idf "vAdd")
                                       (Idf "v_0"))
                                   (App
                                       (App
                                           (Idf "sMul")
                                           (Idf "dt"))
                                       (Idf "f_1")))))


          ,Def (Idf "updP")
               (Lambda (Idf "p_0")
                       (Lambda (Idf "v_1")
                               (App
                                   (App
                                       (Idf "vAdd")
                                       (Idf "p_0"))
                                   (App
                                       (App
                                           (Idf "sMul")
                                           (Idf "dt"))
                                       (Idf "v_1")))))

          ,Def (Idf "bodyForce")
               (Lambda (Tuple [Idf "pos"])
                       (App
                           (App
                               (Idf "map")
                               (App
                                   (Idf "calcF")
                                   (Idf "pos")))
                           (Idf "pos")))

          ,Def (Idf "velocities")
               (Lambda (Tuple [Idf "frc",Idf "vel"])
                       (App
                           (App
                               (App
                                   (Idf "zipWith")
                                   (Idf "updV"))
                               (Idf "vel"))
                           (Idf "frc")))

          ,Def (Idf "integrate")
               (Lambda (Tuple [Idf "pos",Idf "vel"])
                       (App
                           (App
                               (App
                                   (Idf "zipWith")
                                   (Idf "updP"))
                               (Idf "pos"))
                           (Idf "vel")))

          ,Def (Idf "Result")
               (App
                   (App
                       (App
                           (Idf "itn")
                           (Lambda (Tuple [Idf "p_0",Idf "v_1"])
                                   (Let [Def (Idf "frc")
                                             (App
                                                 (Idf "bodyForce")
                                                 (Idf "p_0"))
                                        ,Def (Idf "v_1'0")
                                             (App
                                                 (Idf "velocities")
                                                 (Tuple [Idf "frc",Idf "v_1"]))
                                        ,Def (Idf "p_0'1")
                                             (App
                                                 (Idf "integrate")
                                                 (Tuple [Idf "p_0",Idf "v_1'0"]))
                                        ]
                                        (Tuple [Idf "p_0'1",Idf "v_1'0"]))))
                       (Tuple [Idf "pos",Idf "vel"]))
                   (Idf "nIters"))

          ]

          (Idf "Result")

