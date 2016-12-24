{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module PrPrInstances where

import Types
import Debug.Trace

-- =================================================================================================
-- == Testing ======================================================================================
-- =================================================================================================

space n = map ((replicate n ' ') ++)

-- ============================================================================================


addSpace n = map ((replicate n ' ') ++)

addListNotation []                 =   [["["]]

addListNotation ([]:strss)         =   ["["]
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addListNotation ((str:strs):strss) =   (("["++str):strs)
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addEndBrack [strs]       = [ strs ++ ["]"] ]
addEndBrack (strs:strss) = strs : addEndBrack strss


instance PrPr ParseTree where

        toString tree = case tree of
                PLeaf t         -> show t
                PNode nt ts     -> show nt ++ "\n" ++ (concat $ map (++"\n") $ map toString ts)

        toStrings tree = case tree of
                PLeaf t                 -> ["PLeaf " ++ show t]
                PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 2 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)

                PError tr rule nt str k -> [ "==========="
                                           , "Parse Error"
                                           , "==========="
                                           , "Recognized:"
                                           , "-----------"
                                           ]
                                           ++ toStrings tr ++
                                           [ "-----------"
                                           , "Still to go:   " ++ show rule
                                           , "Expected:      " ++ show nt
                                           , "Found:         " ++ str
                                           , "At position:   " ++ show k
                                           , "==========="
                                           ]

        prpr  t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t

-- ============================================================================================

single expr = case expr of
      Const n           -> True
      I n               -> True
      Idf str           -> True
      List xs           -> True
      LstSel x i        -> False
      Numeric o x y     -> False
      BinInfix o x y    -> False
      App f x           -> False
      Lambda x b        -> False
      Def x e           -> True
      Let defs x        -> True

op2str op = case op of
      Add       -> " + "
      Sub       -> " - "
      Mul       -> " * "
      And       -> " && "
      Or        -> " || "
      Eql       -> " == "
      Gt        -> " > "

hs2str expr = case expr of
      Const n           -> show n
      I n               -> show n
      Idf str           -> str

      List []           -> "[]"
      List (e:es)       -> "[" ++ hs2str e ++ concat [ "," ++ hs2str e' | e' <- es ] ++ "]"
      LstSel xs i       -> hs2str xs ++ "!!" ++ hs2str i

      Numeric op e0 e1  -> hs2str e0 ++ " " ++ op ++ " " ++ hs2str e1
      BinInfix o e0 e1  -> hs2str e0 ++ op2str o ++ hs2str e1
      App f x           -> hs2str f ++ " " ++ l_br ++ hs2str x ++ r_br
                        where
                          (l_br,r_br) | single x   = ("","")
                                      | otherwise  = ("(",")")
      Lambda x e        -> "(\\" ++ hs2str x ++ " -> " ++ hs2str e ++ ")"

      Def x e           -> hs2str x ++ " = " ++ hs2str e
      Let defs e        -> "let " ++ concat (map hs2str defs) ++ " in " ++ hs2str e



instance PrPr Expr where

        toString e = case e of
                Idf str         -> str
                I   n           -> show n
                F   n           -> show n
                B   b           -> show b
                Numeric o e e'  -> lb ++ toString e ++ rb ++ o ++ lb' ++ toString e' ++ rb'
                                        where
                                          (lb,rb)   | prio o > prio (opr e)     = ("(",") ")
                                                    | otherwise                 = ("", " ")
                                          (lb',rb') | prio o > prio (opr e')    = (" (",")")
                                                    | otherwise                 = (" ", "")
                Boolean o e e'  -> toString e ++ " " ++ o ++ " " ++ toString e'
                Compose o e e'  -> toString e ++ " " ++ o ++ " " ++ toString e'
                Pair e0 e1      -> "(" ++ toString e0 ++ "," ++ toString e1 ++ ")"
                Triple e0 e1 e2 -> "(" ++ toString e0 ++ "," ++ toString e1 ++ ","++ toString e2 ++ ")"
                Null            -> "Null"
                Cons e es       -> toString e ++ " : " ++ toString es
                Lambda x e      -> "(\\" ++ toString x ++ " -> " ++ toString e ++ ")"
                App e0 e1       -> toString e0 ++ " (" ++ toString e1 ++ ")"
                Def x e1        -> toString x ++ " = " ++ toString e1
                Let ds e1       -> (concat $ map (++";  ") $ map toString ds) ++ toString e1
                IndSel xs i     -> toString xs ++ "[" ++ toString i ++ "]"
                FldSel rc n     -> toString rc ++ "." ++ toString n
                IfE e0 e1 e2    -> "if " ++ toString e0 ++ "then " ++ toString e1 ++ "else " ++ toString e2

                Func xs stmnts e ->  ("\n  func " ++ (concat $ map (++" ") $ map toString xs) ++ "{\n")
                                        ++ (concat $ space 4 $ map (++"\n") $ concat $ map toStrings stmnts)
                                        ++ (concat $ space 4 $ ["return " ++ toString e ++ ";\n"])
                                        ++ (concat $ space 2 ["}"])

        -- ====================================================================================

        toStrings e = case e of
                Idf str         -> ["Idf "++str]
                I   n           -> ["I "++show n]
                F   n           -> ["F "++show n]
                B   b           -> ["B "++show b]
                Numeric o e0 e1 -> ["Numeric "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
                Boolean o e0 e1 -> ["Boolean "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
                Compose o e0 e1 -> ["Compose "++o] ++ space 2 (toStrings e0 ++ toStrings e1)
                Pair e0 e1      -> ["Pair"]        ++ space 2 (toStrings e0 ++ toStrings e1)
                Triple e0 e1 e2 -> ["Triple"]      ++ space 2 (toStrings e0 ++ toStrings e1 ++ toStrings e2)
                Null            -> ["Null"]
                Cons e0 es      -> ["Cons "]       ++ space 2 (toStrings e0 ++ toStrings es)
                Lambda x e0     -> ["(\\"          ++ toString x ++ " -> "] ++ space 2 (toStrings e0) ++ [")"]
                App e0 e1       -> ["App"]         ++ space 2 (toStrings e0 ++ toStrings e1)
                Def x e'        -> ["Def"]         ++ space 2 (toStrings x ++ toStrings e')
                Let [] e'       -> ["Let","in"]    ++ space 2 (toStrings e')
                Let ds e'       -> ["Let"]         ++ space 2 (concat $ map toStrings ds) ++ ["in"] ++ (space 2 $ toStrings e')
                IndSel e0 i     -> ["IndSel"]      ++ space 2 (toStrings e0 ++ toStrings i)
                FldSel e0 n     -> ["IndSel"]      ++ space 2 (toStrings e0 ++ toStrings n)
                IfE e0 e1 e2    -> ["If"]          ++ space 2 (toStrings e0) ++ ["then"] ++ space 2 (toStrings e1) ++ ["else"] ++ space 2 (toStrings e2)

                Func xs stmnts e -> ("func " ++ (concat $ map (++" ") $ map toString xs) ++ "{")
                                        :  (space 4 $ concat $ map toStrings stmnts)
                                        ++ (space 4 $ ["return " ++ toString e ++ ";"])
                                        ++  space 4 ["};\n"]

                Empty           -> ["Empty"]

                -- _               -> error ("toStrings: " ++ show e)
        -- ====================================================================================

        prpr  t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t

-- ============================================================================================

{-
instance PrPr Type where

        toString  tp = show tp
        toStrings tp = [show tp]
        prpr      tp = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings tp
-}

-- ============================================================================================

instance PrPr Stmnt where

        toString stmnt = case stmnt of
                Skip                            -> "skip;\n"
                Assign x e                      -> toString x ++ " = " ++ toString e ++ ";\n"
                IfS e0 ps0 ps1                  -> "if " ++ toString e0 ++ " then " ++ (concat $ map ("\n"++) $ map toString ps0)
                                                                        ++ " else " ++ (concat $ map ("\n"++) $ map toString ps1)
                Break                           -> "break;\n"
                For (Idf i) (b,n,s) stmnts      -> ("for (" ++ i ++ "...)")     -- "=" ++ show b ++ "; " ++ i ++ "<" ++ n ++ "; " ++ i ++ "++)")
                                                        ++ (concat $ map ("\n"++) $ map toString stmnts)
                While e stmnts                  -> "while " ++ toString e
                                                        ++ (concat $ map ("\n"++) $ map toString stmnts)

        toStrings stmnt = case stmnt of
                Skip                            -> ["Skip;"]
                Assign x e                      -> [toString x ++ " = " ++ toString e ++ ";"]
                IfS e0 ps0 ps1                  -> ["if"] ++ space 2 [toString e0] ++
                                                   ["then {"] ++ space 2 (concat $ map toStrings ps0) ++ ["}"] ++
                                                   ["else {"] ++ space 2 (concat $ map toStrings ps1) ++ ["};"]
                Break                           -> ["break;"]
                For (Idf i) (b,n,s) stmnts      -> ("for (" ++ i ++ "...) {")   -- "=" ++ show b ++ "; " ++ i ++ "<" ++ n ++ "; " ++ i ++ "++) {")
                                                        : (space 2 $ concat $ map toStrings stmnts) ++ ["};"]
                While e stmnts                  -> ("while " ++ toString e ++ " {")     -- "=" ++ show b ++ "; " ++ i ++ "<" ++ n ++ "; " ++ i ++ "++) {")
                                                        : (space 2 $ concat $ map toStrings stmnts) ++ ["};"]

        prpr  stmnt  = putStr $ toString stmnt

-- ============================================================================================

instance PrPr [Stmnt] where

        toString stmnts         = concat $ map toString stmnts
        toStrings stmnts        = concat $ map toStrings stmnts

        prpr stmnts = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings stmnts

-- ============================================================================================




