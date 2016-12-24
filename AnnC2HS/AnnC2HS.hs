module AnnC2HS where

-- import GHC.Generics
-- import FPPrac.Trees
-- import PolcaAlphabet
-- import PolcaGrammar
-- import Tokenizer
import TypesEtc
import SubstInstances
import PrPr
-- import ParserGen
-- import PT2AST
import Data.List
import Debug.Trace


newpatt :: Int -> Expr -> (Int,Expr)
newpatt k e = case e of
                 Tuple es               -> (k', Tuple vars)
                                        where
                                          (k',vars) = mapAccumL newpatt k es
                 List es                -> (k', List vars)
                                        where
                                          (k',vars) = mapAccumL newpatt k es
                 _                      -> (k+1 , Idf $ [head $ toString HS e] ++ "_" ++ show k)          -- New "variables" _1, _25, etc .... Wow, that works :-)

e2vs expr patt = case (expr,patt) of            -- gives list of replacements in the form of (e,p) 
                        (Tuple es, Tuple ps) -> concat $ zipWith e2vs es ps
                        (List  es, List  ps) -> concat $ zipWith e2vs es ps
                        _                    -> [(expr,patt)]

expr2var (e,p) expr | e == expr   = p
                    | otherwise   = case expr of

                        Const n                 -> Const n
                        Idf str                 -> Idf str
                        Ref y                   -> Ref $ expr2var (e,p) y
                        Deref y                 -> Deref $ expr2var (e,p) y
                        LstSel xs i             -> LstSel (expr2var (e,p) xs) (expr2var (e,p) i)
                        RecSel xs i             -> RecSel (expr2var (e,p) xs) i
                        IfE t th_br e_br        -> IfE (expr2var (e,p) t) (expr2var (e,p) th_br) (expr2var (e,p) e_br)
                        Tuple es                -> Tuple $ map (expr2var (e,p)) es
                        List es                 -> List $ map (expr2var (e,p)) es

                        BinInfix op e0 e1       -> BinInfix op (expr2var (e,p) e0) (expr2var (e,p) e1)
                        App f e0                -> App (expr2var (e,p) f) (expr2var (e,p) e0)
                        Lambda y body           -> Lambda y $ expr2var (e,p) body                 -- !!! Assume no variable clashes!
                        Def y e0                -> Def (expr2var (e,p) y) $ expr2var (e,p) e0
                        Let defs e0             -> Let (map (expr2var (e,p)) defs) (expr2var (e,p) e0)


replaceAll replacements expr = foldl (\expr (e,p) -> expr2var (e,p) expr) expr replacements

{-
lambdaExtr :: Int -> [Expr] -> Expr -> (Int,Expr)
lambdaExtr k [] expr = (k, expr)
lambdaExtr k (iexpr:iexprs) (Let defs expr) = (k'', Lambda p_k body)
                                            where
                                              (k'',body)   = lambdaExtr k' iexprs $ Let (map (replaceAll replacements) defs) (replaceAll replacements expr)
                                              (k',p_k)     = newpatt k iexpr
                                              replacements = e2vs iexpr p_k
-}

lambdaExtr :: Int -> [Expr] -> Expr -> Expr -> (Int,Expr)
lambdaExtr k  []            oexpr (Let defs expr)           = (k, body)
                                            where
                                              (k',p_k)     = newpatt k oexpr
                                              replacements = e2vs oexpr p_k
                                              body         = Let (map (replaceAll replacements) defs) (replaceAll replacements expr)

lambdaExtr k (iexpr:iexprs) oexpr (Let defs expr) = (k'', Lambda p_k body)
                                            where
                                              (k',p_k)     = newpatt k iexpr
                                              replacements = e2vs iexpr p_k
                                              (k'',body)   = lambdaExtr k' iexprs oexpr $ Let (map (replaceAll replacements) defs) (replaceAll replacements expr)

-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
annC2hs :: Int -> [AnnStmnt] -> (Int,[Expr])
annC2hs k []        = (k, [])
{-
annC2hs k [c]       = case c of
                        Assign x e                      -> (k, [ Def x e ])
                        x                               -> error ("annC2hs: should not occur (A): " ++ show x)
-}

annC2hs k ( FunDef name pars body
          : stmnts                 ) = ( k'', Def name lambdaExpr : exprs )
                                   where
                                     Return x   = last body                     -- NOTE: "return" MUST be the last statement in a fundef
                                     (k', defs) = annC2hs k $ init body

                                     lambdaExpr = Lambda pars $ Let defs x

                                     (k'', exprs) = annC2hs k stmnts

annC2hs k ( Assign x e
          : stmnts                 ) = ( k', Def x e : exprs )
                                   where
                                     (k', exprs) = annC2hs k stmnts

annC2hs k ( PolcaIO "output" [expr]
          : IfS tst th_br e_br
          : stmnts                 ) = ( k2, Def expr ifE : exprs )
                                   where
                                     (k0, letThen) = annC2hs k  th_br
                                     (k1, letElse) = annC2hs k0 e_br

                                     ifE = IfE tst (Let letThen expr) (Let letElse expr)

                                     (k2 , exprs) = annC2hs k1 stmnts
annC2hs k ( PolcaIO "output" [expr]
          : stmnts                 ) = ( k', [Let defs expr] )
                                   where
                                     (k', defs) = annC2hs k stmnts

annC2hs k ( PolcaFunc hof fName es tgt
          : For (LoopCntr i n0 ne s)                            -- for now: i, n0, ne, s ingnored
                ( PolcaDef fName'
                : PolcaIO "input"  iexprs
                : PolcaIO "output" [oexpr]
                : body
                )
          : stmnts                 ) = (k2, Def tgt hofExpr : exprs )
                                   where
                                     (k0, defs)    = annC2hs k body
                                     (k1, f)       = lambdaExtr k0 iexprs oexpr (Let defs oexpr)        -- only place where k is really used

                                     hofExpr       = foldl App (Idf hof) (f:es)

                                     (k2, exprs)   = annC2hs k1 stmnts


annC2hs k ( x : stmnts             ) = error ("annC2hs: should not occur (B): " ++ show x)



-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
{-
annC2hs :: Int -> [AnnStmnt] -> (Int,[Expr])
annC2hs k []        = (k, [])
annC2hs k [c]       = case c of
                        Assign x e                      -> (k, [ Def x e ])
                        x                               -> error ("annC2hs: should not occur (A): " ++ show x)

annC2hs k (c:c':cs) = case c of

    FunDef name pars stmnts        -> trace "TEST FunDef" traceShow (c,c')
                                      ( k'', Def name lambdaExpr : exprs )
                                   where
                                     Return x   = last stmnts                     -- NOTE: "return" MUST be the last statement in a fundef
                                     (k', defs) = annC2hs k $ init stmnts

                                     lambdaExpr = Lambda pars $ Let defs x

                                     (k'', exprs) = annC2hs k (c':cs)

    Assign x e                     -> trace "TEST Assign" traceShow (c,c')
                                      ( k', Def x e : exprs )
                                   where
                                     (k', exprs) = annC2hs k (c':cs)

    PolcaIO "output" [expr]        -> trace "TEST PolcaIO output" traceShow (c,c')
                                      ( k3, Def expr ifE : exprs )
                                   where
                                     (k2, ifE) = case c' of
                                         IfS tst th_br e_br -> (k1, IfE tst letThen letElse)
                                                            where
                                                              (k0, letThen) = annC2hsOut k  (PolcaIO "output" [expr] : th_br)
                                                              (k1, letElse) = annC2hsOut k0 (PolcaIO "output" [expr] : e_br)

                                         _                  -> error "annCshs: no if-the-else??"

                                     (k3 , exprs) = annC2hs k2 cs


    PolcaFunc hof fName es tgt     -> trace "TEST PolcaFunc" traceShow (c,c')
                                      (k2, Def tgt hofExpr : exprs )
                                   where
                                     -- unpacking
                                     For loopCntr body  = c'
                                     LoopCntr i n0 ne s = loopCntr
                                     PolcaDef fName' : PolcaIO "input" iexprs : stmnts = body

                                     -- production
                                     (k0, letBody) = annC2hsOut k stmnts
                                     (k1,f)        = lambdaExtr k0 iexprs letBody        -- only place where k is really used

                                     hofExpr       = foldl App (Idf hof) (f:es)
                                     (k2,exprs)    = annC2hs k1 cs

    x                              -> error ("annC2hs: should not occur (B): " ++ show x)
-}


-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
-- ============================================================================================
annC2hsOut :: Int -> [AnnStmnt] -> (Int,Expr)
annC2hsOut k (PolcaIO "output" [expr] : stmnts) = (k', Let defs expr)
                                           where
                                             (k', defs) = annC2hs k stmnts

annC2hsOut k x    = error ("annC2hsOut: should not occur: " ++ show x)

-- ============================================================================================

sel2vars (Def x e) = case x of
                     LstSel e' i     -> sel2vars $ Def e' (UpdLst (sel2vars e') i (sel2vars e))
                     RecSel e' nm    -> Def e' (UpdRec (sel2vars e') nm (sel2vars e))
                     _               -> Def x $ sel2vars e

sel2vars expr      = case expr of
               Const n          -> Const n
               Idf str          -> Idf str
               Empty            -> Empty
               Ref e            -> Ref $ sel2vars e
               Deref e          -> Deref $ sel2vars e

               IfE t th_br e_br -> IfE (sel2vars t) (sel2vars th_br) (sel2vars e_br)
               Tuple es         -> Tuple $ map sel2vars es
               List es          -> List $ map sel2vars es
               Field nm e       -> Field nm $ sel2vars e
               Rec es           -> Rec $ map sel2vars es

               LstSel e e'      -> LstSel (sel2vars e) (sel2vars e')
               RecSel e e'      -> RecSel (sel2vars e) e'

               UpdLst e0 e1 e2  -> UpdLst (sel2vars e0) (sel2vars e1) (sel2vars e2)
               UpdRec e0 e1 e2  -> UpdRec (sel2vars e0) e1 (sel2vars e2)

               BinInfix o e e'  -> BinInfix o (sel2vars e) (sel2vars e')
               App f e          -> App (sel2vars f) (sel2vars e)
               Lambda x e       -> Lambda x (sel2vars e)
               -- Def x e          -> see above
               Let defs expr    -> Let (map sel2vars defs) (sel2vars expr)

allvars expr = case expr of

               Const n          -> []
               Idf str          -> [Idf str]
               Empty            -> []
               Ref e            -> allvars e
               Deref e          -> allvars e

               IfE t th_br e_br -> allvars t ++ allvars th_br ++ allvars e_br
               Tuple es         -> concat $ map allvars es
               List es          -> concat $ map allvars es
               Field nm e       -> allvars e
               Rec es           -> concat $ map allvars es
               LstSel e e'      -> allvars e ++ allvars e'
               RecSel e e'      -> allvars e
               UpdLst e0 e1 e2  -> allvars e0 ++ allvars e1 ++ allvars e2
               UpdRec e0 e1 e2  -> allvars e0 ++ allvars e2

               BinInfix o e e'  -> allvars e ++ allvars e'
               App f e          -> allvars f ++ allvars e
               Lambda x e       -> allvars e \\ allvars x
               Def x e          -> allvars x ++ allvars e                               -- !?!?!?
               Let defs expr    -> (concat $ map allvars defs) ++ allvars expr




-- xs *&* ys = intersect xs ys


newIdf k (Idf x) = (k+1, Idf x')
                 where
                   x' = takeWhile (/='\'') x ++ "\'" ++ show k


uniquevars :: Int -> [Expr] -> Expr -> (Int,Expr)
uniquevars k vars expr = case expr of                   -- to rename variables in let-definitions upto uniqueness;
                                                        -- to emulate non-recursive let-bindings
               Const n          -> (k, Const n)
               Idf str          -> (k, Idf str)
               Empty            -> (k, Empty)
               Ref e            -> (k', Ref e')
                                where
                                  (k',e') = uniquevars k vars e
               Deref e          -> (k', Deref e')
                                where
                                  (k',e') = uniquevars k vars e

               IfE t th_br e_br -> (k''', IfE t' th_br' e_br')
                                where
                                  (k'  ,t')     = uniquevars k vars t           -- probably not correct since t may contain let-expressions
                                  (k'' ,th_br') = uniquevars k' vars th_br
                                  (k''',e_br')  = uniquevars k'' vars e_br

               Tuple es         -> (k' , Tuple es')
                                where
                                  (ks,es') = unzip $ map (uniquevars k vars) es
                                  k' | null ks   = k
                                     | otherwise = maximum ks
               List es          -> (k' , List es')
                                where
                                  (ks,es') = unzip $ map (uniquevars k vars) es
                                  k' | null ks   = k
                                     | otherwise = maximum ks

               Field nm e       -> (k', Field nm e')
                                where
                                  (k',e') = uniquevars k vars e
               Rec es           -> (k' , Rec es')
                                where
                                  (ks,es') = unzip $ map (uniquevars k vars) es
                                  k' | null ks   = k
                                     | otherwise = maximum ks
               LstSel e e'      -> (k1, LstSel e0 e1)
                                where
                                  (k0,e0) = uniquevars k vars e
                                  (k1,e1) = uniquevars k0 vars e'
               RecSel e e'      -> (k0, RecSel e0 e')
                                where
                                  (k0,e0) = uniquevars k vars e
               UpdLst e0 e1 e2  -> (k2, UpdLst e0' e1' e2')
                                where
                                  (k0,e0') = uniquevars k vars e0
                                  (k1,e1') = uniquevars k0 vars e1
                                  (k2,e2') = uniquevars k1 vars e2
               UpdRec e0 e1 e2  -> (k1,UpdRec e0' e1 e2')
                                where
                                  (k0,e0') = uniquevars k vars e0
                                  (k1,e2') = uniquevars k0 vars e2

               BinInfix o e0 e1 -> (k'', BinInfix o e0' e1')
                                where
                                  (k',e0') = uniquevars k vars e0
                                  (k'',e1') = uniquevars k' vars e1
               App f e          -> (k'',App f' e')
                                where
                                  (k',f') = uniquevars k vars f
                                  (k'',e') = uniquevars k' vars e
               Lambda x e       -> (k', Lambda x e')
                                where
                                  (k',e') = uniquevars k (nub $ x:vars) e

               Def x e          -> (k', Def x e')
                                where
                                  (k',e') = uniquevars k (nub $ allvars x ++ vars) e

               Let defs expr    -> (k''', Let (init redefs) (last redefs))
                                where
                                  (k''',redefs) = redef k'' vars defs' expr'
                                  (ks,defs') = unzip $ map (uniquevars k vars) defs
                                  k' | null ks   = k
                                     | otherwise = maximum ks
                                  (k'',expr') = uniquevars k' vars expr         -- 2x same k ?!?!?




redef k vars   []             expr = (k, [expr])

redef k vars (Def x e : defs) expr = (k'', Def x' e : redefs)
                                   where
                                     toBeRenamed  = intersect (nub $ allvars x) (vars ++ allvars e)
                                     (k',newvars) = mapAccumL newIdf k toBeRenamed

                                     substs = zip toBeRenamed newvars
                                     x'     = foldl (<<=) x substs
                                     defs'  = [ foldl (<<=:) def substs | def <- defs ]
                                     expr'  = foldl (<<=) expr substs

                                     (k'', redefs) = redef k' (allvars x' ++ vars) defs' expr'

flatten var = case var of
        Idf x           -> [Idf x]
        Tuple xs        -> xs
        List xs         -> xs
        _               -> [var]








{-
uniqueIdfs k expr = case expr of
                     Const n         -> Const n
                     Idf str         
                     -- Ref Expr
                     -- Deref Expr

                     Tuple es
                     List es
                     Field nm e
                     Rec es
                     LstSel e e'
                     RecSel e e'

                     BinInfix o e e'
                     App f x
                     Lambda x e
                     Def x e         -> 
                                     where
                                       doubles = intersection x e
                     Let defs e

                     _               -> error ("psHS2execHS: " ++ show expr)
-}

{-
psHS2execHS globals substs expr = case expr of
-}

{-
psHS2execHS globals substs expr = case expr of
        Const n         -> Const n
        Idf str         
        -- Ref Expr
        -- Deref Expr

        Tuple es
        List es
        Field nm e
        Rec es
        LstSel e e'
        RecSel e e'

        BinInfix o e e'
        App f x
        Lambda x e
        Def x e
        Let defs e

        _               -> error ("psHS2execHS: " ++ show expr)
-}




-- ============================================================================================
-- ============================================================================================
