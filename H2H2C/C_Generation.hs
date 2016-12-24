module C_Generation where

import Types
import Parse
import SubstInstances
import Transformations
import Data.List
import Data.Maybe
import Debug.Trace

{-
env0 = [ ( Idf "fst"     , Fun (Tuple2 (TVar "a") (TVar "b")) (TVar "a") )
       , ( Idf "snd"     , Fun (Tuple2 (TVar "a") (TVar "b")) (TVar "b") )

       , ( Idf "plus"    , Fun (TVar "a") (Fun (TVar "a") (TVar "a")) )

       , ( Idf "map"     , Fun (Fun (TVar "a") (TVar "b")) (Fun (List (TVar "a")) (List (TVar "b"))) )
       , ( Idf "zipWith" , Fun   (Fun (TVar "a") (Fun (TVar "b") (TVar "c")))    (Fun (List (TVar "a")) (Fun (List (TVar "b")) (List (TVar "c")))) )
       ]

extractEnv (Idf x)        t                     = [(Idf x,t)]
extractEnv (Pair x y)     (Tuple2 tx ty)        = extractEnv x tx ++ extractEnv y ty
extractEnv (Triple x y z) (Tuple3 tx ty tz)     = extractEnv x tx ++ extractEnv y ty ++ extractEnv z tz
-}




-- ====================================================================================
-- ====================================================================================
-- ====================================================================================
-- ====================================================================================
-- Extract Higher Order Functions when nested in other expressions
-- ====================================================================================
extrHOF topLevel k e    | isHOF e && not topLevel       = (k'+1, e'')
                        | otherwise                     = (k',   e' )

  where
    v     = "v" ++ show k'
    e''   = Let [Def (Idf v) e'] (Idf v)

    (k',e') = case e of

        App (Idf "concat") xss                  -> (k0, (Idf "concat")#xss')            where   (k0, [xss'])            = mapAccumL (extrHOF False) k [xss]

        App (App (Idf "replicate") n) a         -> (k0, (Idf "replicate")#n'#a')        where   (k0, [n',a'])           = mapAccumL (extrHOF False) k [n,a]

        App (App (Idf "split") m) xs            -> (k0, (Idf "split")#m'#xs')           where   (k0, [m',xs'])          = mapAccumL (extrHOF False) k [m,xs]

        App (App (App (Idf "itn") f) x) n       -> (k0, (Idf "itn")#f'#x'#n')           where   (k0, [f',x',n'])        = mapAccumL (extrHOF False) k [f,x,n]
        App (App (App (Idf "itnscan") f) x) n   -> (k0, (Idf "itnscan")#f'#x'#n')       where   (k0, [f',x',n'])        = mapAccumL (extrHOF False) k [f,x,n]
        App (App (Idf "map") f) xs              -> (k0, (Idf "map")#f'#xs')             where   (k0, [f',xs'])          = mapAccumL (extrHOF False) k [f,xs]
        App (App (App (Idf "zipWith") f) xs) ys -> (k0, (Idf "zipWith")#f'#xs'#ys')     where   (k0, [f',xs',ys'])      = mapAccumL (extrHOF False) k [f,xs,ys]
        App (App (App (Idf "foldl") f) a) xs    -> (k0, (Idf "foldl")#f'#a'#xs')        where   (k0, [f',a',xs'])       = mapAccumL (extrHOF False) k [f,a,xs]
        App (App (App (Idf "scanl") f) a) xs    -> (k0, (Idf "scanl")#f'#a'#xs')        where   (k0, [f',a',xs'])       = mapAccumL (extrHOF False) k [f,a,xs]
        App (App (App (Idf "mapAccumL") f) a) xs-> (k0, (Idf "mapAccumL")#f'#a'#xs')    where   (k0, [f',a',xs'])       = mapAccumL (extrHOF False) k [f,a,xs]

        App (App (App (Idf "while") p) f) x     -> (k0, (Idf "while")#p'#f'#x')         where   (k0, [p',f',x'])        = mapAccumL (extrHOF False) k [p,f,x]

        App f a                                 -> (k', App f' a')                      where   (k', [f',a'])           = mapAccumL (extrHOF False) k [f,a]

        Numeric o e0 e1                         -> (k', Numeric o e0' e1')              where   (k', [e0',e1'])         = mapAccumL (extrHOF False) k [e0,e1]

        Boolean o e0 e1                         -> (k', Boolean o e0' e1')              where   (k', [e0',e1'])         = mapAccumL (extrHOF False) k [e0,e1]

        Compose o e0 e1                         -> (k', Compose o e0' e1')              where   (k', [e0',e1'])         = mapAccumL (extrHOF False) k [e0,e1]

        Pair e0 e1                              -> (k', Pair e0' e1')                   where   (k', [e0',e1'])         = mapAccumL (extrHOF False) k [e0,e1]

        Triple e0 e1 e2                         -> (k', Triple e0' e1' e2')             where   (k', [e0',e1',e2'])     = mapAccumL (extrHOF False) k [e0,e1,e2]

        Cons e0 e1                              -> (k', Cons e0' e1')                   where   (k', [e0',e1'])         = mapAccumL (extrHOF False) k [e0,e1]

        IndSel e0 i                             -> (k', IndSel e0' i)                   where   (k', e0')               = extrHOF False k e0
        FldSel e0 i                             -> (k', FldSel e0' i)                   where   (k', e0')               = extrHOF False k e0

        IfE e0 e1 e2                            -> (k', IfE e0' e1' e2')                where   (k0, e0')               = extrHOF False k e0
                                                                                                (k', [e1',e2'])         = mapAccumL (extrHOF True) k0 [e1,e2]

        -- TODO
        Def x e0                                -> (k', Def x e0')                      where   (k', e0')               = extrHOF True k e0

        Let ds e0                               -> (k', Let ds' e0')                    where   (k0, ds')               = mapAccumL (extrHOF False) k ds
                                                                                                (k', e0')               = extrHOF True k0 e0

        Lambda x e0                             -> (k', Lambda x e0')                   where   (k', e0')               = extrHOF True k e0

        _                                       -> (k, e)

-- ====================================================================================
-- Translation from Haskell to C        -- hs2c: Fingers Crossed ...

hs2c :: Int -> Expr -> (Int,[Stmnt])

-- TODO (2x)
hs2c k (Def z (Let [] (Idf x))) = (k, [Assign z (Idf x)])
hs2c k (Def z (Let ds (Idf x))) = (k', concat pss)
                                   where
                                     ds0      = map (rename (Idf x,z)) ds
                                     (k',pss) = mapAccumL hs2c k ds0


-- TODO
hs2c k (Def z (Let ds e)) = (k', concat pss)
                where
                  (k',pss) = mapAccumL hs2c k (ds ++ [Def z e])





hs2c k (Def z e) = case e of         -- TODO

        App (Idf "concat") xss                  -> ( k', [ For i (0,"N",1) [For j (0,"m",1) ps] ] )             -- <<<=== !!! (0,n,1)
                                                where
                                                  i             =  Idf ("i"++show k)
                                                  j             =  Idf ("i"++show (k+1))
                                                  ind           =  Numeric "+" (Numeric "*" i (Idf "m")) j
                                                  (k',ps)       =  hs2c (k+2) $ Def (z!ind) (topLet $ simplify $ xss!i!j)         -- TODO

        App (App (Idf "replicate") n) t         -> ( k', [ For i (0,"N",1) ps ] )               -- <<<=== !!! (0,n,1)
                                                where
                                                  i             =  Idf ("i"++show k)
                                                  (k', ps)      =  hs2c (k+1) $ Def (z!i) (topLet $ simplify t)                   -- TODO

        App (App (Idf "split") m) xs            -> ( k', [ For i (0,"N",1) $ [For j (0,"m",1) ps] ] )           -- <<<=== !!! (0,n,1)
                                                where
                                                  i             =  Idf ("i"++show k)
                                                  j             =  Idf ("i"++show (k+1))
                                                  ind           =  Numeric "+" (Numeric "*" i (Idf "m")) j
                                                  (k', ps)      =  hs2c (k+2) $ Def (z!i!j) (topLet $ simplify $ xs!ind)         -- TODO

        App (App (App (Idf "itn") f) a) n       -> ( k', psa ++ [ For i (0,"n",1) psx ]         -- <<<=== !!! (0,n,1)
                                                   )
                                                where
                                                  (k0, psa)     =  hs2c k $ Def z (topLet $ simplify a)  -- TODO

                                                  i             =  Idf ("i"++show k0)
                                                  (k', psx)     =  hs2c (k0+1) $ Def z (topLet $ simplify $ f#z)         -- TODO

        App (App (App (Idf "itnscan") f) a) n   -> ( k', psa ++ [ For i (0,"n",1) psx ]         -- <<<=== !!! (0,n,1)
                                                   )
                                                where
                                                  (k0, psa)     =  hs2c k $ Def (z!(I 0)) (topLet $ simplify a)        -- TODO

                                                  i             =  Idf ("i"++show k0)
                                                  (k', psx)     =  hs2c (k0+1) $ Def (z!(Numeric "+" i (I 1))) (topLet $ simplify $ f#(z!i))   -- TODO

        App (App (Idf "map") f) xs              -> ( k', [ For i (0,"N",1) ps ] )               -- <<<=== !!! (0,n,1)
                                                where
                                                  i             =  Idf ("i"++show k)
                                                  (k', ps)      =  hs2c (k+1) $ Def (z!i) (topLet $ simplify $ f#(xs!i))         -- TODO

        App (App (App (Idf "zipWith") f) xs) ys -> ( k', [ For i (0,"N",1) ps ] )               -- <<<=== !!! (0,n,1)
                                                where
                                                  i             =  Idf ("i"++show k)
                                                  (k', ps)      =  hs2c (k+1) $ Def (z!i) (topLet $ simplify $ f#(xs!i)#(ys!i))  -- TODO

        App (App (App (Idf "foldl") f) a) xs    -> ( k', psa ++ [ For i (0,"N",1) psx ]         -- <<<=== !!! (0,n,1)
                                                   )
                                                where
                                                  (k0, psa)     =  hs2c k $ Def z (topLet $ simplify a)

                                                  i             =  Idf ("i"++show k0)
                                                  (k', psx)     =  hs2c (k0+1) $ Def z (topLet $ simplify $ f#z#(xs!i))  -- TODO: type of xs!i ?

        App (App (App (Idf "scanl") f) a) xs    -> ( k', psa ++ [ For i (0,"N",1) psx ]         -- <<<=== !!! (0,n,1)
                                                   )
                                                where
                                                  (k0, psa)     =  hs2c k $ Def (z!(I 0)) (topLet $ simplify a)        -- TODO

                                                  i             =  Idf ("i"++show k0)
                                                  (k', psx)     =  hs2c (k0+1) $ Def (z!(Numeric "+" i (I 1))) (topLet $ simplify $ f#(z!i)#(xs!i))    -- TODO

        App (App (App (Idf "mapAccumL") f) a) xs-> ( k', psa ++ [ For i (0,"N",1) psx ]         -- <<<=== !!! (0,n,1)
                                                   )
                                                where
                                                  v             =  Idf ("v"++show k)
                                                  (k0, psa)     =  hs2c (k+1) $ Def v (topLet $ simplify a)      -- TODO

                                                  i             =  Idf ("i"++show k0)
                                                  (k', psx)     =  hs2c (k0+1) $ Def (Pair v (z!i)) (topLet $ simplify $ f#v#(z!i))      -- TODO








        Lambda x (Let [] (Lambda y body))       -> ( k'', [Assign z $ Func (x:ys) stmnts e'] )                           -- TODO typing
                                                where
                                                  (k'', [Assign _ (Func ys stmnts e')]) = hs2c k (Def z (Lambda y body))

        Lambda x (Let ds body)                  ->  ( k'', [Assign z $ Func [x] (concat pss ++ qs') e'] )                -- TODO typing
                                                where
                                                  (k',pss)  = mapAccumL hs2c k ds

                                                  vk        = Idf ("w" ++ show k')

                                                  (k'', qs) = hs2c (k'+1) (Def vk body)

                                                  (qs',e')  | isHOF body || isLambda body       = (qs, vk)
                                                            | otherwise                         = ([], body)




        Lambda x body                           -> hs2c k $ Def z (Lambda x $ Let [] body)       -- TODO

        App (App (Compose "<." f g) x) y        ->  ( k', ps)
                                                where
                                                  (k', ps)      = hs2c k $ Def z (topLet $ simplify $ f#x#(g#y))         -- TODO

        App (Compose "." f g) x                 ->  ( k', ps)
                                                where
                                                  (k', ps)      = hs2c k $ Def z (topLet $ simplify $ App f (App g x))   -- TODO


        App (Compose ";" f g) x                 ->  ( k', concat pss)
                                                where
                                                  (k', pss)     = mapAccumL hs2c k [ Def z (topLet $ simplify $ App f x)         -- TODO
                                                                                   , Def z (topLet $ simplify $ App g z)         -- TODO
                                                                                   ]

        App (Compose ";>" f g) x                ->  ( k', concat pss)
                                                where
                                                  (k', pss)     = mapAccumL hs2c k [ Def z (topLet $ simplify $ f#x)     -- TODO
                                                                                   , Def z (topLet $ simplify $ g#z#x)   -- TODO
                                                                                   ]

        Empty                                   ->  ( k, [] )

        IfE e0 e1 e2                            ->  ( k', [IfS e0 ps0' ps1'] )
                                                where
                                                  (k0,ps0') = hs2c k  $ Def z e1         -- TODO  -- $ topLet $ simplify e1
                                                  (k',ps1') = hs2c k0 $ Def z e2         -- TODO  -- $ topLet $ simplify e2

        -- App (f;g) e'                 -> ... Let [Def z (f#e'), Def z (g#z)] Skip

        _                                       ->  ( k, [Assign z e] )          -- TODO typing

hs2c k e = error ("hs2c: " ++ show e)

{-
Let
        [Def    (Idf "h")
                (Lambda (Idf "x")
                        (App (Idf "f") (App (Idf "g") (Idf "x"))))
        ]

        (App (App (Idf "map") (Idf "h")) (Idf "V"))
-}

-- ====================================================================================

isHOF e = case e of
                App (Idf "concat") _                    -> True
                App (App (Idf "replicate") _) _         -> True
                App (App (Idf "split") _) _             -> True
                App (App (Idf "map") _) _               -> True
                App (App (App (Idf "zipWith") _) _) _   -> True
                App (App (App (Idf "foldl") _) _) _     -> True
                _                                       -> False

isLambda e = case e of
                Lambda _ _                              -> True
                _                                       -> False


-- ====================================================================================

remRedun []              = []
remRedun (stmnt : stmnts) = case stmnt of
                                Break                           -> Break : remRedun stmnts
                                Skip                            -> Skip : remRedun stmnts
                                Assign x e      | x == e        -> remRedun stmnts
                                                | otherwise     -> stmnt : remRedun stmnts
                                IfS e0 stmnts0 stmnts1          -> IfS e0 (remRedun stmnts0) (remRedun stmnts1) : remRedun stmnts
                                For i tr stmnts'                -> For i tr (remRedun stmnts') : remRedun stmnts
                                While e stmnts'                 -> While e (remRedun stmnts') : remRedun stmnts
                                 -- _                               -> error ("remRedun: " ++ show stmnt)
