{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module Transformations where

import Types
import SubstInstances
import PrPrInstances
import Data.List
import Debug.Trace


-- ====================================================================================
-- inline
-- ====================================================================================
-- inline x e:
--   - find definition (Def x t) for x en substitute t for x in e
--   - assumptions (possibly too optimistic)::
--       - let-bindings are non-recursive 
--       - all defined variables have different names
--       - no variable captures inside Def- or Lambda-expression
-- ====================================================================================

inline x e = case e of

        Let ds e        | null dsx      -> simplify $ Let (map (inline x) ds) (inline x e)
                        | otherwise     -> simplify $ Let (map (<<=(x',t)) ds') (e<<=(x',t))
                                        where
                                          isDef x (Def y t)    = x==y
                                          (dsx,ds')            = partition (isDef x) ds
                                          Def x' t             = head dsx

        Def y e'                        -> simplify $ Def y (inline x e')
        Lambda y e'                     -> simplify $ Lambda y (inline x $ topLet e')
        App f y                         -> simplify $ App (inline x f) (inline x y)             -- topLet ??
        _                               -> error ("inline missing: " ++ show e)

inlines [] e = e
inlines (x:xs) e = inlines xs $ inline x e

-- ====================================================================================
-- simplify: executes STRUCTURAL evaluation steps
-- ====================================================================================
-- remove map, zipWith ?

simplify e = case e of

    Idf x                                       -> Idf x
    I   x                                       -> I   x
    F   x                                       -> F   x
    B   x                                       -> B   x

    Numeric o e e'                              -> Numeric o (simplify e) (simplify e')
    Boolean o e e'                              -> Boolean o (simplify e) (simplify e')

    Compose o e e'                              -> Compose o (simplify e) (simplify e')

    Pair e0 e1                                  -> Pair   (simplify e0) (simplify e1)
    Triple e0 e1 e2                             -> Triple (simplify e0) (simplify e1) (simplify e2)

    Null                                        -> Null
    Cons x xs                                   -> Cons (simplify x) (simplify xs)

    Def x t                                     -> Def x (simplify t)

    Let []   t                                  -> simplify t
    Let defs t                                  -> Let (map simplify defs) (simplify t)

    IfE e0 e1 e2                                -> IfE (simplify e0) (simplify e1) (simplify e2)

    Lambda x e                                  -> Lambda x (simplify e)                                        -- ?? whnf ??

    App (App (Idf "map") f) (App (Idf "zip") (Pair x y))
                                                -> App (App (App (Idf "zipWith") f) x) y

    App f (Let defs body)                       -> simplify $ Let defs $ f # body
    App f a       -> case f' of

       Lambda x t       -> case x of
                                Idf _           -> simplify $ t <<= (x,a')
                                Pair _ _        -> case a' of
                                                        Pair _ _      -> simplify $ t <<= (x, a')
                                                        _             -> simplify $ t <<= (x, Pair (Idf "fst" # a') (Idf "snd" # a'))
                                Triple _ _ _    -> case a' of
                                                        Triple _ _ _  -> simplify $ t <<= (x,a')
                                                        _             -> simplify $ t <<= (x, Triple (Idf "fst3" # a') (Idf "snd3" # a') (Idf "thd3" # a'))


       -- Lambda x t                               -> simplify $ t <<= (x,a')                                      -- ? Variable capture!!

       Pair g0 g1                               -> simplify $ Pair (g0#a) (g1#a)
       Triple g0 g1 g2                          -> simplify $ Triple (g0#a) (g1#a) (g2#a)

       Compose "." g h                          -> simplify $ g # (h # a) 
       Compose "<." op h                        -> Lambda (Idf "__x__") $ simplify $ op # a # (h # Idf "__x__")
       Compose ".>" h op                        -> Lambda (Idf "__x__") $ simplify $ op # (h # a) # Idf "__x__"

       Idf "zip"  -> case a of
                         Pair Null ys           -> Null
                         Pair xs Null           -> Null
                         Pair (Cons x xs) (Cons y ys)
                                                -> simplify $ Cons (Pair x y) $ Idf "zip" # Pair xs ys
                         _       | a'/=a        -> simplify $ Idf "zip" # a'
                                 | otherwise    -> Idf "zip" # a

       Idf "zip3" -> case a of
                         Triple Null ys zs      -> Null
                         Triple xs Null zs      -> Null
                         Triple xs ys Null      -> Null
                         Triple (Cons x xs) (Cons y ys) (Cons z zs)
                                                -> simplify $ Cons (Triple x y z) $ (Idf "zip3") # (Triple xs ys zs)
                         _       | a'/=a        -> simplify $ Idf "zip3" # a'
                                 | otherwise    -> Idf "zip3" # a

       Idf "fst"  -> case a of
                         Pair x y               -> simplify x
                         _       | a'/=a        -> simplify $ Idf "fst" # a'
                                 | otherwise    -> Idf "fst" # a
       Idf "snd"  -> case a of
                         Pair x y               -> simplify y
                         _       | a'/=a        -> simplify $ Idf "snd" # a'
                                 | otherwise    -> Idf "snd" # a

       Idf "fst3" -> case a of
                         Triple x y z           -> simplify x
                         _       | a'/=a        -> simplify $ Idf "fst3" # a'
                                 | otherwise    -> Idf "fst3" # a
       Idf "snd3" -> case a of
                         Triple x y z           -> simplify y
                         _       | a'/=a        -> simplify $ Idf "snd3" # a'
                                 | otherwise    -> Idf "snd3" # a
       Idf "thd3" -> case a of
                         Triple x y z           -> simplify z
                         _       | a'/=a        -> simplify $ Idf "thd3" # a'
                                 | otherwise    -> Idf "thd3" # a

       _                 | f'/=f || a'/=a       -> simplify $ App f' a'
                         | otherwise            -> App f' a'

       where
         f' = simplify f
         a' = simplify a

    IndSel e' i             -> case e' of
                                        Let defs t                              -> simplify $ Let defs (t!i)
                                        -- App (App (Idf "map") f) xs                   -> simplify $ App f (xs!i)
                                        -- App (App (App (Idf "zipWith") f) xs) ys      -> simplify $ App (App f (xs!i)) (ys!i)

                                        App (Idf "tail") xs                     -> simplify $ xs!(calc $ Numeric "+" i (I 1))
                                        App (Idf "init") xs                     -> simplify $ xs!i

                                        App (Idf "zip") xy                      -> simplify $ Pair ((Idf "fst"#xy)!i) ((Idf "snd"#xy)!i)
                                        App (Idf "zip3") xyz                    -> simplify $ Triple ((Idf "fst3"#xyz)!i) ((Idf "snd3"#xyz)!i) ((Idf "thd3"#xyz)!i)

                                        App (App (Idf "take") n) xs             -> simplify $ xs!(Numeric "+" i n)
                                        App (App (Idf "drop") n) xs             -> simplify $ xs!i

                                        IndSel (App (Idf "transpose") m) j      -> simplify $ (m!i)!j

                                        Null                                    -> error "simplify: Select from empty list"
                                        Cons x xs       | eqNum i 0             -> x
                                                        | otherwise             -> simplify $ IndSel xs $ calc $ Numeric "-" i (I 1)
                                                                                where
                                                                                  eqNum i n = simplify i == I n

                                        otherwise                               -> (simplify e')!(simplify i)

    FldSel e' i             -> case e' of
                                        Let defs t                              -> simplify $ Let defs (FldSel e' i)
                                        -- App (App (Idf "map") f) xs                   -> simplify $ App f (xs!i)
                                        -- App (App (App (Idf "zipWith") f) xs) ys      -> simplify $ App (App f (xs!i)) (ys!i)

                                        otherwise                               -> FldSel (simplify e') i

    _                       -> error ("simplify -- unexpected expression: " ++ show e)

-- ====================================================================================
-- Lambda lifting

lamLift e = case e of

        Idf "concat"                    -> Lambda (Idf "xss") $ (Idf "concat")#(Idf "xss")

        App (Idf "replicate") n         -> Lambda (Idf "a") $ (Idf "replicate")#n#(Idf "a")
        Idf "replicate"                 -> Lambda (Idf "n" ) $ Lambda (Idf "a") $ (Idf "replicate")#(Idf "n")#(Idf "a")

        App (Idf "split") m             -> Lambda (Idf "a") $ (Idf "split")#m#(Idf "a")
        Idf "split"                     -> Lambda (Idf "n" ) $ Lambda (Idf "a") $ (Idf "split")#(Idf "n")#(Idf "a")

        App (Idf "map") f               -> Lambda (Idf "xs") $ (Idf "map")#f#(Idf "xs")
        Idf "map"                       -> Lambda (Idf "f" ) $ Lambda (Idf "xs") $ (Idf "map")#(Idf "f")#(Idf "xs")

        App (App (Idf "zipWith") f) xs  -> Lambda (Idf "ys") $ (Idf "zipWith")#f#xs#(Idf "ys")
        App (Idf "zipWith") f           -> Lambda (Idf "xs") $ Lambda (Idf "ys") $ (Idf "zipWith")#f#(Idf "xs")#(Idf "ys")
        Idf "zipWith"                   -> Lambda (Idf "f" ) $ Lambda (Idf "xs") $ Lambda (Idf "ys") $ (Idf "zipWith")#(Idf "f")#(Idf "xs")#(Idf "ys")

        App (App (Idf "foldl") f) a     -> Lambda (Idf "xs") $ (Idf "foldl")#f#a#(Idf "xs")
        App (Idf "foldl") f             -> Lambda (Idf "a" ) $ Lambda (Idf "xs") $ (Idf "foldl")#f#(Idf "a")#(Idf "xs")
        Idf "foldl"                     -> Lambda (Idf "f" ) $ Lambda (Idf "a") $ Lambda (Idf "xs") $ (Idf "foldl")#(Idf "f")#(Idf "a")#(Idf "xs")

        Pair e0 e1                      -> Pair (lamLift e0) (lamLift e1)

        Triple e0 e1 e2                 -> Triple (lamLift e0) (lamLift e1) (lamLift e2)

        Cons e0 e1                      -> Cons (lamLift e0) (lamLift e1)

        IfE e0 e1 e2                    -> IfE e0 (lamLift e1) (lamLift e2)

        Def x e0                        -> Def x (lamLift e0)

        Let ds e0                       -> Let (map lamLift ds) (lamLift e0)

        Lambda x e0                     -> Lambda x (lamLift e0)

        _                               -> e

-- ====================================================================================
-- Lift let-bindings to top-level

topLet e = case e of
        Idf e0          -> Let [] e
        I   e0          -> Let [] e
        F   e0          -> Let [] e
        B   e0          -> Let [] e
        Null            -> Let [] Null
        Empty           -> Let [] Empty

        Numeric o e0 e1 -> Let (ds0++ds1) $ Numeric o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Boolean o e0 e1 -> Let (ds0++ds1) $ Boolean o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Compose o e0 e1 -> Let (ds0++ds1) $ Compose o e0' e1'           where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Pair e0 e1      -> Let (ds0++ds1) $ Pair e0' e1'                where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        Triple e0 e1 e2 -> Let (ds0++ds1++ds2) $ Triple e0' e1' e2'     where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1
                                                                                Let ds2 e2'     = topLet e2

        Cons e0 e1      -> Let (ds0++ds1) $ Cons e0' e1'                where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1

        IndSel e0 i     -> Let ds $ IndSel e0' i                        where   Let ds e0'      = topLet e0
        FldSel e0 i     -> Let ds $ FldSel e0' i                        where   Let ds e0'      = topLet e0

        IfE e0 e1 e2    -> Let ds0 $ IfE e0' (Let ds1 e1')
                                             (Let ds2 e2')              where   Let ds0 e0'     = topLet e0
                                                                                Let ds1 e1'     = topLet e1
                                                                                Let ds2 e2'     = topLet e2

        App f e0        -> Let (dsf++ds0) $ App f' e0'                  where   Let dsf f'      = topLet f
                                                                                Let ds0 e0'     = topLet e0

        Def x e0        -> Let (ds++[d]) Empty                          where   Let ds e0'      = topLet e0
                                                                                d               = Def x e0'

        Let ds e0       -> Let (ds'++ds0) e0'                           where   ds'             = concat [ dsi | Let dsi Empty <- map topLet ds ]
                                                                                Let ds0 e0'     = topLet e0

        Lambda x es     -> Let [] $ Lambda x $ topLet es

        _               -> error ("topLet: " ++ show e)

-- ====================================================================================
-- Translation from Haskell to C

rename (x,y) e = case e of                -- TOO RADICAL

        Def z e'        | z == x          -> Def y (rename (x,y) e')
                        | otherwise       -> Def z (rename (x,y) e')

        Idf z           | x == Idf z      -> y
                        | otherwise       -> Idf z

        I n                               -> I n
        F n                               -> F n
        B n                               -> B n

        Numeric o e0 e1                   -> Numeric o (rename (x,y) e0) (rename (x,y) e1)
        Boolean o e0 e1                   -> Boolean o (rename (x,y) e0) (rename (x,y) e1)
        Compose o e0 e1                   -> Compose o (rename (x,y) e0) (rename (x,y) e1)

        Pair e0 e1                        -> Pair (rename (x,y) e0) (rename (x,y) e1)
        Triple e0 e1 e2                   -> Triple (rename (x,y) e0) (rename (x,y) e1) (rename (x,y) e2)

        Null                              -> Null
        Cons t ts                         -> Cons (rename (x,y) t) (rename (x,y) ts)

        IndSel e0 i                       -> IndSel (rename (x,y) e0) (rename (x,y) i)
        FldSel e0 i                       -> FldSel (rename (x,y) e0) i
        IfE t e0 e1                       -> IfE (rename (x,y) t) (rename (x,y) e0) (rename (x,y) e1)
        App e0 e1                         -> App (rename (x,y) e0) (rename (x,y) e1)

        Lambda z t      | z == x          -> Lambda z t
                        | otherwise       -> Lambda z (rename (x,y) t)

        Func [] stmnts e0                 -> Func [] stmnts (rename (x,y) e0)             -- <== rename statments

        Func (z:zs) stmnts e0 | z == x    -> Func (z:zs) stmnts e0
                              | otherwise -> Func (z:zs) stmnts e0'
                                          where
                                            Func _ _ e0' = rename (x,y) (Func zs stmnts e0)             -- <== rename statments

{-
        Func z stmnts e0 | z == x         -> Func z stmnts e0
                         | otherwise      -> Func z stmnts (rename (x,y) e0)              -- <== rename statments
-}

        Let ds t                          -> Let (map (rename (x,y)) ds) (rename (x,y) t)

        _                                 -> error ("rename: " ++ show e)

-- ====================================================================================
-- calculate expressions

calc e = case e of

        Idf x                                   -> Idf x
        I   x                                   -> I   x
        F   x                                   -> F   x
        B   x                                   -> B   x

        -- ======================================================================

        Numeric o x y -> case (x,y) of
                (I x', I y')           | o == "/"           -> I (x' `div` y')              -- NO "^" for floating exponent
                                       | o == "^"           -> I (x' ^ y')
                                       | otherwise          -> I (n_oper o x' y')
                (I x', F y')                                -> F (n_oper o (fromIntegral x') y')
                (F x', I y')           | o == "^"           -> F (x' ^ y')
                                       | otherwise          -> F (n_oper o x' (fromIntegral y'))
                (F x', F y')           | o == "/"           -> F (x' / y')
                                       | otherwise          -> F (n_oper o x' y')

                _                      | x'/=x || y'/=y     -> calc $ Numeric o x' y'
                                       | otherwise          -> Numeric o x y
                                       where
                                         (x', y') = (calc x, calc y)

        -- ======================================================================

        Boolean o (B x) (B y)               -> B (b_oper o x y)

        Boolean o x y -> case (x,y) of
                (I x', I y')                    -> B (r_oper o x' y')
                (I x', F y')                    -> B (r_oper o (fromIntegral x') y')
                (F x', I y')                    -> B (r_oper o x' (fromIntegral y'))
                (F x', F y')                    -> B (r_oper o x' y')

                _          | x'/=x || y'/=y     -> calc $ Boolean o x' y'
                           | otherwise          -> Boolean o x y
                                                where
                                                  (x', y') = (calc x, calc y)

        App (Idf "not") (B True)                -> B False
        App (Idf "not") (B False)               -> B True
        App (Idf "not") x  | x' /= x            -> calc $ App (Idf "not") x'
                           | otherwise          -> App (Idf "not") x
                                                where
                                                  x' = calc x

        -- ======================================================================

        IfE (B True)  e0 e1                     -> calc e0
        IfE (B False) e0 e1                     -> calc e1
        IfE t e0 e1        | t' /= t            -> calc $ IfE t' e0 e1
                           | otherwise          -> IfE t (calc e0) (calc e1)
                                                where
                                                  t' = calc t

        -- ======================================================================

        Null                                    -> Null
        Cons x xs                               -> Cons (calc x) (calc xs)

        -- ======================================================================

        _                                       -> e

-- ====================================================================================
-- expand HOFs by unrolling the list -- NEEDS RECONSIDERING. -- Combine with calc?
-- ====================================================================================
expandHOF e = case e of

        Def x y                                                 -> Def x (expandHOF y)

        -- ======================================================================

        App (Idf "head") Null                                   -> error "expandHOF: head applied to Null"
        App (Idf "head") (Cons x xs)                            -> x
        App (Idf "head") xs             | xs'/=xs               -> expandHOF $ Idf "head" # xs'
                                        | otherwise             -> Idf "head" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "tail") Null                                   -> error "expandHOF: tail applied to Null"
        App (Idf "tail") (Cons x xs)                            -> xs
        App (Idf "tail") xs             | xs'/=xs               -> expandHOF $ Idf "tail" # xs'
                                        | otherwise             -> Idf "tail" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "init") Null                                   -> error "expandHOF: init applied to Null"
        App (Idf "init") (Cons x Null)                          -> Null
        App (Idf "init") (Cons x xs)                            -> Cons x $ expandHOF $ Idf "init" # xs
        App (Idf "init") xs             | xs'/=xs               -> expandHOF $ Idf "init" # xs'
                                        | otherwise             -> Idf "init" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "last") Null                                   -> error "expandHOF: last applied to Null"
        App (Idf "last") (Cons x Null)                          -> x
        App (Idf "last") (Cons x xs)                            -> expandHOF $ Idf "last" # xs
        App (Idf "last") xs             | xs'/=xs               -> expandHOF $ Idf "last" # xs'
                                        | otherwise             -> Idf "last" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "take") (I 0)) xs                         -> Null
        App (App (Idf "take") (I n)) Null                       -> Null
        App (App (Idf "take") (I n)) (Cons x xs)                -> Cons x $ expandHOF $ Idf "take" # I (n-1) # xs
        App (App (Idf "take") (I n)) xs       | xs'/=xs         -> expandHOF $ Idf "take" # I n # xs'
                                                | otherwise     -> Idf "take" # I n # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "drop") (I 0)) xs                         -> xs
        App (App (Idf "drop") (I n)) Null                       -> Null
        App (App (Idf "drop") (I n)) (Cons x xs)                -> expandHOF $ Idf "drop" # I (n-1) # xs
        App (App (Idf "drop") (I n)) xs         | xs'/=xs       -> expandHOF $ Idf "drop" # I n # xs'
                                                | otherwise     -> Idf "drop" # I n # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "split") (I n)) Null                      -> Null
        App (App (Idf "split") (I n)) xs                        -> Cons (expandHOF $ Idf "take" # I n # xs')
                                                                        (expandHOF $ Idf "split" # I n # (expandHOF $ Idf "drop" # I n # xs'))
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "replicate") (I 0)) x                     -> Null
        App (App (Idf "replicate") (I n)) x                     -> Cons x (expandHOF $ Idf "replicate" # I (n-1) # x)

        App (Idf "zip") (Pair Null Null)                        -> Null
        App (Idf "zip") (Pair (Cons x xs) (Cons y ys))          -> Cons (Pair x y) $ expandHOF $ Idf "zip" # Pair xs ys
        App (Idf "zip") p               | p'/=p                 -> expandHOF $ Idf "zip" # p'
                                        | otherwise             -> Idf "zip" # p
                                                                where
                                                                  p' = expandHOF p

        App (Idf "unzip") Null                                  -> Pair Null Null
        App (Idf "unzip") (Cons (Pair x y) xys)                 -> Pair (Cons x xs) (Cons y ys)
                                                                where
                                                                  Pair xs ys = expandHOF $ Idf "unzip" # xys
        App (Idf "unzip") xs            | xs'/=xs               -> expandHOF $ Idf "unzip" # xs'
                                        | otherwise             -> Idf "unzip" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "zip3") (Triple Null Null Null)                        -> Null
        App (Idf "zip3") (Triple (Cons x xs) (Cons y ys) (Cons z zs))   -> Cons (Triple x y z) $ expandHOF $ Idf "zip3" # Triple xs ys zs
        App (Idf "zip3") xs             | xs'/=xs               -> expandHOF $ Idf "zip3" # xs'
                                        | otherwise             -> Idf "zip3" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "unzip3") Null                                 -> Triple Null Null Null
        App (Idf "unzip3") (Cons (Triple x y z) xyzs)           -> Triple (Cons x xs) (Cons y ys) (Cons z zs)
                                                                where
                                                                  Triple xs ys zs = expandHOF $ Idf "unzip3" # xyzs
        App (Idf "unzip3") xs           | xs'/=xs               -> expandHOF $ Idf "unzip3" # xs'
                                        | otherwise             -> Idf "unzip3" # xs
                                                                where
                                                                  xs' = expandHOF xs

        App (Idf "transpose") (Cons Null xss)                   -> Null
        App (Idf "transpose") xss                               -> Cons (expandHOF $ Idf "map" # Idf "head" # xss) $ expandHOF $ Idf "transpose" # (expandHOF $ Idf "map" # Idf "tail" # xss)

        -- ======================================================================

        App (App (Idf "map") f) Null                            -> Null
        App (App (Idf "map") f) (Cons x xs)                     -> Cons (expandHOF $ f#x) $ expandHOF $ Idf "map" # f # xs'
                                                                where
                                                                  xs' = expandHOF xs

        App (App (Idf "map") f) xs              | xs'/=xs       -> expandHOF $ (Idf "map") # f # xs'
                                                | otherwise     -> Idf "map" # expandHOF f # xs
                                                                where
                                                                  xs' = expandHOF xs

        -- ======================================================================

        App (App (App (Idf "itn") f) a) (I 0) -> expandHOF $ a
        App (App (App (Idf "itn") f) a) (I n) -> expandHOF $ Idf "itn" # f # (f#a) # I (n-1)

        App (App (App (Idf "itn") f) a) n       | a'/=a || n'/=n        -> expandHOF $ Idf "itn" # f # a' # n'
                                                | otherwise             -> Idf "itn" # expandHOF f # a # n
                                                                        where
                                                                          a' = expandHOF a
                                                                          n' = expandHOF n

        -- ======================================================================

        App (App (App (Idf "itnscan") f) a) (I 0)                     -> Cons a Null
        App (App (App (Idf "itnscan") f) a) (I n)                     -> topLet $ Let [Def (Idf ax) (f#a)]
                                                                                (Cons a $ expandHOF $ Idf "itnscan" # f # Idf ax # I (n-1))
                                                                        where
                                                                          ax = toString a ++ "`"

        App (App (App (Idf "itnscan") f) a) n   | n'/=n                 -> expandHOF $ Idf "itnscan" # f # a # n'
                                                | otherwise             -> Idf "itnscan" # expandHOF f # a # n
                                                                        where
                                                                          n' = expandHOF n

        -- ======================================================================

        App (App (App (Idf "zipWith") f) Null) xs       -> Null
        App (App (App (Idf "zipWith") f) xs) Null       -> Null
        App (App (App (Idf "zipWith") f) (Cons x xs)) (Cons y ys)
                                                        -> Cons ((f#x#y)) $ expandHOF $ (Idf "zipWith")#f#xs#ys

        App (App (App (Idf "zipWith") f) xs) ys | xs'/=xs || ys'/=ys    -> expandHOF $ (Idf "zipWith") # f # xs' # ys'
                                                | otherwise             -> (Idf "zipWith") # expandHOF f # xs # ys
                                                                        where
                                                                          xs' = expandHOF xs
                                                                          ys' = expandHOF ys

        -- ======================================================================

        App (App (App (Idf "foldl") f) a) Null          -> a
        App (App (App (Idf "foldl") f) a) (Cons x xs)   -> expandHOF $ (Idf "foldl") # f # (expandHOF $ f#a#x) # xs'
                                                                        where
                                                                          xs' = expandHOF xs

        App (App (App (Idf "foldl") f) a) xs    | xs'/=xs               -> expandHOF $ (Idf "foldl") # f # a # xs'
                                                | otherwise             -> (Idf "foldl") # expandHOF f # a # xs
                                                                        where
                                                                          xs' = expandHOF xs

        -- ======================================================================

        Pair x y                                        -> Pair (expandHOF x) (expandHOF y)
        Triple x y z                                    -> Triple (expandHOF x) (expandHOF y) (expandHOF z)

        Cons x xs                                       -> Cons (expandHOF x) (expandHOF xs)

        IfE e e1 e2                                     -> IfE (expandHOF e) (expandHOF e1) (expandHOF e2)

        _                                               -> e

-- ====================================================================================


fI       = Idf "f"
gI       = Idf "g"
mI       = Idf "m"
splitI   = Idf "split"
concatI  = Idf "concat"
zipI     = Idf "zip"
mapI     = Idf "map"
foldlI   = Idf "foldl"
zipWithI = Idf "zipWith"


transform (r,i) e = case (r,i) of

        (_,0)   -> e

        -- ======================================================================
        -- LHS: map f xs

        ("m",i)         -> case e of
                                App (App (Idf "map") f) xs

                                 -> case i of
                                        -- RHS: concat $ map (map f) $ split m xs
                                     10 -> concatI # (mapI # (mapI#f) # xssm)
                                     12 -> concatI # (mapI # (mapI#f) # xss2)
                                 where
                                   xssm = splitI # mI # xs
                                   xss2 = Idf "split" # I 2 # xs

        -- ======================================================================
        -- LHS: foldl f a xs

        ("f",i)         -> case e of
                                App (App (App (Idf "foldl") f) a) xs

                                 -> case i of
                                     10 -> foldlI # (foldlI#f) # a # xssm                               -- foldl f a xs ==>>  foldl (foldl f) a (split m xs)
                                     12 -> foldlI # (foldlI#f) # a # xss2
                                     13 -> foldlI # (foldlI#f) # a # xss3
                                     14 -> foldlI # (foldlI#f) # a # xss4

                                     20 -> foldlI # f # a # (mapI # (foldlI#f#a) # xssm)                --              ==>>  foldl f a $ map (foldl f a) (split m xs)
                                     22 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss2)
                                     23 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss3)
                                     24 -> foldlI # f # a # (mapI # (foldlI#f#a) # xss4)

                                     30 -> foldlI # f # a # (foldlI# (zipWithI#f) # asm # xssm)         --              ==>>  foldl f a $ foldl (zipWith f) (replicate m a) (split m xs)
                                     32 -> foldlI # f # a # (foldlI# (zipWithI#f) # as2 # xss2)
                                     33 -> foldlI # f # a # (foldlI# (zipWithI#f) # as3 # xss3)
                                     34 -> foldlI # f # a # (foldlI# (zipWithI#f) # as4 # xss4)

                                 where
                                   xssm = Idf "split" # mI # xs
                                   xss2 = Idf "split" # I 2 # xs
                                   xss3 = Idf "split" # I 3 # xs
                                   xss4 = Idf "split" # I 4 # xs

                                   asm  = Idf "replicate" # mI # a
                                   as2  = Idf "replicate" # I 2 # a
                                   as3  = Idf "replicate" # I 3 # a
                                   as4  = Idf "replicate" # I 4 # a

                                _ -> error ("transform: " ++ (show $ toString e))

        -- ======================================================================
        -- LHS: map f (map g xs)

        ("mm",1)        -> case e of
                                App (App (Idf "map") f) (App (App (Idf "map") g) xs)
                                        -> mapI # (Compose "." f g) # xs                                -- RHS: map (f.g) xs

        -- ======================================================================
        -- LHS: zipWith ((f .> g) .> h)  (zip(xs,ys)) zs

        ("mzWL2",i)     -> case e of
                                -- App (App (App (Idf "zipWith") (Compose ".>" f o)) xs) ys
                                -- App (App (App (Idf "zipWith") (Lambda x (Lambda y (App (App o e') y')))) xs) ys

                                --   -> case i of
                                --         -- RHS: zipWith (\v y -> o v (g y)) (map f xs) ys
                                --         1 -> traceShow e' 
                                --              App (App (App (Idf "zipWith") o)   (App (App (Idf "map") e') xs))  ys
                                --       -- 1 -> zipWithI # (Lambda v $ Lambda y $ o#v#(g#y)) # (mapI#f#xs) # ys


                                App
                                  (App
                                     (App
                                        (Idf "zipWith")
                                        (Lambda x
                                                (Lambda y
                                                        (App
                                                           (App
                                                              h
                                                              (Lambda z
                                                                      (App
                                                                         (App
                                                                            g
                                                                            (App
                                                                               f
                                                                               x'))
                                                                         z')))
                                                            y'))))
                                     (App (Idf "zip") (Pair ps vs)))
                                  ps'
                                  -> case i of
                                        -- RHS: zipWith (\v y -> o v (g y)) (map f xs) ys
                                        1 -> zipWithI # h # (zipWithI # g # (mapI # f # ps) # vs) # ps'
                                        -- 1 -> App (App (App (Idf "zipWith") h)   (App (App (Idf "map") e') xs))  ys

                                _ -> error $ "*****>>>>  " ++ show e

        -- ======================================================================
        -- LHS: zipWith (f .> g)  xs ys

        ("mzW1",i)     -> case e of
                                -- App (App (App (Idf "zipWith") (Compose ".>" f o)) xs) ys
                                -- App (App (App (Idf "zipWith") (Lambda x (Lambda y (App (App o e') y')))) xs) ys

                                --   -> case i of
                                --         -- RHS: zipWith (\v y -> o v (g y)) (map f xs) ys
                                --         1 -> traceShow e' 
                                --              App (App (App (Idf "zipWith") o)   (App (App (Idf "map") e') xs))  ys
                                --       -- 1 -> zipWithI # (Lambda v $ Lambda y $ o#v#(g#y)) # (mapI#f#xs) # ys

                                App
                                  (App
                                     (App
                                        (Idf "zipWith")
                                        (Lambda x
                                                (Lambda y
                                                        (App
                                                           (App
                                                              g
                                                              (App
                                                                 f
                                                                 x'))
                                                           y'))))
                                      xs)
                                  ys
                                  -> case i of
                                        -- RHS: zipWith g (map f xs) ys
                                        1 -> zipWithI # g # (mapI # f # xs) # ys


                                _ -> error $ "*****>>>>  " ++ show e



        -- ======================================================================
        -- LHS: zipWith g (map f xs) ys
        ("mzWL",i)     -> case e of
                                App (App (App (Idf "zipWith") g)   (App (App (Idf "map") f) xs))  ys
                                  -> case i of
                                        -- RHS: zipWith (f .> g) xs ys
                                      1 -> zipWithI # (Compose ".>" f g) # xs # ys

        -- LHS: zipWith g xs (map f ys)
        ("mzWR",i)     -> case e of
                                App (App (App (Idf "zipWith") g) xs) (App (App (Idf "map") f) ys)
                                  -> case i of
                                        -- RHS: zipWith (g <. f) xs ys
                                      1 -> zipWithI # (Compose "<." g f) # xs # ys
        -- ======================================================================



        -- ======================================================================
        -- LHS: zipWith o (map f xs) (map g ys)

        ("mmzW",i)      -> case e of
                                App (App (App (Idf "zipWith") o) (App (App (Idf "map") f) xs)) (App (App (Idf "map") g) ys)
                                  -> case i of
                                        -- RHS: zipWith (\v y -> o v (g y)) (map f xs) ys
                                      1 -> zipWithI # (Lambda v $ Lambda y $ o#v#(g#y)) # (mapI#f#xs) # ys

                                        -- RHS: zipWith (\x v -> o (f x) v) xs (map g ys)
                                      2 -> zipWithI # (Lambda x $ Lambda v $ o#(f#x)#v) # xs # (mapI#g#ys)

                                        -- RHS: map (\x -> o (f x) (g x)) xs
                                      3 -> mapI # (Lambda x $ o#(f#x)#(g#x)) # xs
                        where
                          (v,x,y) = (Idf"v", Idf "x", Idf "y")

        -- ======================================================================
        -- LHS: foldl f a $ map g xs

        ("fm",i)        -> case e of
                                App (App (App (Idf "foldl") f) a) (App (App (Idf "map") g) xs)

                                 -> case i of
                                        -- RHS: foldl (f <. g x) a xs
                                      1 -> foldlI # (Compose "<." f g) # a # xs

                                      2 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Idf "x") $
                                                                f#(Idf "a")#(g#(Idf "x"))
                                                  ) # a # xs

                                        -- RHS: foldl (\a x -> let y = g x in f a y) a xs
                                      3 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Idf "x") $
                                                                Let [Def (Idf "y") (g#(Idf"x"))]
                                                                    f#(Idf "a")#(Idf "y")
                                                  ) # a # xs

        -- ======================================================================
        -- LHS: foldl f a $ zipWith g xs ys

        ("fzW",i)       -> case e of
                                App (App (App (Idf "foldl") f) a) (App (App (App (Idf "zipWith") g) xs) ys)

                                 -> case i of
                                        -- RHS: foldl ((f<.g) a (x,y)         f a (g x y)) a (zip xs ys)
                                      1 -> foldlI # (Compose "<." f g) # a # (zipI#xs#ys)

                                        -- RHS: foldl (\a (x,y) -> f a (g x y)) a (zip xs ys)
                                      2 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Pair (Idf "x") (Idf "y")) $
                                                                f#(Idf "a")#(g#(Idf "x")#(Idf "y"))
                                                  ) # a # (zipI#xs#ys)

                                        -- RHS: foldl (\a (x,y) -> let u = g x y in f a u) a (zip xs ys)
                                      3 -> foldlI # (Lambda (Idf "a") $
                                                        Lambda (Pair (Idf "x") (Idf "y")) $
                                                                Let [Def (Idf "u") (g#(Idf "x")#(Idf "y"))]
                                                                    f#(Idf "a")#(Idf "u")
                                                  ) # a # (zipI#xs#ys)

        -- ======================================================================



