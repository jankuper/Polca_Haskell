{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module SubstInstances where

import Types
import Data.List
import Debug.Trace

-- ====================================================================================

instance Subst Expr where

   e <<= (r,s)  = case (r,s) of

        (Pair x y, Pair ex ey)                  -> e <<= (x,ex) <<= (y,ey)                       -- Careful: ex should not contain y !!
                                                                                                -- Solution: first rename y in e
        -- (Pair (Idf x) (Idf y), e')              -> e <<= (Idf x, Idf "fst" # e') <<= (Idf y, Idf "snd" # e')

        (Triple x y z, Triple ex ey ez)         -> e <<= (x,ex) <<= (y,ey) <<= (z,ez)            -- Ibid for ex, ey

        -- (Triple (Idf x) (Idf y) (Idf z), e')    -> e <<= (Idf x, Idf "fst3" # e') <<= (Idf y, Idf "snd3" # e') <<= (Idf z, Idf "thd3" # e')

        -- TODO !!!
        -- (Triple (Idf x) (Idf y) (Idf z), a)     -> e <<= (Triple (Idf x) (Idf y) (Idf z) , simplify a)

        (Null, ys)                              -> e
        (Cons x xs, Cons ex exs)                -> e <<= (x,ex) <<= (xs,exs)                     -- ibid

        (x,e')          -> case e of
                                Idf y           | x == Idf y    -> e'
                                                | otherwise     -> Idf y
                                I n                             -> I n
                                F n                             -> F n
                                Numeric o t t'                  -> Numeric o (t <<= (x,e')) (t' <<= (x,e'))
                                Boolean o t t'                  -> Boolean o (t <<= (x,e')) (t' <<= (x,e'))
                                Compose o t t'                  -> Compose o (t <<= (x,e')) (t' <<= (x,e'))
                                Pair   t t'                     -> Pair   (t <<= (x,e')) (t' <<= (x,e'))
                                Triple t t' t''                 -> Triple (t <<= (x,e')) (t' <<= (x,e')) (t'' <<= (x,e'))
                                Null                            -> Null
                                Cons y ys                       -> Cons (y <<= (x,e')) (ys <<= (x,e'))
                                Lambda v t      | x == v        -> Lambda v t
                                                | otherwise     -> Lambda v (t <<= (x,e'))              -- Wrong!!!
                                App f t                         -> App (f <<= (x,e')) (t <<= (x,e'))
                                Def v t         | x == v        -> Def v t
                                                | otherwise     -> Def v (t <<= (x,e'))
                                -- TpDef v t                       -> TpDef v t
                                Let defs t                      -> Let (map (<<=(x,e')) defs) (t <<= (x,e'))    -- Also wrong !!!
                                IndSel t t'                     -> IndSel (t <<= (x,e')) (t' <<= (x,e'))
                                FldSel t t'                     -> FldSel (t <<= (x,e')) t'

                                IfE e0 e1 e2                    -> IfE (e0 <<= (x,e')) (e1 <<= (x,e')) (e2 <<= (x,e'))

                                Func [] stmnts t                -> Func [] (map (<<=(x,e')) stmnts) (t<<=(x,e'))

                                Func (y:ys) stmnts t    | x == y        -> Func (y:ys) stmnts t
                                                        | otherwise     -> Func (y:ys) stmnts' t'
                                                                        where
                                                                          Func _ stmnts' t' = (Func ys stmnts t) <<= (x,e')

                                -- _                               -> error ("substitution: " ++ show e)


instance Subst Stmnt where

        stmnt <<= (x, e)   = case stmnt of
                Skip                    -> Skip
                Assign x' e'            -> Assign (x'<<=(x,e)) (e'<<=(x,e))
                IfS e0 ps0 ps1          -> IfS (e0<<=(x,e)) (map (<<=(x,e)) ps0) (map (<<=(x,e)) ps1)
                Break                   -> Break
                For i (b,n,s) stmnts    -> For (i<<=(x,e)) (b,n,s) $ map (<<=(x,e)) stmnts
                While px stmnts         -> While (px<<=(x,e)) $ map (<<=(x,e)) stmnts


instance Subst [Stmnt] where

        stmnts <<= (x,e)        = map (\s -> s <<= (x,e)) stmnts





-- ====================================================================================
-- Substitution for TYPES -- NO instance of Subst
-- ====================================================================================


-- (<<==*) :: Type -> [(Type,Type)] -> Type
-- (<<==*) = foldl (<<==)

-- ====================================================================================

{-
(<<==) :: Type -> (Type,Type) -> Type

tp <<== (x,t) = case (x,t) of

        (Tuple2 a b, Tuple2 ta tb)              -> tp <<== (a,ta) <<== (b,tb)                       -- Careful: ex should not contain y !!
                                                                                                -- Solution: first rename y in e

        (Tuple3 a b c, Tuple3 ta tb tc)         -> tp <<== (a,ta) <<== (b,tb) <<== (c,tc)            -- Ibid for ex, ey

        (List a , List ta)                      -> tp <<== (a,ta)

        (Array n a, Array m ta)                 | m==n          -> tp <<== (a,ta)
                                                | otherwise     -> error "Type Substitution for arrays goes wrong"

        (Fun a b, Fun ta tb)                    -> tp <<== (a,ta) <<== (b,tb)

        -- (PMFun a b , PMFun ta tb)               -> tp <<== (a,ta) <<== (b,tb)
        -- (PMFun a b , Fun ta tb)                 -> tp <<== (a,ta) <<== (b,tb)

        _  -> case tp of

                TVar y                  |  x == TVar y          -> t
                                        |  otherwise             -> TVar y

                NoType                  -> NoType
                INT                     -> INT
                FLOAT                   -> FLOAT
                BOOL                    -> BOOL

                Tuple2 t0 t1            -> Tuple2 (t0 <<== (x,t)) (t1 <<== (x,t))

                Tuple3 t0 t1 t2         -> Tuple3 (t0 <<== (x,t)) (t1 <<== (x,t)) (t2 <<== (x,t))

                List t0                 -> List (t0 <<== (x,t))

                Array  n t0             -> Array n (t0 <<== (x,t))

                Fun  a b                |  x == a        -> Fun a b                             -- not sufficient to catch all possibilities?
                                        |  otherwise     -> Fun (a <<== (x,t)) (b <<==(x,t))
-}
