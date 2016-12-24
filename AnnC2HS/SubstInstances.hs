{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module SubstInstances where

import TypesEtc
import Data.List
import Debug.Trace

-- ====================================================================================

class Subst a where
        (<<=)     ::  a  -> (Expr,Expr) -> a
        (<<=:)    ::  a  -> (Expr,Expr) -> a            -- for translating Haskell-to-C ONLY

--        (<<=*)    ::  a  -> [(Expr,Expr)] -> a
--        (<<=*)    = foldl (<<=)


instance Subst Expr where

   e <<= (x,e')  = case (x,e') of

        (Tuple xs, Tuple es)  | length xs == length es -> foldl (<<=) e $ zip xs es
                              | otherwise              -> error "<<=, substitution: tuples of different length"
                                                                -- Careful: ex should not contain y !!
                                                                -- Solution: first rename y in e

        -- (Null, ys)                              -> e
        -- (Cons x xs, Cons ex exs)                -> e <<= (x,ex) <<= (xs,exs)                     -- ibid

        _  -> case e of
                Idf y           | x == Idf y    -> e'
                                | otherwise     -> Idf y
                Const n                         -> Const n
                BinInfix o t t'                 -> BinInfix o (t <<= (x,e')) (t' <<= (x,e'))
                -- Compose o t t'                  -> Compose o (t <<= (x,e')) (t' <<= (x,e'))
                IfE t th_br e_br                -> IfE (t<<=(x,e')) (th_br<<=(x,e')) (e_br<<=(x,e'))
                Tuple ts                        -> Tuple (map (<<=(x,e')) ts)
                List ts                         -> List (map (<<=(x,e')) ts)
                -- Null                            -> Null
                -- Cons y ys                       -> Cons (y <<= (x,e')) (ys <<= (x,e'))
                Lambda v t      | x == v        -> Lambda v t
                                | otherwise     -> Lambda v (t <<= (x,e'))              -- WRONG if e' contains v free !!!
                App f t                         -> App (f <<= (x,e')) (t <<= (x,e'))
                Def v t         | x == v        -> Def v t                              -- WRONG in case of non-rec bindings
                                | otherwise     -> Def v (t <<= (x,e'))
                Let defs t                      -> Let (map (<<=(x,e')) defs) (t <<= (x,e'))    -- WRONG !!!
                LstSel t t'                     -> LstSel (t <<= (x,e')) (t' <<= (x,e'))
                RecSel t t'                     -> RecSel (t <<= (x,e')) t'
                _ -> error $ show e


                {-
                IfE e0 e1 e2                    -> IfE (e0 <<= (x,e')) (e1 <<= (x,e')) (e2 <<= (x,e'))

                Func [] stmnts t                -> Func [] (map (<<=(x,e')) stmnts) (t<<=(x,e'))

                Func (y:ys) stmnts t    | x == y        -> Func (y:ys) stmnts t
                                        | otherwise     -> Func (y:ys) stmnts' t'
                                        where
                                          Func _ stmnts' t' = (Func ys stmnts t) <<= (x,e')

                _                       -> error ("substitution: " ++ show e)
                -}

   e <<=: (x,e') = case (x,e') of

        (Tuple xs, Tuple es)  | length xs == length es -> foldl (<<=:) e $ zip xs es
                              | otherwise              -> error "<<=:, substitution: tuples of different length"
                                                                -- Careful: ex should not contain y !!
                                                                -- Solution: first rename y in e

        -- (Null, ys)                              -> e
        -- (Cons x xs, Cons ex exs)                -> e <<=: (x,ex) <<=: (xs,exs)                     -- ibid

        _  -> case e of
                Const n                         -> Const n
                Idf y           | x == Idf y    -> e'
                                | otherwise     -> Idf y
                Empty                           -> Empty
                -- Compose o t t'                  -> Compose o (t <<=: (x,e')) (t' <<=: (x,e'))
                IfE t th_br e_br                -> IfE (t<<=:(x,e')) (th_br<<=:(x,e')) (e_br<<=:(x,e'))
                Tuple ts                        -> Tuple (map (<<=:(x,e')) ts)
                List ts                         -> List (map (<<=:(x,e')) ts)
                LstSel t t'                     -> LstSel (t <<=: (x,e')) (t' <<=: (x,e'))
                RecSel t t'                     -> RecSel (t <<=: (x,e')) t'
                UpdLst e0 e1 e2                 -> UpdLst (e0 <<=: (x,e')) (e1 <<=: (x,e')) (e2 <<=: (x,e'))
                UpdRec e0 e1 e2                 -> UpdRec (e0 <<=: (x,e')) e1 (e2 <<=: (x,e'))

                BinInfix o t t'                 -> BinInfix o (t <<=: (x,e')) (t' <<=: (x,e'))
                App f t                         -> App (f <<=: (x,e')) (t <<=: (x,e'))
                Lambda v t      | x == v        -> Lambda v t
                                | otherwise     -> Lambda v (t <<=: (x,e'))              -- WRONG if e' contains v free !!!
                Def v t                         -> Def (v <<=: (x,e')) (t <<=: (x,e'))
                Let defs t                      -> Let (map (<<=:(x,e')) defs) (t <<=: (x,e'))    -- WRONG !!!

{-
instance Subst Stmnt where

        stmnt <<= (x, e)   = case stmnt of
                Skip                    -> Skip
                Assign tp x' e'         -> Assign tp (x'<<=(x,e)) (e'<<=(x,e))
                IfS e0 ps0 ps1          -> IfS (e0<<=(x,e)) (map (<<=(x,e)) ps0) (map (<<=(x,e)) ps1)
                Break                   -> Break
                For i (b,n,s) stmnts    -> For (i<<=(x,e)) (b,n,s) $ map (<<=(x,e)) stmnts
                While px stmnts         -> While (px<<=(x,e)) $ map (<<=(x,e)) stmnts


instance Subst [Stmnt] where

        stmnts <<= (x,e)        = map (\s -> s <<= (x,e)) stmnts
-}





