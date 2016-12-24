module NBody_hs_26_modified where

import Debug.Trace
import Input

concatWith x []  = []
concatWith x [y] = y
concatWith x (y:ys) = y ++ x ++ concatWith x ys

itnscan f x 0 = [x]
itnscan f x n = x : itnscan f (f x) (n-1)

{-
data Vector3 = Vector3 { x :: Float
                       , y :: Float
                       , z :: Float
                       }
                       deriving (Show,Eq)
-}

sOFTENING = 0.001 :: Float
dt = 0.04

xyCrds n = -- writeFile "xyCrds.py"
           putStr
         $ (++ "\n    ]\n\n")
         $ ("\nxyCrds = [\n" ++)
         $ concatWith ",\n"
         $ map ("    "++)
         $ map show
         $ map (map (\(a,b,c) -> (a,b)))
         $ map fst
         $ itnscan result (posTuples,velTuples) n

-- ==============================================================================
-- Modified Haskell from code generated from annotated C

result (posTuples,velTuples) =
         let
           vAdd (x0,y0,z0) (x1,y1,z1)  = (x0+x1, y0+y1, z0+z1)

           vSub (x0,y0,z0) (x1,y1,z1)  = (x0-x1, y0-y1, z0-z1)

           sMul a (x1,y1,z1)           = ( a*x1,  a*y1,  a*z1)

           dotPr (x0,y0,z0) (x1,y1,z1) = x0*x1 + y0*y1 + z0*z1

           incrF p f q = let
                           d        = vSub q p
                           distSqr  = dotPr d d + sOFTENING
                           invDist  = 1 / sqrt distSqr
                           invDist3 = invDist * invDist * invDist
                         in
                           vAdd f $ sMul invDist3 d

           calcF pos p = foldl (incrF p) (0,0,0) pos

           updV f v    = vAdd v $ sMul dt f

           updP p v    = vAdd p $ sMul dt v

           timeStep (pos,vel) = let
                                  frc   = map (calcF pos) pos                   -- bodyForce  = map (calcF pos)
                                  vel'0 = zipWith updV frc vel                  -- velocities = zipWith updV
                                  pos'1 = zipWith updP pos vel'0                -- integrate  = zipWith updP
                                in
                                  (pos'1,vel'0)
         in
           timeStep (posTuples,velTuples)

