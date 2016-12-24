module NBody_hs_26 where

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
-- Generated Haskell from annotated C

result (posTuples,velTuples) =
         let
           vAdd = \((x0,y0,z0),(x1,y1,z1)) -> 
                     let
                       res = (x0 + x1,y0 + y1,z0 + z1)
                     in
                       res

           vSub = \((x0,y0,z0),(x1,y1,z1)) -> 
                     let
                       res = (x0 - x1,y0 - y1,z0 - z1)
                     in
                       res

           sMul = \(a,(x1,y1,z1)) -> 
                     let
                       res = (a * x1,a * y1,a * z1)
                     in
                       res

           dotPr = \((x0,y0,z0),(x1,y1,z1)) -> 
                      let
                        res = x0 * x1 + y0 * y1 + z0 * z1
                      in
                        res

           incrF = \(p,f,q) -> 
                      let
                        d = vSub (q,p)
                        distSqr = dotPr (d,d) + sOFTENING
                        invDist = 1 / sqrt distSqr
                        invDist3 = invDist * invDist * invDist
                        f'0 = vAdd (f,sMul (invDist3,d))
                      in
                        f'0

           calcF = \(pos,p) -> 
                      let
                        f = (0,0,0)
                        f'1 = foldl
                                (\f_0 -> 
                                   \p_1 -> 
                                      let
                                        f_0'0 = incrF (p,f_0,p_1)
                                      in
                                        f_0'0)
                                f
                                pos

                      in
                        f'1

           bodyForce = \(pos) -> 
                          let
                            frc = map
                                    (\p_0 -> 
                                       let
                                         f_1 = calcF (pos,p_0)
                                       in
                                         f_1)
                                    pos

                          in
                            frc

           updV = \(f,v) -> 
                     let
                       v'0 = vAdd (v,sMul (dt,f))
                     in
                       v'0

           updP = \(p,v) -> 
                     let
                       p'0 = vAdd (p,sMul (dt,v))
                     in
                       p'0

           velocities = \(frc,vel) -> 
                           let
                             vel'1 = zipWith
                                       (\f_0 -> 
                                          \v_1 -> 
                                             let
                                               v_1'0 = updV (f_0,v_1)
                                             in
                                               v_1'0)
                                       frc
                                       vel

                           in
                             vel'1

           integrate = \(pos,vel) -> 
                          let
                            pos'1 = zipWith
                                      (\p_0 -> 
                                         \v_1 -> 
                                            let
                                              p_0'0 = updP (p_0,v_1)
                                            in
                                              p_0'0)
                                      pos
                                      vel

                          in
                            pos'1

           timeStep = \(pos,vel) -> 
                         let
                           frc = bodyForce pos
                           vel'0 = velocities (frc,vel)
                           pos'1 = integrate (pos,vel'0)
                         in
                           (pos'1,vel'0)

           next = timeStep (posTuples,velTuples)
         in
           next

