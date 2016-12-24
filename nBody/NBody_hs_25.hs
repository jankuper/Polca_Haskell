module NBody_hs_25 where

import Debug.Trace
import Input




concatWith x []  = []
concatWith x [y] = y
concatWith x (y:ys) = y ++ x ++ concatWith x ys

itn f x 0 = x
itn f x n = itn f (f x) (n-1)

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
         $ map (map (\p -> (x p, y p)))
         $ map fst
         $ itnscan result (pos,vel) n



-- ==============================================================================
-- Generated Haskell from annotated C

f = Vector3 { x=undefined, y=undefined, z=undefined }         -- Sorry, hand made :-)

result (pos,vel) =
         let
           incrF = \(p,f,q) -> 
                      let
                        dx = x q - x p
                        dy = y q - y p
                        dz = z q - z p
                        distSqr = dx * dx + dy * dy + dz * dz + sOFTENING
                        invDist = 1 / sqrt distSqr
                        invDist3 = invDist * invDist * invDist
                        f'0 = f { x = x f + dx * invDist3 }
                        f'1 = f'0 { y = y f'0 + dy * invDist3 }
                        f'2 = f'1 { z = z f'1 + dz * invDist3 }
                      in
                        f'2

           calcF = \(pos,p) -> 
                      let
                        f'1 = f { x = 0 }
                        f'2 = f'1 { y = 0 }
                        f'3 = f'2 { z = 0 }
                        f'4 = foldl
                                (\f_0 -> 
                                   \p_1 -> 
                                      let
                                        f_0'0 = incrF (p,f_0,p_1)
                                      in
                                        f_0'0)
                                f'3
                                pos

                      in
                        f'4

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
                       v'0 = v { x = x v + dt * x f }
                       v'1 = v'0 { y = y v'0 + dt * y f }
                       v'2 = v'1 { z = z v'1 + dt * z f }
                     in
                       v'2

           updP = \(p,v) -> 
                     let
                       p'0 = p { x = x p + dt * x v }
                       p'1 = p'0 { y = y p'0 + dt * y v }
                       p'2 = p'1 { z = z p'1 + dt * z v }
                     in
                       p'2

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

           next = timeStep (pos,vel)
         in
           next

