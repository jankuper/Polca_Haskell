module NBody_hs_24 where

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

f_3 = Vector3 { x=undefined, y=undefined, z=undefined }         -- Sorry, hand made :-)

result (pos,vel) =
         let
           bodyForce = \(pos) -> 
                          let
                            frc = map
                                    (\p_2 -> 
                                       let
                                         f_3'3 = f_3 { x = 0 }
                                         f_3'4 = f_3'3 { y = 0 }
                                         f_3'5 = f_3'4 { z = 0 }
                                         f_3'6 = foldl
                                                   (\f_0 -> 
                                                      \p_1 -> 
                                                         let
                                                           dx = x p_1 - x p_2
                                                           dy = y p_1 - y p_2
                                                           dz = z p_1 - z p_2
                                                           distSqr = dx * dx + dy * dy + dz * dz + sOFTENING
                                                           invDist = 1 / sqrt distSqr
                                                           invDist3 = invDist * invDist * invDist
                                                           f_0'0 = f_0 { x = x f_0 + dx * invDist3 }
                                                           f_0'1 = f_0'0 { y = y f_0'0 + dy * invDist3 }
                                                           f_0'2 = f_0'1 { z = z f_0'1 + dz * invDist3 }
                                                         in
                                                           f_0'2)
                                                   f_3'5
                                                   pos

                                       in
                                         f_3'6)
                                    pos

                          in
                            frc

           velocities = \(frc,vel) -> 
                           let
                             vel'3 = zipWith
                                       (\v_0 -> 
                                          \f_1 -> 
                                             let
                                               v_0'0 = v_0 { x = x v_0 + dt * x f_1 }
                                               v_0'1 = v_0'0 { y = y v_0'0 + dt * y f_1 }
                                               v_0'2 = v_0'1 { z = z v_0'1 + dt * z f_1 }
                                             in
                                               v_0'2)
                                       vel
                                       frc

                           in
                             vel'3

           integrate = \(pos,vel) -> 
                          let
                            pos'3 = zipWith
                                      (\p_0 -> 
                                         \v_1 -> 
                                            let
                                              p_0'0 = p_0 { x = x p_0 + dt * x v_1 }
                                              p_0'1 = p_0'0 { y = y p_0'0 + dt * y v_1 }
                                              p_0'2 = p_0'1 { z = z p_0'1 + dt * z v_1 }
                                            in
                                              p_0'2)
                                      pos
                                      vel

                          in
                            pos'3

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

