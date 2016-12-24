module NBody_hs_23 where

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


result (pos,vel)
       = let
           bodyForce = \(pos,vel) -> 
                          let
                            vel'9 = zipWith
                                      (\p_4 -> 
                                         \v_5 -> 
                                            let
                                              fx = 0
                                              fy = 0
                                              fz = 0
                                              (fx'3,fy'4,fz'5) = foldl
                                                                   (\(f_0,f_1,f_2) -> 
                                                                      \p_3 -> 
                                                                         let
                                                                           dx = x p_3 - x p_4
                                                                           dy = y p_3 - y p_4
                                                                           dz = z p_3 - z p_4
                                                                           distSqr = dx * dx + dy * dy + dz * dz + sOFTENING
                                                                           invDist = 1 / sqrt distSqr
                                                                           invDist3 = invDist * invDist * invDist
                                                                           f_0'0 = f_0 + dx * invDist3
                                                                           f_1'1 = f_1 + dy * invDist3
                                                                           f_2'2 = f_2 + dz * invDist3
                                                                         in
                                                                           (f_0'0,f_1'1,f_2'2))
                                                                   (fx,fy,fz)
                                                                   pos
                                              
                                              v_5'6 = v_5 { x = x v_5 + dt * fx'3 }
                                              v_5'7 = v_5'6 { y = y v_5'6 + dt * fy'4 }
                                              v_5'8 = v_5'7 { z = z v_5'7 + dt * fz'5 }
                                            in
                                              v_5'8)
                                      pos
                                      vel
                            
                          in
                            vel'9
           
           integrate = \(pos,vel) -> 
                          let
                            pos'3 = zipWith
                                      (\p_0 -> 
                                         \v_1 -> 
                                            let
                                              p_0'0 = p_0 { x = x p_0 + x v_1 * dt }
                                              p_0'1 = p_0'0 { y = y p_0'0 + y v_1 * dt }
                                              p_0'2 = p_0'1 { z = z p_0'1 + z v_1 * dt }
                                            in
                                              p_0'2)
                                      pos
                                      vel
                            
                          in
                            pos'3
           
           timeStep = \(pos,vel) -> 
                         let
                           vel'0 = bodyForce (pos,vel)
                           pos'1 = integrate (pos,vel'0)
                         in
                           (pos'1,vel'0)
           
           next = timeStep (pos,vel)
         in
           next
         
