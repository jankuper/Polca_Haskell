module Main where

-- ==========================================================================
-- Jan Kuper, 2015-2016
-- License: BSD2
-- ==========================================================================

import GHC.Generics
import FPPrac.Trees
import TypesEtc
import PolcaAlphabet
import PolcaGrammar
import Tokenizer hiding (C)
import ParserGen
import PT2AST
import AnnC2HS
import SubstInstances
import Transformations
import PrPr

-- ==========================================================================
-- show functions
--      These functions show various levels of analysis of annotated C input.
--      Call: any of these functions PLUS the number of one of the programs below.
--      Example: hs_code 24
-- ==========================================================================

-- parse input string nr i:
str2PTree      i = parse polcaGrammar P_Program $ tokenize fsa $ prog i

-- yield a pretty printed parse tree, and the corresponding AST:
prpr_PTree     i = prpr Any $ str2PTree i
prpr_inp_AST   i = prpr Any $ toRoseTree $ pt2inpAST $ str2PTree i

-- yield the finetuned Haskell AST:
hs_AST         i = snd $ uniquevars 0 [] $ sel2vars $ head $ snd $ annC2hs 0 $ pt2inpAST $ str2PTree i
prpr_hs_AST    i = prpr Any $ toRoseTree $ hs_AST i

-- yield the human readable haskell code:
hs_code        i = prpr HS $ addBrackets $ snd $ uniquevars 0 [] $ hs_AST i

-- ==========================================================================
-- Examples
-- ==========================================================================
prog i = case i of


-- ==========================================================================
-- simple example: sum
-- ==========================================================================
  11     -> unlines [ "#pragma polca output sum;"
                   , "sum=0;"
                   , "#pragma polca func foldl SUM sum as sum;"
                   , "for (i=0; i<n; i++) {"
                   , "  #pragma polca def SUM;"
                   , "  #pragma polca input sum as[i];"
                   , "  #pragma polca output sum;"
                   , "  sum = sum + as[i];"
                   , "  }"
                   ]

-- ==========================================================================
-- N-Body
--  21: Polca-initial; only velocity updte; original formulation;
--  22: Polca-initial; only velocity update; vector level;
--  23: Polca-initial; complete;
--  24: Polca-adap; complete;
--  25: Functions extracted from 24;
--  26: Same as 25, reformulated using tuples;
-- ==========================================================================
-- ==========================================================================
-- 21: Polca-initial; only velocity updte; original formulation;

  21    -> unlines [ "#pragma polca output vel;"

                   , "#pragma polca func zipWith UpdV pos vel vel;"
                   , "for (i = 0; i < n; i++) {"
                   , "  #pragma polca def UpdV;"
                   , "  #pragma polca input pos[i] vel[i];"
                   , "  #pragma polca output vel[i];"
                   , "  Fx = 0;"
                   , "  Fy = 0;"
                   , "  Fz = 0;"

                   , "  #pragma polca func foldl AddF (Fx,Fy,Fz) pos (Fx,Fy,Fz);"
                   , "  for (j = 0; j < n; j++) {"
                   , "    #pragma polca def AddF;"
                   , "    #pragma polca input (Fx,Fy,Fz) pos[j];"
                   , "    #pragma polca output (Fx,Fy,Fz);"
                   , "    dx = pos[j].x - pos[i].x;"
                   , "    dy = pos[j].y - pos[i].y;"
                   , "    dz = pos[j].z - pos[i].z;"
                   , "    distSqr  = dx*dx + dy*dy + dz*dz + SOFTENING;"
                   , "    invDist  = 1 / sqrt(distSqr);"
                   , "    invDist3 = invDist * invDist * invDist;"

                   , "    Fx = Fx + dx * invDist3;"
                   , "    Fy = Fy + dy * invDist3;"
                   , "    Fz = Fz + dz * invDist3;"
                   , "    }"
                   , "  vel[i].x = vel[i].x + dt*Fx;"
                   , "  vel[i].y = vel[i].y + dt*Fy;"
                   , "  vel[i].z = vel[i].z + dt*Fz;"
                   , "  }"
                   ]

-- ==========================================================================
-- ==========================================================================
-- 22: Polca-initial; only velocity update; vector level;

  22    -> unlines [ "#pragma polca output vel;"

                   , "#pragma polca func zipWith UpdV pos vel vel;"               -- [0..9] x"
                   , "for (i=0; i<n; i++) {"
                   , "  #pragma polca def UpdV;"
                   , "  #pragma polca input pos[i] vel[i];"
                   , "  #pragma polca output vel[i];"
                   , "  F = (0,0,0);"

                   , "  #pragma polca func foldl ADDFORCE F pos F;"
                   , "  for (j = 0; j < n; j++) {"
                   , "    #pragma polca def ADDFORCE;"
                   , "    #pragma polca input F pos[j];"
                   , "    #pragma polca output F;"
                   , "    d = pos[j] - pos[i];"
                   , "    distSqr = dotpr(d,d) + SOFTENING;"
                   , "    invDist = 1 / sqrt(distSqr);"
                   , "    invDist3 = invDist * invDist * invDist;"

                   , "    F = F + sProd(invDist3,d);"
                   , "    }"
                   , "  vel[i] = vel[i] + sProd(dt,F);"
                   , "  }"
                   ]



-- ==========================================================================
-- ==========================================================================
-- 23: Polca-initial; complete;

  23    -> unlines [ ""
                   , "#pragma polca output next;"
                   , ""

                   -- function def
                   , "fundef bodyForce (pos, vel) {"          -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdV pos vel vel;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def UpdV;"
                   , "    #pragma polca input pos[i] vel[i];"
                   , "    #pragma polca output vel[i];"
                   , "    Fx = 0;"
                   , "    Fy = 0;"
                   , "    Fz = 0;"
                   , ""

                   , "    #pragma polca func foldl AddF (Fx,Fy,Fz) pos (Fx,Fy,Fz);"
                   , "    for (j = 0; j < n; j++) {"
                   , "      #pragma polca def AddF;"
                   , "      #pragma polca input (Fx,Fy,Fz) pos[j];"
                   , "      #pragma polca output (Fx,Fy,Fz);"
                   , "      dx = pos[j].x - pos[i].x;"
                   , "      dy = pos[j].y - pos[i].y;"
                   , "      dz = pos[j].z - pos[i].z;"
                   , "      distSqr  = dx*dx + dy*dy + dz*dz + SOFTENING;"
                   , "      invDist  = 1 / sqrt(distSqr);"
                   , "      invDist3 = invDist * invDist * invDist;"

                   , "      Fx = Fx + dx * invDist3;"
                   , "      Fy = Fy + dy * invDist3;"
                   , "      Fz = Fz + dz * invDist3;"
                   , "      }"
                   , "    vel[i].x = vel[i].x + dt*Fx;"
                   , "    vel[i].y = vel[i].y + dt*Fy;"
                   , "    vel[i].z = vel[i].z + dt*Fz;"
                   , "    }"
                   , "  return vel;"
                   , "}"
                   , ""

                   --function def
                   , "fundef integrate (pos, vel) {"                   -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdP pos vel pos;"
                   , "  for (i = 0 ; i < n; i++) {"
                   , "    #pragma polca def UpdP;"
                   , "    #pragma polca input pos[i] vel[i];"
                   , "    #pragma polca output pos[i];"
                   , "    pos[i].x = pos[i].x + vel[i].x*dt;"
                   , "    pos[i].y = pos[i].y + vel[i].y*dt;"
                   , "    pos[i].z = pos[i].z + vel[i].z*dt;"
                   , "  }"
                   , "  return pos;"
                   , "}"
                   , ""

                   -- function def: nextStep
                   , "fundef timeStep (pos, vel) {"                 -- SKIP: dt, n
                   , "  vel = bodyForce (pos, vel);"
                   , "  pos = integrate  (pos, vel);"
                   , "  return (pos,vel);"
                   , "}"
                   , ""

                   -- main program
                   , "next = timeStep (pos,vel);"
                   ]


-- ==========================================================================
-- ==========================================================================
-- 24: Polca-adap; complete;

  24    -> unlines [ ""
                   , "#pragma polca output next;"
                   , ""
  
                   , "fundef bodyForce (pos) {"            -- SKIP: n
                   , "  #pragma polca func map CalcF pos frc;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def CalcF;"
                   , "    #pragma polca input pos[i];"
                   , "    #pragma polca output frc[i];"
                   , "    frc[i].x = 0;"
                   , "    frc[i].y = 0;"
                   , "    frc[i].z = 0;"
                   , ""

                   , "    #pragma polca func foldl AddF frc[i] pos (frc[i]);"
                   , "    for (j = 0; j < n; j++) {"
                   , "      #pragma polca def AddF;"
                   , "      #pragma polca input frc[i] pos[j];"
                   , "      #pragma polca output frc[i];"
                   , "      dx = pos[j].x - pos[i].x;"
                   , "      dy = pos[j].y - pos[i].y;"
                   , "      dz = pos[j].z - pos[i].z;"
                   , "      distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;"
                   , "      invDist = 1 / sqrt(distSqr);"
                   , "      invDist3 = invDist * invDist * invDist;"

                   , "      frc[i].x = frc[i].x + dx * invDist3;"
                   , "      frc[i].y = frc[i].y + dy * invDist3;"
                   , "      frc[i].z = frc[i].z + dz * invDist3;"
                   , "    }"
                   , "  }"
                   , "  return frc;"
                   , "}"
                   , ""

                   -- function def
                   , "fundef velocities (frc, vel) {"               -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdV vel frc vel;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def UpdV;"
                   , "    #pragma polca input vel[i] frc[i];"
                   , "    #pragma polca output vel[i];"
                   , "    vel[i].x = vel[i].x + dt*frc[i].x;"
                   , "    vel[i].y = vel[i].y + dt*frc[i].y;"
                   , "    vel[i].z = vel[i].z + dt*frc[i].z;"
                   , "  }"
                   , "  return vel;"
                   , "}"
                   , ""

                   -- function def
                   , "fundef integrate(pos, vel) {"                 -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdP pos vel pos;"
                   , "  for (i = 0 ; i < n; i++) {"
                   , "    #pragma polca def UpdP;"
                   , "    #pragma polca input pos[i] vel[i];"
                   , "    #pragma polca output pos[i];"
                   , "    pos[i].x = pos[i].x + dt * vel[i].x;"
                   , "    pos[i].y = pos[i].y + dt * vel[i].y;"
                   , "    pos[i].z = pos[i].z + dt * vel[i].z;"
                   , "  }"
                   , "  return pos;"
                   , "}"
                   , ""

                   -- function def: nextStep
                   , "fundef timeStep (pos, vel) {"                 -- SKIP: dt, n
                   , "  frc = bodyForce  (pos);"
                   , "  vel = velocities (frc, vel);"
                   , "  pos = integrate  (pos, vel);"
                   , "  return (pos,vel);"
                   , "}"
                   , ""

                   -- main program
                   , "next = timeStep (pos,vel);"
                   , ""
                   ]

-- ==========================================================================
-- ==========================================================================
-- 25: Nr 24 with functions extracted

  25    -> unlines [ ""
                   , "#pragma polca output next;"
                   , ""

                   -- function def: incrF
                   , "fundef incrF (p,f,q) {" 
                   , "  dx = q.x - p.x;"
                   , "  dy = q.y - p.y;"
                   , "  dz = q.z - p.z;"
                   , "  distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;"
                   , "  invDist = 1 / sqrt(distSqr);"
                   , "  invDist3 = invDist * invDist * invDist;"
                   , "  f.x = f.x + dx * invDist3;"
                   , "  f.y = f.y + dy * invDist3;"
                   , "  f.z = f.z + dz * invDist3;"
                   , "  return f;"
                   , "}"
                   , ""

                   -- function def: calcF
                   , "fundef calcF (pos,p) {"            -- SKIP: n
                   , "  f.x = 0;"
                   , "  f.y = 0;"
                   , "  f.z = 0;"
                   , "  #pragma polca func foldl AddF f pos f;"
                   , "  for (j = 0; j < n; j++) {"
                   , "    #pragma polca def AddF;"
                   , "    #pragma polca input f pos[j];"
                   , "    #pragma polca output f;"
                   , "    f = incrF (p,f,pos[j]);"
                   , "  }"
                   , "  return f;"
                   , "}"
                   , ""

                   -- function def: bodyForce
                   , "fundef bodyForce (pos) {"            -- SKIP: n
                   , "  #pragma polca func map CalcF pos frc;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def CalcF;"
                   , "    #pragma polca input pos[i];"
                   , "    #pragma polca output frc[i];"
                   , "    frc[i] = calcF (pos,pos[i]);"
                   , "    }"
                   , "  return frc;"
                   , "}"
                   , ""

                   -- function def: updV
                   , "fundef updV (f,v) {"               -- SKIP: dt, n
                   , "  v.x = v.x + dt*f.x;"
                   , "  v.y = v.y + dt*f.y;"
                   , "  v.z = v.z + dt*f.z;"
                   , "  return v;"
                   , "}"
                   , ""

                   -- function def: updP
                   , "fundef updP (p, v) {"               -- SKIP: dt, n
                   , "  p.x = p.x + dt*v.x;"
                   , "  p.y = p.y + dt*v.y;"
                   , "  p.z = p.z + dt*v.z;"
                   , "  return p;"
                   , "}"
                   , ""

                   -- function def: velocities
                   , "fundef velocities (frc,vel) {"               -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdV frc vel vel;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def UpdV;"
                   , "    #pragma polca input frc[i] vel[i];"
                   , "    #pragma polca output vel[i];"
                   , "    vel[i] = updV (frc[i],vel[i]);"
                   , "  }"
                   , "  return vel;"
                   , "}"
                   , ""

                   -- function def: integrate
                   , "fundef integrate(pos, vel) {"                 -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdP pos vel pos;"
                   , "  for (i = 0 ; i < n; i++) {"
                   , "    #pragma polca def UpdP;"
                   , "    #pragma polca input pos[i] vel[i];"
                   , "    #pragma polca output pos[i];"
                   , "    pos[i] = updP (pos[i],vel[i]);"
                   , "  }"
                   , "  return pos;"
                   , "}"
                   , ""

                   -- function def: nextStep
                   , "fundef timeStep (pos, vel) {"                 -- SKIP: dt, n
                   , "  frc = bodyForce  (pos);"
                   , "  vel = velocities (frc, vel);"
                   , "  pos = integrate  (pos, vel);"
                   , "  return (pos,vel);"
                   , "}"
                   , ""

                   -- main program
                   , "next = timeStep (pos,vel);"
                   , ""
                   ]


-- ==========================================================================
-- ==========================================================================
-- 26: Nr 25 with tupels

  26    -> unlines [ ""
                   , "#pragma polca output next;"
                   , ""

                   -- function def: vAdd
                   , "fundef vAdd ((x0,y0,z0),(x1,y1,z1)) {" 
                   , "  res = (x0+x1, y0+y1, z0+z1);"
                   , "  return res;"
                   , "}"
                   , ""

                   -- function def: vSub
                   , "fundef vSub ((x0,y0,z0),(x1,y1,z1)) {" 
                   , "  res = (x0-x1, y0-y1, z0-z1);"
                   , "  return res;"
                   , "}"
                   , ""

                   -- function def: sMul
                   , "fundef sMul (a,(x1,y1,z1)) {" 
                   , "  res = (a*x1, a*y1, a*z1);"
                   , "  return res;"
                   , "}"
                   , ""

                   -- function def: dotPr
                   , "fundef dotPr ((x0,y0,z0),(x1,y1,z1)) {" 
                   , "  res = x0*x1 + y0*y1 + z0*z1;"
                   , "  return res;"
                   , "}"
                   , ""

                   -- function def: incrF
                   , "fundef incrF (p,f,q) {" 
                   , "  d = vSub (q,p);"
                   , "  distSqr = dotPr (d,d) + SOFTENING;"
                   , "  invDist = 1 / sqrt(distSqr);"
                   , "  invDist3 = invDist * invDist * invDist;"
                   , "  f = vAdd (f, sMul(invDist3,d));"
                   , "  return f;"
                   , "}"
                   , ""

                   -- function def: calcF
                   , "fundef calcF (pos,p) {"            -- SKIP: n
                   , "  f = (0,0,0);"
                   , "  #pragma polca func foldl AddF f pos f;"
                   , "  for (j = 0; j < n; j++) {"
                   , "    #pragma polca def AddF;"
                   , "    #pragma polca input f pos[j];"
                   , "    #pragma polca output f;"
                   , "    f = incrF (p,f,pos[j]);"
                   , "  }"
                   , "  return f;"
                   , "}"
                   , ""

                   -- function def: bodyForce
                   , "fundef bodyForce (pos) {"            -- SKIP: n
                   , "  #pragma polca func map CalcF pos frc;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def CalcF;"
                   , "    #pragma polca input pos[i];"
                   , "    #pragma polca output frc[i];"
                   , "    frc[i] = calcF (pos,pos[i]);"
                   , "    }"
                   , "  return frc;"
                   , "}"
                   , ""

                   -- function def: updV
                   , "fundef updV (f,v) {"               -- SKIP: dt, n
                   , "  v = vAdd (v, sMul(dt,f));"
                   , "  return v;"
                   , "}"
                   , ""

                   -- function def: updP
                   , "fundef updP (p, v) {"               -- SKIP: dt, n
                   , "  p = vAdd (p, sMul(dt,v));"
                   , "  return p;"
                   , "}"
                   , ""

                   -- function def: velocities
                   , "fundef velocities (frc,vel) {"               -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdV frc vel vel;"
                   , "  for (i = 0; i < n; i++) {"
                   , "    #pragma polca def UpdV;"
                   , "    #pragma polca input frc[i] vel[i];"
                   , "    #pragma polca output vel[i];"
                   , "    vel[i] = updV (frc[i],vel[i]);"
                   , "  }"
                   , "  return vel;"
                   , "}"
                   , ""

                   -- function def: integrate
                   , "fundef integrate(pos, vel) {"                 -- SKIP: dt, n
                   , "  #pragma polca func zipWith UpdP pos vel pos;"
                   , "  for (i = 0 ; i < n; i++) {"
                   , "    #pragma polca def UpdP;"
                   , "    #pragma polca input pos[i] vel[i];"
                   , "    #pragma polca output pos[i];"
                   , "    pos[i] = updP (pos[i],vel[i]);"
                   , "  }"
                   , "  return pos;"
                   , "}"
                   , ""

                   -- function def: nextStep
                   , "fundef timeStep (pos, vel) {"                 -- SKIP: dt, n
                   , "  frc = bodyForce  (pos);"
                   , "  vel = velocities (frc, vel);"
                   , "  pos = integrate  (pos, vel);"
                   , "  return (pos,vel);"
                   , "}"
                   , ""

                   -- main program
                   , "next = timeStep (posTuples,velTuples);"
                   , ""
                   ]

