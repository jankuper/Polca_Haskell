{-# LANGUAGE RecordWildCards #-}

module Main where

import FPPrac.Trees
import Data.List
import Debug.Trace
import Types
import SubstInstances
import PrPrInstances
import Parse
import Transformations
import C_Generation

-- ============================================================================================

c_code inl (r,i) k = prpr                                    -- including INLINE parameter
-- c_code (r,i) k  = prpr
                $ remRedun
                $ snd
                $ hs2c 0
                $ (\e -> Def (Idf "Z") e)                   -- TODO !!
                $ topLet
                $ calc
                $ simplify
                $ snd
                $ extrHOF True 0
                $ simplify
                $ lamLift
                $ simplify
                $ transform (r,i)
                $ simplify
                $ inlines inl
                $ pt2expr
                $ parse
                $ example k

-- hs_code inl (r,i) str =                                      -- including INLINE parametr
hs_code (r,i) k = hs2str
                $ transform (r,i)
                -- $ inlines inl
                $ pt2expr
                $ parse
                $ example k

-- ===============================================================================================

example i = case i of

        1       -> "map f xs"
        11      -> "map f (map g xs)"
        12      -> "map (map (map f)) xsss"
        15      -> "let t = foldl f a xs in t"

        160     -> "let f = (*2) in foldl (+) 0 (map f xs)"                                             -- C
        161     -> "let f = (*2) in foldl (+) 0 (map f [x0,x1,x2,x3,x4,x5])"                            -- graph

        170     -> "foldl f a xs + foldl g b ys"
        171     -> "foldl f a [x0,x1,x2,x3] + foldl g b [y0,y1,y2,y3,y4,y5]"

        -- ====================================================================================
        -- Transformations on foldl

        210     -> "foldl (+) a xs"                                                             -- C    , laws ("f",10,20,30)
        211     -> "foldl (+) 0 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11]"                        -- graph, laws ("f",12,13,14,22,23,24,32,33,34)
        -- ------------------------------------------------------------------------------------
        220     -> unlines [ "let"                                                              -- C    , inline (Idf "isum")
                           , "  xss = split m xs;"
                           , "  isum = \\a xs -> foldl (+) a xs"
                           , "in"
                           , "  foldl isum 0 xss"
                           ]
        221     -> unlines [ "let"                                                              -- graph, inlines [Idf "xss", Idf "isum"]
                           , "  xss = split 3 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11];"         -- ALWAYS inline xss
                           , "  isum = \\a xs -> foldl (+) a xs"
                           , "in"
                           , "  foldl isum 0 xss"
                           ]
        -- ------------------------------------------------------------------------------------
        230     -> unlines [ "let"                                                              -- C    , inline (Idf "sum")
                           , "  xss = split m xs;"
                           , "  dsum = foldl (+) 0"
                           , "in"
                           , "  foldl (+) 0 (map dsum xss)"
                           ]
        231     -> unlines [ "let"                                                              -- graph, inlines [Idf "xss", Idf "sum"]
                           , "  xss = split 4 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11];"         -- ALWAYS inline xss
                           , "  dsum = \\xs -> foldl (+) 0 xs"
                           , "in"
                           , "  foldl (+) 0 (map dsum xss)"
                           ]
        -- ------------------------------------------------------------------------------------
        240     -> unlines [ "let"                                                              -- C    , inline (Idf "psum")
                           , "  xss = split m xs;"
                           , "  zeroes = replicate m 0;"
                           -- , "  psum = \\as xs -> zipWith (+) as xs"
                           , "  psum = zipWith (+)"
                           , "in"
                           , "  foldl (+) 0 (foldl psum zeroes xss)"
                           ]
        241     -> unlines [ "let"                                                              -- graph, inlines [Idf "xss", Idf "zeroes", Idf psum"]
                           , "  xss = split 4 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11];"         -- ALWAYS inline xss, zeroes, psum
                           , "  zeroes = replicate 4 0;"
                           , "  psum = \\as xs -> zipWith (+) as xs"
                           -- , "  psum = zipWith (+)"
                           , "in"
                           , "  foldl (+) 0 (foldl psum zeroes xss)"
                           ]


        245     -> " foldl (+) 0 xs"                                                             -- C    , laws ("f",10,20,30)
        256     -> " foldl (foldl (+)) 0 xss"
        247     -> " foldl (+) 0 (map (foldl (+) 0) xss)"
        248     -> " foldl (+) 0 (foldl (zipWith (+)) zs xss)"

        253     -> " zipWith (f <. g) xs ys"

        254     -> " zipWith ((map f) <. (map g)) xss yss"
        2541    -> " zipWith ((map f) <. (map g)) xss yss"


        -- ====================================================================================
        -- ====================================================================================
        -- ====================================================================================

        260     -> "foldl f a xs"
        261     -> "foldl (foldl f) a xss"

        -- ====================================================================================

        262     -> "zipWith f xs ys"

        263     -> "zipWith (zipWith f) xss yss"

        264     -> "zipWith g (map f xs) ys"

        265     -> "zipWith (f .> g) xs ys"

        266     -> "zipWith (zipWith (f .> g)) xss yss"

        267     -> "zipWith ((map f) .> (zipWith g))  xss  yss"

        268     -> "zipWith (zipWith g)  (map (map f) xss) yss"

        -- ====================================================================================




        278     -> " zipWith g (zipWith f xs ys) zs"
        279     -> " zipWith (f .> g) (zip (xs,ys)) zs"

        281     -> " zipWith ((f.>g).>h) (zip(ps,vs)) ps"
        282     -> " zipWith h (zipWith g (map f ps) vs) ps"


        -- ====================================================================================
        -- Examples D4.2, Imdea
        -- Difference: variable naming ==>> memory usage !! Parallelizability !!

        -- D4.2, Fig 2.2, p19
        310     -> "let C = map (a*) V in zipWith (\\c v -> c + b*v) C V"               -- (a)
        311     -> "map (\\v -> let c = a*v in c + b*v) V"                              -- (a)

        320     -> "zipWith (\\c v -> c + b*v) (map (a*) V) V"                          -- inline c' in (a)
        321     -> "zipWith ((+) <. (b*)) (map (a*) V) V"                               -- inline c' in (a)
        322     -> "zipWith ((a*) .> (+)) V (map (b*) V)"                               -- inline c' in (a)
        323     -> "zipWith (+) (map (a*) V) (map (b*) V)"                              -- (a) parallelized

        330     -> "map (\\v -> let c=a*v in c+b*v) V"                                  -- (b), (c) ... almost
        331     -> "map (\\v -> let c=a*v, cc=c+b*v in cc) V"                           -- (b), (c)             -- c=a*v, c=c+b*v: SCOPE! Not yet correctly implemented.

        340     -> "map (\\v -> a*v + b*v) V"                                           -- (d)
        341     -> "map (\\v -> (a+b)*v) V"                                             -- (e)
        342     -> "let k=a+b in map (k*) V"                                            -- (f)

        350     -> "map (let f=\\v->a*v, g=\\v->b*v in \\v -> f v + g v) V"             -- extra
        351     -> "map (let f=(a*), g=(b*) in \\v -> f v + g v) V"                     -- extra


        -- ====================================================================================
        -- Dotproduct, matrix multiplication

        410     -> "foldl (+) 0 (zipWith (*) xs ys)"
        411     -> "foldl (+) 0 (zipWith (*) [x0,x1,x2,x3,x4,x5] [y0,y1,y2,y3,y4,y5])"
        412     -> "foldl (+) 0 (map (*2) xs)"

        -- ====================================================================================
        -- D4.2, Fig 2.4, p22
        420     -> unlines [ "let "     -- NOTE: uncurried dotprod !!
                           , "  dpr = (foldl (\\c (a,b) -> c+a*b) 0) . zip"                             -- C, function composition
                           , "in "
                           , "  map (\\as -> map (\\bs -> dpr (as,bs)) (transpose qss)) pss"
                           ]
        -- ------------------------------------------------------------------------------------
        421     -> unlines [ "let "     -- NOTE: uncurried dotprod !!
                           , "  dpr = (foldl (\\c (a,b) -> c+a*b) 0) . zip"                             -- graph
                           , "in "
                           , "  map (\\as -> map (\\bs -> dpr (as,bs)) (transpose [[q00,q01],[q10,q11]])) [[p00,p01],[p10,p11]]"
                           ]
        422     -> unlines [ "let "     -- NOTE: uncurried dotprod !!
                           , "  prod = \\p -> fst p * snd p;"
                           , "  dpr = (foldl  ((+) <. prod) 0) . zip"                                   -- graph, function composition
                           , "in "
                           , "  map (\\as -> map (\\bs -> dpr (as,bs)) (transpose [[q00,q01],[q10,q11]])) [[p00,p01],[p10,p11]]"
                           ]
        -- ------------------------------------------------------------------------------------
        430     -> unlines [ "let "                                                                     -- graph
                           ,    "dpr = (foldl (\\c (a,b) -> c+a*b) 0) . zip"
                           , "in "
                           ,    "dpr ([a0,a1,a2,a3] , [0,0,1,0])"
                           ]

        431     -> "foldl (\\c (a,b) -> c+a*b) 0 (zip ([1,2,3,4,5],[1,2,3,4,5]))"
        432     -> "foldl (\\c (a,b) -> c+a*b) 0 (zip ([a0,a1,a2,a3,a4],[0,0,1,0,0]))"

        -- ====================================================================================
        -- heat diffusion

        510     -> unlines [ "let"
                           , "  hf = \\triple -> snd3 triple + c * (fst3 triple - 2*snd3 triple + thd3 triple);"
                           , "  triples = zip3 (init(init ts), init(tail ts), tail(tail ts))"
                           , "in"
                           , "  map hf triples"
                           ]

        511     -> unlines [ "let"
                           -- , "  hf = \\(tl,t,tr) -> t + c * (tl - 2*t + tr);"                                -- compare variant 301
                           , "  hf = \\triple -> snd3 triple + c * (fst3 triple - 2*snd3 triple + thd3 triple);"
                           , "  ts = [t0,t1,t2,t3,t4,t5,t6];"
                           , "  triples = zip3 (init(init ts), init(tail ts), tail(tail ts))"
                           , "in"
                           , "  map hf triples"
                           ]


        1000    -> unlines [ "let"
                           , "  p = \\(i,s) -> i < 10 :: (Int,Int) -> (Int,Int);"
                           , "  f = \\(i,s) -> (i+1, s+i) :: (Int,Int) -> (Int,Int);"
                           , "  i0 = 0 :: Int;"
                           , "  s0 = 0 :: Int"
                           , "in"
                           , "  while p f (i0,s0)"
                           ]

        1233    -> unlines [ "let"
                           , "  sxv = \\x   ys -> map (x*) ys;"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss;"

                           , "  x0   = [5,6];"
                           , "  bVec = [1,2];"
                           , "  aMat = [[1,2];[3,4]];"
                           , "  r0   = zipWith (-) bVec (mxv aMat x0)"
                           -- , "  del0 = vxv r0 r0;"
                           , "in"
                           , "  vxv r0 r0"
                           ]

        3999    -> unlines [ "let"
                           , "  vxv' = \\xs ys -> foldl (\\c (a,b) -> c + a*b) 0    ( zip xs ys)"
                           , "in"
                           , "  map (\\xs -> vxv' xs ys) xss"
                           ]

        40001   -> unlines [ "let"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxmA = \\xss yss -> map (mxv xss) (transpose yss)"
                           , "in"
                           , "  mxmA xss yss"
                           ]

        40002   -> unlines [ "let"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  vxm = \\xs yss -> map (vxv xs) (transpose yss);"
                           , "  mxmB = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxmB xss yss"
                           ]

        4000    -> unlines [ "let"
                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys);"

                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxmA = \\xss yss -> map (mxv xss) (transpose yss);"

                           , "  vxm = \\xs yss -> map (vxv xs) (transpose yss);"
                           , "  mxmB = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxmB xss yss"
                           ]

        4001    -> unlines [ "let"
                           , "  xss = [[x00,x01],[x10,x11]];"
                           , "  yss = [[y00,y01],[y10,y11]];"

                           , "  vxv = \\xs ys -> foldl (+) 0 (zipWith (*) xs ys);"

                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxmA = \\xss yss -> map (mxv xss) (transpose yss);"

                           , "  vxm = \\xs yss -> map (vxv xs) (transpose yss);"
                           , "  mxmB = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxmA xss yss"
                           ]


        4002    -> unlines [ "let"
                           , "  vxv  = \\xs  ys  -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys));"

                           , "  mxv  = \\xss ys  -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxmA = \\xss yss -> map (mxv xss) (transpose yss)"

                           , "in"
                           , "  mxmA xss yss"
                           ]

        4003    -> unlines [ "let"
                           , "  vxv = \\xs  ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys));"

                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss"

                           , "in"
                           , "  mxv xss ys"
                           ]


        4004    -> unlines [ "let"      -- mxm: variant 1
                           , "  vxv = \\xs   ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys));"
                           , "  mxv = \\xss  ys -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxm = \\xss yss -> transpose (map (mxv xss) (transpose yss))"
                           , "in"
                           , "  mxm xss yss"
                           ]

        40041   -> unlines [ "let"      -- mxm: variant 1
                           , "  vxv = \\xs   ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  mxv = \\xss  ys -> map (\\xs -> vxv xs ys) xss;"
                           , "  mxm = \\xss yss -> transpose (map (mxv xss) (transpose yss))"
                           , "in"
                           , "  mxm xss yss"
                           ]

        4005    -> unlines [ "let"      -- mxm: variant 2
                           , "  vxv = \\xs   ys -> foldl (\\a (x,y) -> a+x*y) 0 (zip (xs,ys));"
                           , "  vxm = \\xs  yss -> map (vxv xs) (transpose yss);"
                           , "  mxm = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxm xss yss"
                           ]

        40051   -> unlines [ "let"      -- mxm: variant 2
                           , "  vxv = \\xs   ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  vxm = \\xs  yss -> map (vxv xs) (transpose yss);"
                           , "  mxm = \\xss yss -> map (\\xs -> vxm xs yss) xss"
                           , "in"
                           , "  mxm xss yss"
                           ]



        4100    -> unlines [ -- "let"
                           -- , "  zipWith2 f xs ys = zipWith (zipWith f) xs ys"
                           -- , "in"
                           "  zipWith (zipWith(*)) A B"
                           ]




        -- CG
        1234    -> unlines [ "let"
                           , "  sxv = \\x   ys -> map (x*) ys;"
                           , "  vxv = \\xs  ys -> foldl (+) 0 (zipWith (*) xs ys);"
                           , "  mxv = \\xss ys -> map (\\xs -> vxv xs ys) xss;"

                           , "  bVec = [10,12];"
                           , "  aMat = [[1,2],[3,4]];"
                           , "  imax = 10;"
                           , "  eps  = 20;"

                           , "  i0   = 0;"
                           , "  r0   = zipWith (-) bVec (mxv aMat x0);"
                           , "  d0   = r0;"
                           , "  del0 = vxv r0 r0;"

                           , "  x0   = [5,6];"

                           , "  f    = \\((i,del,x),d,r) ->"
                           , "           let"
                           , "             q    = mxv aMat d;"
                           , "             a    = del / vxv d q;"
                           , "             x'   = zipWith (+) x (sxv a d);"
                           , "             r'   = if   (mod i 50 == 0)"
                           , "                    then (zipWith (-) bVec (mxv aMat x))"
                           , "                    else (zipWith (-) r (sxv a q));"
                           , "             del' = vxv r r;"
                           , "             b    = del' / del;"
                           , "             d'   = zipWith (+) r' (sxv b d);"
                           , "             i' = i+1"
                           , "           in"
                           , "             ((i',del',x'),d',r');"

                           , "  p    = \\((i,del,x),d,r) -> i<imax && del > eps^2 * del0"

                           , "in"
                           , "  while p f ((i0,del0,x0),d0,r0)"
                           ]

        -- Gaussian Pyramyd
        2000    -> unlines [ "let"
                           , "  dScl1 = \\xs -> map head (split 2 xs);"
                           -- , "  dScl1 = \\[] -> [];"
                           -- , "  dScl1 = \\(x:xs) -> x : dScl1 (drop 1 xs)"
                           , "  dScl = \\xss -> map head (split 2 xss);"

                           , "  img' = dScl (last imgs);"
                           , "  imgs = scanl blur img fltrs;"
                           , "  fltrs  = map g sigmas;"
                           , "  sigmas = itnscan (*k) sigma (s-1);"
                           , "  ff = \\img sigma -> (img', imgs);"

                           , "  (img', imgss) = mapAccumL ff img sigmas;"
                           , "  sigmas = itnscan (*2) sigma0 (o-1);"
                           , "  gPyr = \\img sigma -> (img', imgss);"
                           , "  img = [[1,2,3],[4,5,6],[7,8,9]];"
                           , "  mmm = 3;"               -- length img
                           , "  nnn = 3;"               -- length $ head img
                           , "  o = 4;"
                           , "  s = 3;"
                           , "  k = 1;"         -- 1.1
                           , "  sigma0 = 2;"    -- 1.6

                           -- , "  g = \\sigma -> \\(x,y) -> let"
                           -- , "                              s2Sq = 2 * sigma^2"
                           -- , "                            in"
                           -- , "                              exp (-(x^2+y^2)/s2Sq) / (pi * s2Sq);"
                           , "  g = \\sigma -> \\p -> let"
                           , "                              s2Sq = 2 * sigma^2"
                           , "                            in"
                           , "                              exp (0-((fst p)^2+(snd p)^2)/s2Sq) / (pi * s2Sq);"
                           , "  r = 1;"         -- ceiling (3*sigma)
                           , "  gM = \\sigma -> map (g sigma) crds;"    -- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)];"    -- Wrong

                           , "  conv = \\pss qss -> foldl2 (+) 0 (zipWith2 (*) pss qss);"
                           , "  foldl2   = foldl.foldl;"
                           , "  zipWith2 = zipWith.zipWith;"

                           , "  fstCols = map (replicate r) (map head img);"
                           , "  lstCols = map (replicate r) (map last img);"
                           , "  body = zipWith (+) (zipWith (+) fstCols img) lstCols;"
                           , "  extend = \\img r -> replicate r (head body) + body + replicate r (last body);"
                           , "  img = extend img r;"

                           -- , "  subImg = \\img r (x,y) -> map (take (2*r+1)) (map (drop y) (take (2*r+1) (drop x img)));"
                           , "  subImg = \\img r p -> map (take (2*r+1)) (map (drop (snd p)) (take (2*r+1) (drop (fst p) img)));"

                           , "  r = ceiling (3*sigma);"
                           -- , "  blur img sigma = [ [ conv (gM sigma) (subImg img r (x,y)) | y<-[0..mmm-1] ]"
                           -- , "                                                    | x<-[0..nnn-1] ]"
                           , "  blur = \\img sigma -> map (map ((conv (gM sigma)) . (subImg img r))) img"
                           -- ,         "[[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)],[(2,0),(2,1),(2,2)]];"

                           , "in"
                           , "  dScl (extend img 3)"
                           ]

        3000    -> unlines [
                            "dScl []       = []"
                           , "dScl (xs:xss) = dScl1 xs : dScl (drop 1 xss)"

                           , "let"
                           , "  img'   = dScl (last imgs);"
                           , "  imgs   = scanl blur img fltrs;"
                           , "  fltrs  = map g sigmas;"
                           , "  sigmas = itnscan (*k) sigma (s-1)"
                           , "in"
                           , "  ff img sigma = (img', imgs)"

                           , "let"
                           , "  (img', imgss) = mapAccumL ff img sigmas;"
                           , "  sigmas = itnscan (*2) sigma0 (o-1)"
                           , "in"
                           , "  gPyr img sigma = (img', imgss)"

                           , "img = [[1,2,3],[4,5,6],[7,8,9]]"
                           , "mmm = 3"          -- length img
                           , "nnn = 3"          -- length $ head img

                           , "o = 4"
                           , "s = 3"
                           , "k = 1"            -- 1.1
                           , "sigma0 = 2"       -- 1.6

                           , "let"
                           , "  s2Sq = 2 * sigma^2"
                           , "in"
                           , "  g sigma = \\(x,y) -> exp (-(x^2+y^2)/s2Sq) / (pi * s2Sq)"

                           , "let"
                           , "  r = 1"          -- ceiling (3*sigma)
                           , "in"
                           , "  gM sigma = map (g sigma) [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]"       -- Wrong
                           -- , "  gM sigma = [ [g sigma (i,j) | j<-[-r..r]]  | i<-[-r..r] ]"

                           , "conv pss qss = foldl2 (+) 0 (zipWith2 (*) pss qss)"

                           , "let"
                           , "  fstCols = map (replicate r . head) img;"
                           , "  lstCols = map (replicate r . last) img"
                           , "  body = zipWith (++) (zipWith (++) fstCols img) lstCols"
                           , "in"
                           , "  extend img r = replicate r (head body) ++ body ++ replicate r (last body)"

                           , "let"
                           , "  img' = extend img r"
                           , "in"
                           , "  subImg img r (x,y) = map (take (2*r+1) . drop y) (take (2*r+1) $ drop x img')"

                           , "let"
                           , "  r = ceiling (3*sigma)"
                           , "in"
                           -- , "  blur img sigma = [ [ conv (gM sigma) (subImg img r (x,y)) | y<-[0..mmm-1] ]"
                           -- , "                                                    | x<-[0..nnn-1] ]"
                           , "map (map ((conv (gM sigma)) . (subImg img r)))"
                           ,            "[[(0,0),(0,1),(0,2)],[(1,0),(1,1),(1,2)],[(2,0),(2,1),(2,2)]]"

                           , "itnscan f a 0 = [a]"
                           , "itnscan f a n = a : itnscan f (f a) (n-1)"

                           , "foldl2   = foldl.foldl"
                           , "zipWith2 = zipWith.zipWith"
                           ]

                        -- dpr = (foldl (\\c (a,b) -> c+a*b) 0) . zip

        -- N-Body
        6000    -> unlines [ "let"
                           , "  vAdd   = \\(x,y,z) -> \\(x',y',z') -> (x+x', y+y', z+z')"
                           , "          :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  vSub   = \\(x,y,z) -> \\(x',y',z') -> (x-x', y-y', z-z')"
                           , "          :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  sProd  = \\a -> \\(x,y,z) -> (a*x, a*y, a*z)"
                           , "          :: Float -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  force1 = \\(dx,dy,dz) -> let"
                           , "                        distSqr  = dx^2 + dy^2 + dz^2 + softening :: Float;"
                           , "                        invDist  = 1 / (sqrt distSqr)             :: Float;"
                           , "                        invDist3 = invDist^3                      :: Float"
                           , "                      in"
                           , "                         sProd invDist3 (dx,dy,dz)"
                           , "          :: (Float,Float,Float) -> (Float,Float,Float);"

                           -- , "  forceN = \\p -> \\ps -> foldl vAdd (0.0,0.0,0.0) (map force1 (map (vSub p) ps))"
                           , "  forceN = \\p -> \\ps -> foldl (vAdd <. (force1 . (vSub p))) (0.0,0.0,0.0) ps"
                           , "          :: (Float,Float,Float) -> (List (Float,Float,Float)) -> (Float,Float,Float);"

                           , "  velocity = \\ps -> \\(p,v) -> vAdd v (sProd dt (forceN ps p))"
                           , "          :: (List (Float,Float,Float)) -> ((Float,Float,Float),(Float,Float,Float)) -> (Float,Float,Float);"

                           , "  update1 = \\ps -> \\(p,v) -> let"
                           , "                                 v' = velocity ps (p,v)   :: (Float,Float,Float);"
                           , "                                 p' = vAdd p (sProd dt v) :: (Float,Float,Float)"
                           , "                               in"
                           , "                                 ( p', v' )"
                           , "          :: (List (Float,Float,Float)) -> ((Float,Float,Float),(Float,Float,Float)) -> ((Float,Float,Float),(Float,Float,Float));"

                           , "  updateN = \\pvs -> let"
                           , "                       ps = map fst pvs     :: List (Float,Float,Float)"
                           , "                     in"
                           , "                       map (update1 ps) pvs"
                           , "          :: (List ((Float,Float,Float),(Float,Float,Float))) -> (List ((Float,Float,Float),(Float,Float,Float)))"
                           , "in"
                           , "  updateN [ ((1.0,2.0,3.0),(1.0,2.0,3.0)), ((4.0,5.0,6.0),(4.0,5.0,6.0)) ]"
                           ]

        6003    -> unlines [ "let"
                           , "  setA = \\x -> let a=x::Int in a :: Int->Int;"
                           , "  mul3 = \\x -> x+x+x :: Int->Int"
                           , "in"
                           , "  mul3 (setA 5)"
                           ]

        -- N-Body
        6002    -> unlines [ "let"
                           , "  vAdd   =  \\(x,y,z) -> \\(x',y',z') -> (x+x', y+y', z+z')"
                           , "         :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  vSub   =  \\(x,y,z) -> \\(x',y',z') -> (x-x', y-y', z-z')"
                           , "         :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  sProd  =  \\a -> \\(x,y,z) -> (a*x, a*y, a*z)"
                           , "         :: Float -> (Float,Float,Float) -> (Float,Float,Float);"

                           , "  force1 =  \\(dx,dy,dz) -> let"
                           , "                        distSqr  = dx^2 + dy^2 + dz^2 + softening :: Float;"
                           , "                        invDist  = 1 / (sqrt distSqr)             :: Float;"
                           , "                        invDist3 = invDist^3                      :: Float"
                           , "                      in"
                           , "                         sProd invDist3 (dx,dy,dz)"
                           , "         :: (Float,Float,Float) -> (Float,Float,Float);"

                           , "  forceN =  \\p -> \\ps -> foldl (vAdd <. (force1 . (vSub p))) (0,0,0) ps"
                           , "         :: (Float,Float,Float) -> (List (Float,Float,Float)) -> (Float,Float,Float)"

                           , "in"
                           , "  forceN (1.0,2.0,3.0) [(4.0,5.0,6.0), (7.0,8.0,9.0), (10.0,11.0,12.0)]"
                           ]

        8000    -> unlines [ "map fst [(1,2.0), (3,4.0)]" ]
        8001    -> unlines [ "zipWith (+) [1.0,2.0,3.0] [4.0,5.0,6.0]" ]

        8010    -> unlines [ "let"
                           , " invD3 = \\(dx,dy,dz) -> let"
                           , "                          distSq = dx^2 + dy^2 + dz^2 + SOFT :: Float;"
                           , "                          invD   = 1 / sqrt distSq           :: Float"
                           , "                        in"
                           , "                          invD^3;"
                           , "  force = \\ps -> \\p -> foldl (\\f -> \\q -> f + d * invD3 (q-p)) (0,0,0) ps"
                           , "        :: List (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float);"
                           , "  upd   = \\ps -> \\p -> \\v -> v + dt * force ps p"
                           , "        :: List (Float,Float,Float) -> (Float,Float,Float -> (Float,Float,Float) -> (Float,Float,Float);"
                           , "in"
                           , "  vs'   = zipWith (upd ps) ps vs"
                           ]

        9001    -> unlines [ "3 * (x+5)" ]
        9002    -> unlines [ "foldl (+) 0 [1,2,3,4]" ]
        9003    -> unlines [ "foldl f a xs" ]
        9004    -> unlines [ "foldl (foldl f) a xss" ]
        9005    -> unlines [ "foldl f a (map (foldl f a) xss)" ]
        9006    -> unlines [ "foldl f a (foldl (zipWith f) as xss)" ]


f <. g = \a x -> f a ( g x )
