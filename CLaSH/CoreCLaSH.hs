module CoreCLaSH where

import CLaSH.Prelude
import HWTypes
import InputTuples
import Data.Maybe

-- ==============================================================================
-- alu  -- may change the stack
-- ==============================================================================
calcElem :: CState -> ElExpr -> (FixPt,StPtr)
calcElem (heap,stack,sp) expr = (val,sp')
                where
                  val = case expr of
                                Cnst n     -> n
                                Input x    -> x
                                Read a     -> heap !!a
                                Pop        -> stack!!(sp-1)

                  sp' | expr == Pop  = sp - 1
                      | otherwise    = sp

alu :: CState -> Expr -> (FixPt,StPtr)
alu (heap,stack,sp) expr = case expr of

        Elem e0         -> calcElem (heap,stack,sp) e0

        Comp opc e0 e1  -> case opc of
                        Add     -> ( val0 + val1   , sp1 )
                        Sub     -> ( val0 - val1   , sp1 )
                        Mul     -> ( val0 * val1   , sp1 )
                        ShiftR  -> ( shiftR val0 1 , sp0 )
                        Magic   -> ( magic val0    , sp0 )
                        where
                          (val0,sp0) = calcElem (heap, stack, sp ) e0
                          (val1,sp1) = calcElem (heap, stack, sp0) e1

-- ==============================================================================
-- Core
-- ==============================================================================
core :: CState -> Instr -> (CState, Maybe FixPt)
core (heap,stack,sp) instr = case instr of

        Push e0     -> ( (heap             , stack<~(sp0,val0) , sp0+1) , Nothing )  where  (val0,sp0) = alu (heap,stack,sp) e0

        Save a e0   -> ( (heap <~ (a,val0) , stack             , sp0  ) , Nothing )  where  (val0,sp0) = alu (heap,stack,sp) e0

        Send a      -> ( (heap             , stack             , sp   ) , Just $ heap!!a )

        _           -> ( (heap             , stack             , sp   ) , Nothing )

-- ==============================================================================
-- Master
-- ==============================================================================
master :: Program -> MState -> MInput -> (MState, (Instr,MRegs))
master instrs (pc,k,qs) ins = ( (pc',k',qs') , (instr',qs) )
        where
          instr    = instrs!!pc

          instr'   = case instr of
                       Save a (Elem (Input _)) -> Save a $ Elem $ Input $ qs!!(a-6)
                       _                       -> instr

          (pc',k') = case instr of
                       Restart  -> (0 , 0)
                       Repeat i |  k /= 31      -> (i    , k+1)
                                |  otherwise    -> (pc+1 , k)
                       _        -> (pc+1, k  )

          qs'      = case instr of
                       Send a      -> qs <~ (a, fromJust $ ins!!k)
                       _           -> qs

-- ==============================================================================
-- Multicore
-- ==============================================================================
multicore :: Program -> (MState, CStates32) -> Bit -> ((MState, CStates32), MRegs)
multicore instrs (mstate,cstates) inp = ((mstate',cstates') , outps)
        where
          (mstate' ,(instr,qs)) = master instrs mstate couts

          (cstates',couts) = unzip $ zipWith core cstates $ replicate d32 instr

          outps = qs

-- ==============================================================================
-- program
-- ==============================================================================
nBodyProg :: Program
nBodyProg =    Send 0                                              -- Send "px"
            :> Send 1                                              -- Send "py"
            :> Send 2                                              -- Send "pz"
            -- 3 --
            :> (Save 6 $ Elem $ Input 0)                           -- "0" is just a dummy; value will be filled in by master
            :> (Save 7 $ Elem $ Input 0)                           -- save "qx", "qy", "qz"
            :> (Save 8 $ Elem $ Input 0)
            -- 6 --
            :> (Save 6 $ Comp Sub (Read 6) (Read 0))               -- "dx" = "px" - "qx"
            :> (Save 7 $ Comp Sub (Read 7) (Read 1))               -- "dy" = "py" - "qy"
            :> (Save 8 $ Comp Sub (Read 8) (Read 2))               -- "dz" = "pz" - "qz"
            -- 9 --
            :> (Push $ Comp Mul (Read 6) (Read 6))                 -- "dx" * "dx"
            :> (Push $ Comp Mul (Read 7) (Read 7))                 -- "dy" * "dy"
            :> (Push $ Comp Add Pop Pop)
            :> (Push $ Comp Mul (Read 8) (Read 8))                 -- "dz" * "dz"
            :> (Push $ Comp Add Pop Pop)
            :> (Save 12 $ Comp Add Pop (Read 15))                  -- distSqr = dx*dx + dy*dy * dz*dz + sOFT
            -- 15 --

            -- Simplistic inverse_square_root:
            -- :> (Push $ Comp Sqrt (Read 12) (Cnst 0))               -- sqrt distSqr                 <<== sqrt
            -- :> (Save 13 $ Comp Div (Cnst 1) Pop)                   -- invDist = 1 / sqrt distSqr   <<== Div 

            -- Replaced by: fast_inverse_square_root               -- Variable names as in definition of "magic"
            :> (Push    $ Comp ShiftR (Read 12) (Cnst 0))          -- x2 on the stack
            :> (Save 12 $ Comp Magic  (Read 12) (Cnst 0))          -- y' on addr 12 (where distSqr was)
            :> (Push    $ Comp Mul    (Read 12) (Read 12))         -- y'*y' on the stack
            :> (Push    $ Comp Mul     Pop       Pop)              -- x2 * y'*y' on the stack
            :> (Push    $ Comp Sub    (Cnst 1.5) Pop)              -- 1.5 - (x2*y'*y') on the stack
            :> (Save 13 $ Comp Mul    (Read 12)  Pop)              -- y (= invDist) on addr 13 (where invDist should go)

            -- 21 --
            :> (Push $ Comp Mul (Read 13) (Read 13))               -- invDist * invDist
            :> (Save 14 $ Comp Mul Pop (Read 13))                  -- invDist3 = invDist *invDist * invDist
            -- 23 --
            :> (Push $ Comp Mul (Read 14) (Read 6))                -- invDist3 * dx
            :> (Save 9 $ Comp Add (Read 9) Pop)                    -- fx = fx + invDist3*dx
            -- 25 --
            :> (Push $ Comp Mul (Read 14) (Read 7))                -- invDist3 * dy
            :> (Save 10 $ Comp Add (Read 10) Pop)                  -- fy = fy + invDist3*dy
            -- 27 --
            :> (Push $ Comp Mul (Read 14) (Read 8))                -- invDist3 * dz
            :> (Save 11 $ Comp Add (Read 11) Pop)                  -- fz = fz + invDist3*dz
            -- 29 --
            :> (Repeat 0)                                          -- calcF
            -- 30 --
            :> (Push $ Comp Mul (Read 16) (Read 9))                -- dt * fx
            :> (Save 3 $ Comp Add (Read 3) Pop)                    -- vx = vx + dt*fx
            -- 32 --
            :> (Push $ Comp Mul (Read 16) (Read 10))               -- dt * fy
            :> (Save 4 $ Comp Add (Read 4) Pop)                    -- vy = vy + dt*fy
            -- 34 --
            :> (Push $ Comp Mul (Read 16) (Read 11))               -- dt * fz
            :> (Save 5 $ Comp Add (Read 5) Pop)                    -- vx = vx + dt*fz
            -- 36 --
            :> (Push $ Comp Mul (Read 16) (Read 3))                -- dt * vx
            :> (Save 0 $ Comp Add (Read 0) Pop)                    -- px = px + dt*vx
            -- 38 --
            :> (Push $ Comp Mul (Read 16) (Read 4))                -- dt * vy
            :> (Save 1 $ Comp Add (Read 1) Pop)                    -- py = py + dt*vy
            -- 40 --
            :> (Push $ Comp Mul (Read 16) (Read 5))                -- dt * vz
            :> (Save 2 $ Comp Add (Read 2) Pop)                    -- pz = pz + dt*vz
            -- 42 --
            :> (Save  9 $ Elem $ Cnst 0)                           -- fx = 0
            :> (Save 10 $ Elem $ Cnst 0)                           -- fy = 0
            :> (Save 11 $ Elem $ Cnst 0)                           -- fz = 0
            -- 45 --
            :> Restart
            -- 46 --    32 * 30 + 16 = 976
            :> Nil

-- ==============================================================================
-- basic functions
-- ==============================================================================
-- Fast Inverse Square root: BIG Thanks to Hendrik Folmer & Robin Appel
-- sFixedToFloatBitVector converts a SFixed representation to a bitvector containing the bit reprsentation of a floating point number
sFixedToFloatBitVector :: FixPt -> BitVector 32
sFixedToFloatBitVector x = vector
  where
    x'        = pack x ++# pack (repeat 0 :: Vec (FracPart + IntPart) Bit)

    lZeros    = fromIntegral $ countLeadingZerosBV $ pack x
    shft      = lZeros - (fromIntegral (snatToInteger (snat :: SNat (IntPart -1))))
    exp       = pack ( (127 - shft) :: Unsigned 9)
    mantissa' = bv2v $ shift x' (shft)
    mantissa  = v2bv $ select (snat :: SNat IntPart) d1 d23 mantissa'
    vector    = exp ++# mantissa

-- floatBitVectorToSFixed convert a bitvector, which contains a 32 bit floating point representation to a SFixed representation
floatBitVectorToSFixed :: BitVector 32 -> FixPt
floatBitVectorToSFixed bitVector = sfixed
  where
    vector   = bv2v bitVector
    exp      = v2bv $ select d1 d1 d8 vector
    mantissa = select d9 d1 d23 vector
    shft'    = unpack (exp - 127) :: Signed 8
    shft     = fromIntegral shft'
    sfixed'' = (repeat 0 :: Vec (IntPart -1) Bit) ++ (repeat 1 :: Vec 1 Bit) ++ mantissa ++ (repeat 0 :: Vec 1 Bit)
    sfixed'  = bv2v $ shift (pack sfixed'') shft
    sfixed   = unpack (pack $ select d0 d1 (snat :: SNat (IntPart + FracPart)) sfixed') :: FixPt

-- countLeadingZerosBV counts the leading zeros of a bitvector
countLeadingZerosBV :: (KnownNat n, KnownNat (n+1)) => BitVector n -> Index (n+1)
countLeadingZerosBV = foldr (\l r -> if l == low then 1 + r else 0) 0 . bv2v

magic :: FixPt -> FixPt         -- for fast_inverse_square_root
magic x = y'
  where
    floatBitVector' = sFixedToFloatBitVector x
    floatBitVector  = shiftR floatBitVector' 1
    magicTransf     = pack (1597463007 :: Unsigned 32) - floatBitVector
    y'              = floatBitVectorToSFixed magicTransf

xs <~ (i,x) = replace i x xs

-- ==============================================================================
-- topEntity
-- ==============================================================================
sOFT = 0.001
dt   = 0.04

tpl2heap (px,py,pz) (vx,vy,vz) =  px :> py :> pz
                               :> vx :> vy :> vz
                               :>  0 :>  0 :>  0
                               :>  0 :>  0 :>  0
                               :>  0 :>  0 :>  0
                               :> sOFT :> dt
                               :> Nil

heaps0   = zipWith tpl2heap ps0 vs0
stacks0  = replicate d32 $ 0:>0:>0:>0:>Nil
cstates0 = zip3 heaps0 stacks0 (replicate d32 0)

mstate0  = (0,0, 0:>0:>0:>Nil)

topEntity = (multicore nBodyProg) <^> (mstate0,cstates0)
