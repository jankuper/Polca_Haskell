module HWTypes where

import CLaSH.Prelude

type IntPart   = 12
type FracPart  = 15

type FixPt     = SFixed IntPart FracPart

type Triple    = (FixPt,FixPt,FixPt)
type Triples32 = Vec 32 Triple

type Addr      = Unsigned 5
type Heap      = Vec 17 FixPt
type StPtr     = Unsigned 2
type Stack     = Vec 4 FixPt

type MAddr     = Unsigned 2
type MRegs     = Vec 3 FixPt

type InstrNr   = Unsigned 6
type CoreNr    = Unsigned 5

type MState    = (InstrNr,CoreNr,MRegs)
type CState    = (Heap,Stack,StPtr)
type MInput    = Vec 32 (Maybe FixPt)

type CStates32 = Vec 32 CState

data OpCode    = ShiftR | Magic -- Magic represents operation in fast-inverse-square-root
               | Add | Sub | Mul
               deriving (Show,Eq)

data ElExpr    = Cnst FixPt
               | Input FixPt
               | Read Addr
               | Pop
               deriving (Show,Eq)

data Expr      = Elem ElExpr
               | Comp OpCode ElExpr ElExpr
              deriving (Show,Eq)

data Instr     = Push Expr
               | Save Addr Expr
               | Send Addr
               | Repeat InstrNr
               | Restart
               deriving (Show,Eq)

type Program   = Vec 46 Instr

