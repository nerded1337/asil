

-- UUAGC 0.9.52.1 (src/CFG.ag)
module CFG where

{-# LINE 6 "src\\CFG.ag" #-}

import Data.Word
import ByteCode
{-# LINE 11 "src/CFG.hs" #-}
-- CFG ---------------------------------------------------------
data CFG = CFG_CFG !(Int) !(Segments)
-- CFGs --------------------------------------------------------
type CFGs = [CFG]
-- Code --------------------------------------------------------
data Code = Code_Nop
          | Code_Label !(Int)
          | Code_Jump !(Int) !(JumpCond)
          | Code_Switch !(Int) !(([Int]))
-- Codes -------------------------------------------------------
type Codes = [Code]
-- JumpCond ----------------------------------------------------
data JumpCond = JumpCond_None
              | JumpCond_IfEq
              | JumpCond_IfFalse
              | JumpCond_IfGe
              | JumpCond_IfGt
              | JumpCond_IfLe
              | JumpCond_IfLt
              | JumpCond_IfNGe
              | JumpCond_IfNGt
              | JumpCond_IfNLe
              | JumpCond_IfNLt
              | JumpCond_IfNe
              | JumpCond_IfStrictEq
              | JumpCond_IfStrictNe
              | JumpCond_IfTrue
              | JumpCond_Jump
-- Node --------------------------------------------------------
data Node = Node_Pseudo !(Int) !(Pseudo) !(Codes)
          | Node_Opaque !(Instruction)
-- Nodes -------------------------------------------------------
type Nodes = [Node]
-- Program -----------------------------------------------------
data Program = Program_Program !(CFGs)
-- Programs ----------------------------------------------------
type Programs = [Program]
-- Pseudo ------------------------------------------------------
data Pseudo = Pseudo_Nop
            | Pseudo_EnterBlock
            | Pseudo_LeaveBlock
            | Pseudo_FailBlock
            | Pseudo_EnterFun
            | Pseudo_LeaveFun
            | Pseudo_FailFun
            | Pseudo_BeginCall
            | Pseudo_DoneCall
            | Pseudo_FailedCall
            | Pseudo_BeginCoerce
            | Pseudo_DoneCoerce
            | Pseudo_FailedCoerce
-- Segment -----------------------------------------------------
data Segment = Segment_Segment !(Int) !(Nodes) !(Code)
-- Segments ----------------------------------------------------
type Segments = [Segment]