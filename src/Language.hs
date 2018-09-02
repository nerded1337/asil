

-- UUAGC 0.9.52.1 (src/Language.ag)
module Language where

{-# LINE 6 "src\\Language.ag" #-}

import Data.Word
{-# LINE 10 "src/Language.hs" #-}
-- BinOp -------------------------------------------------------
data BinOp = BinOp_And
           | BinOp_Or
           | BinOp_Rel (Rel)
           | BinOp_Add
           | BinOp_Sub
           | BinOp_Mul
           | BinOp_Div
           | BinOp_Mod
           | BinOp_Max
           | BinOp_Min
           deriving ( Eq,Show)
-- Coercion ----------------------------------------------------
data Coercion = Coercion_None
              | Coercion_Any
              | Coercion_Any'
              | Coercion_String
              | Coercion_Double
              | Coercion_Int
              | Coercion_UInt
              | Coercion_Instance
              deriving ( Eq,Show)
-- Instr -------------------------------------------------------
data Instr = Instr_Nop
           | Instr_Fail ((Maybe String))
           | Instr_Seq (Instr) (Instr)
           | Instr_Alt (Instr) (Instr) (Bool)
           | Instr_Loop (Instr)
           | Instr_CallFun (Val) (Params) (Params)
           | Instr_CallProp (Val) (Params) (Params)
           | Instr_Match (Match)
           | Instr_Type (Val) (Val)
           | Instr_Static (Instr)
           | Instr_Dyn (Instr)
           | Instr_Assign (Int) (Val)
           | Instr_Assert (Val)
           | Instr_BinOp (Val) (Val) (Val) (BinOp)
           | Instr_UnOp (Val) (Val) (UnOp)
           | Instr_Scan (Val) (Val) (Params)
           | Instr_Pretty (Val) (Params) (Val)
           | Instr_TypeOf (Val) (Val)
           | Instr_Coerce (Coercion) (Val) (Val)
           | Instr_Last (Instr)
           deriving ( Eq,Show)
-- Match -------------------------------------------------------
data Match = Match_EnterBlock (Val) (Val) (Val)
           | Match_LeaveBlock (Val) (Val)
           | Match_FailBlock (Val) (Val)
           | Match_EnterFun (Val) (Val) (Val) (Params)
           | Match_LeaveFun (Val) (Val) (Val) (Params)
           | Match_FailFun (Val) (Val) (Val) (Val)
           | Match_BeginCall (Val) (Val) (Val) (Params)
           | Match_DoneCall (Val) (Val) (Val) (Params)
           | Match_FailedCall (Val) (Val) (Val) (Val)
           | Match_BeginCoerce (Val) (Val) (Coercion)
           | Match_DoneCoerce (Val) (Val) (Coercion)
           | Match_FailedCoerce (Val) (Val) (Val) (Coercion)
           deriving ( Eq,Show)
-- MaybeVal ----------------------------------------------------
type MaybeVal = Maybe (Val)
-- Param -------------------------------------------------------
data Param = Param_Param (Val)
           deriving ( Eq,Show)
-- Params ------------------------------------------------------
data Params = Params_Any (Val)
            | Params_Nil
            | Params_Cons (Param) (Params)
            deriving ( Eq,Show)
-- Rel ---------------------------------------------------------
data Rel = Rel_Equal
         | Rel_Smaller
         | Rel_Greater
         | Rel_SmallerEqual
         | Rel_GreaterEqual
         | Rel_Negate (Rel)
         deriving ( Eq,Show)
-- Spec --------------------------------------------------------
data Spec = Spec_Instr (Instr)
          deriving ( Eq,Show)
-- Type --------------------------------------------------------
data Type = Type_Any
          | Type_Bool
          | Type_Int
          | Type_UInt
          | Type_Double
          | Type_String
          | Type_Array (Type)
          | Type_Base
          | Type_Object (String)
          | Type_Method
          deriving ( Eq,Show)
-- UnOp --------------------------------------------------------
data UnOp = UnOp_Abs
          | UnOp_Not
          | UnOp_Neg
          | UnOp_IsJust
          | UnOp_IsNothing
          | UnOp_ExtractJust
          | UnOp_Length
          deriving ( Eq,Show)
-- Val ---------------------------------------------------------
data Val = Val_Int (Int)
         | Val_UInt (Word32)
         | Val_Bool (Bool)
         | Val_String (String)
         | Val_Array (Vals)
         | Val_Sym (Int)
         | Val_Ind (Val) (Val)
         | Val_Prop (Val) (String)
         | Val_Dyn (Val) (Val)
         | Val_Type (Type)
         | Val_Method (String)
         deriving ( Eq,Show)
-- Vals --------------------------------------------------------
type Vals = [Val]