

-- UUAGC 0.9.52.1 (src/ByteCode.ag)
module ByteCode where

{-# LINE 6 "src\\ByteCode.ag" #-}

import Data.Word
import Data.ByteString.Lazy(ByteString)
import Data.Bits
{-# LINE 12 "src/ByteCode.hs" #-}
{-# LINE 19 "src\\ByteCode.ag" #-}

fromS24 :: Word32 -> Int
fromS24 w | testBit w 31 = negate (fromIntegral w)
          | otherwise    = fromIntegral w
{-# LINE 18 "src/ByteCode.hs" #-}

{-# LINE 27 "src\\ByteCode.ag" #-}

type AbcFiles = [AbcFile]
type SwfFiles = [SwfFile]
{-# LINE 24 "src/ByteCode.hs" #-}
-- AbcFile -----------------------------------------------------
data AbcFile = AbcFile_File !(Word16) !(Word16) !(PoolInfo) !(MethodInfos) !(MetaInfos) !(InstanceInfos) !(ClassInfos) !(ScriptInfos) !(BodyInfos)
             deriving ( Eq,Ord,Show)
-- AbcFlag -----------------------------------------------------
data AbcFlag = AbcFlag_LazyInit
             deriving ( Eq,Ord,Show)
-- AbcFlags ----------------------------------------------------
type AbcFlags = [AbcFlag]
-- BodyInfo ----------------------------------------------------
data BodyInfo = BodyInfo_Info !(Word32) !(Word32) !(Word32) !(Word32) !(Word32) !(Instructions) !(Exceptions) !(Traits)
              deriving ( Eq,Ord,Show)
-- BodyInfos ---------------------------------------------------
type BodyInfos = [BodyInfo]
-- CaseOffsets -------------------------------------------------
type CaseOffsets = [(Word32)]
-- ClassInfo ---------------------------------------------------
data ClassInfo = ClassInfo_Info !(Word32) !(Traits)
               deriving ( Eq,Ord,Show)
-- ClassInfos --------------------------------------------------
type ClassInfos = [ClassInfo]
-- DebugType ---------------------------------------------------
data DebugType = DebugType_Local
               deriving ( Eq,Ord,Show)
-- Exception ---------------------------------------------------
data Exception = Exception_Info !(Word32) !(Word32) !(Word32) !(Word32) !(Word32)
               deriving ( Eq,Ord,Show)
-- Exceptions --------------------------------------------------
type Exceptions = [Exception]
-- InstanceFlag ------------------------------------------------
data InstanceFlag = InstanceFlag_ClassSealed
                  | InstanceFlag_ClassFinal
                  | InstanceFlag_ClassInterface
                  | InstanceFlag_ClassProtected
                  deriving ( Eq,Ord,Show)
-- InstanceFlags -----------------------------------------------
type InstanceFlags = [InstanceFlag]
-- InstanceInfo ------------------------------------------------
data InstanceInfo = InstanceInfo_Info !(Word32) !(Word32) !(InstanceFlags) !(Word32) !(Interfaces) !(Word32) !(Traits)
                  deriving ( Eq,Ord,Show)
-- InstanceInfos -----------------------------------------------
type InstanceInfos = [InstanceInfo]
-- Instruction -------------------------------------------------
data Instruction = Instruction_Add
                 | Instruction_Add_i
                 | Instruction_Add_d
                 | Instruction_ApplyType !(Word32)
                 | Instruction_AsType !(Word32)
                 | Instruction_AsTypeLate
                 | Instruction_Breakpoint
                 | Instruction_BreakLine !(Word32)
                 | Instruction_BitAnd
                 | Instruction_BitNot
                 | Instruction_BitOr
                 | Instruction_BitXor
                 | Instruction_Call !(Word32)
                 | Instruction_CallInterface !(Word32) !(Word32)
                 | Instruction_CallMethod !(Word32) !(Word32)
                 | Instruction_CallProp !(Word32) !(Word32)
                 | Instruction_CallPropLex !(Word32) !(Word32)
                 | Instruction_CallPropVoid !(Word32) !(Word32)
                 | Instruction_CallStatic !(Word32) !(Word32)
                 | Instruction_CallSuper !(Word32) !(Word32)
                 | Instruction_CallSuperId
                 | Instruction_CallSuperVoid !(Word32) !(Word32)
                 | Instruction_CheckFilter
                 | Instruction_Coerce !(Word32)
                 | Instruction_Coerce_a
                 | Instruction_Coerce_b
                 | Instruction_Coerce_d
                 | Instruction_Coerce_i
                 | Instruction_Coerce_o
                 | Instruction_Coerce_s
                 | Instruction_Coerce_u
                 | Instruction_Concat
                 | Instruction_Construct !(Word32)
                 | Instruction_ConstructProp !(Word32) !(Word32)
                 | Instruction_ConstructSuper !(Word32)
                 | Instruction_Convert_b
                 | Instruction_Convert_i
                 | Instruction_Convert_d
                 | Instruction_Convert_o
                 | Instruction_Convert_u
                 | Instruction_Convert_s
                 | Instruction_Debug !(DebugType) !(Word32) !(Word32) !(Word32)
                 | Instruction_DebugFile !(Word32)
                 | Instruction_DebugLine !(Word32)
                 | Instruction_DecLocal !(Word32)
                 | Instruction_DecLocal_i !(Word32)
                 | Instruction_Decrement
                 | Instruction_Decrement_i
                 | Instruction_DeleteProperty !(Word32)
                 | Instruction_DeletePropertyLate
                 | Instruction_Divide
                 | Instruction_Dup
                 | Instruction_Dxns !(Word32)
                 | Instruction_DxnsLate
                 | Instruction_Equals
                 | Instruction_EscXAttr
                 | Instruction_EscXElem
                 | Instruction_FindDef !(Word32)
                 | Instruction_FindPropertyGlobalStrict !(Word32)
                 | Instruction_FindPropertyGlobal !(Word32)
                 | Instruction_FindProperty !(Word32)
                 | Instruction_FindPropStrict !(Word32)
                 | Instruction_GetDescendants !(Word32)
                 | Instruction_GetGlobalScope
                 | Instruction_GetGlobalSlot !(Word32)
                 | Instruction_GetLex !(Word32)
                 | Instruction_GetLocal !(Word32)
                 | Instruction_GetLocal0
                 | Instruction_GetLocal1
                 | Instruction_GetLocal2
                 | Instruction_GetLocal3
                 | Instruction_GetOuterScope !(Word32)
                 | Instruction_GetProperty !(Word32)
                 | Instruction_GetScopeObject !(Word8)
                 | Instruction_GetSlot !(Word32)
                 | Instruction_GetSuper !(Word32)
                 | Instruction_GreaterEquals
                 | Instruction_GreaterThan
                 | Instruction_HasNext
                 | Instruction_HasNext2 !(Word32) !(Word32)
                 | Instruction_IfEq !(Word32)
                 | Instruction_IfFalse !(Word32)
                 | Instruction_IfGe !(Word32)
                 | Instruction_IfGt !(Word32)
                 | Instruction_IfLe !(Word32)
                 | Instruction_IfLt !(Word32)
                 | Instruction_IfNGe !(Word32)
                 | Instruction_IfNGt !(Word32)
                 | Instruction_IfNLe !(Word32)
                 | Instruction_IfNLt !(Word32)
                 | Instruction_IfNe !(Word32)
                 | Instruction_IfStrictEq !(Word32)
                 | Instruction_IfStrictNe !(Word32)
                 | Instruction_IfTrue !(Word32)
                 | Instruction_In
                 | Instruction_IncLocal !(Word32)
                 | Instruction_IncLocal_i !(Word32)
                 | Instruction_Increment
                 | Instruction_Increment_i
                 | Instruction_InitProperty !(Word32)
                 | Instruction_InstanceOf
                 | Instruction_IsType !(Word32)
                 | Instruction_IsTypeLate
                 | Instruction_Jump !(Word32)
                 | Instruction_Kill !(Word32)
                 | Instruction_Label
                 | Instruction_LessEquals
                 | Instruction_LessThan
                 | Instruction_LoadFloat32
                 | Instruction_LoadFloat64
                 | Instruction_LoadIndirect8
                 | Instruction_LoadIndirect16
                 | Instruction_LoadIndirect32
                 | Instruction_LookupSwitch !(Word32) !(CaseOffsets)
                 | Instruction_Lshift
                 | Instruction_Modulo
                 | Instruction_Multiply
                 | Instruction_Multiply_i
                 | Instruction_Negate
                 | Instruction_Negate_i
                 | Instruction_NewActivation
                 | Instruction_NewArray !(Word32)
                 | Instruction_NewCatch !(Word32)
                 | Instruction_NewClass !(Word32)
                 | Instruction_NewFunction !(Word32)
                 | Instruction_NewObject !(Word32)
                 | Instruction_NextName
                 | Instruction_NextValue
                 | Instruction_Nop
                 | Instruction_Not
                 | Instruction_Pop
                 | Instruction_PopScope
                 | Instruction_PushByte !(Word8)
                 | Instruction_PushDouble !(Word32)
                 | Instruction_PushFalse
                 | Instruction_PushInt !(Word32)
                 | Instruction_PushNamespace !(Word32)
                 | Instruction_PushNaN
                 | Instruction_PushNull
                 | Instruction_PushScope
                 | Instruction_PushShort !(Word32)
                 | Instruction_PushString !(Word32)
                 | Instruction_PushTrue
                 | Instruction_PushUInt !(Word32)
                 | Instruction_PushUndefined
                 | Instruction_PushWith
                 | Instruction_ReturnValue
                 | Instruction_ReturnVoid
                 | Instruction_Rshift
                 | Instruction_SetLocal !(Word32)
                 | Instruction_SetLocal0
                 | Instruction_SetLocal1
                 | Instruction_SetLocal2
                 | Instruction_SetLocal3
                 | Instruction_SetGlobalSlot !(Word32)
                 | Instruction_SetProperty !(Word32)
                 | Instruction_SetPropertyLate
                 | Instruction_SetSlot !(Word32)
                 | Instruction_SetSuper !(Word32)
                 | Instruction_SignExtend1
                 | Instruction_SignExtend8
                 | Instruction_SignExtend16
                 | Instruction_StoreFloat32
                 | Instruction_StoreFloat64
                 | Instruction_StoreIndirect32
                 | Instruction_StoreIndirect16
                 | Instruction_StoreIndirect8
                 | Instruction_StrictEquals
                 | Instruction_Substract
                 | Instruction_Substract_i
                 | Instruction_Swap
                 | Instruction_Throw
                 | Instruction_Timestamp
                 | Instruction_TypeOf
                 | Instruction_Urshift
                 | Instruction_Location !(Int)
                 deriving ( Eq,Ord,Show)
-- Instructions ------------------------------------------------
type Instructions = [Instruction]
-- Interfaces --------------------------------------------------
type Interfaces = [(Word32)]
-- MetaInfo ----------------------------------------------------
data MetaInfo = MetaInfo_Info !(Word32) !(MetaItems)
              deriving ( Eq,Ord,Show)
-- MetaInfos ---------------------------------------------------
type MetaInfos = [MetaInfo]
-- MetaItem ----------------------------------------------------
data MetaItem = MetaItem_Item !(Word32) !(Word32)
              deriving ( Eq,Ord,Show)
-- MetaItems ---------------------------------------------------
type MetaItems = [MetaItem]
-- MethodFlag --------------------------------------------------
data MethodFlag = MethodFlag_NeedArgs
                | MethodFlag_NeedAct
                | MethodFlag_NeedRest
                | MethodFlag_HasOptionals
                | MethodFlag_SetDXNS
                | MethodFlag_HasParamNames
                deriving ( Eq,Ord,Show)
-- MethodFlags -------------------------------------------------
type MethodFlags = [MethodFlag]
-- MethodInfo --------------------------------------------------
data MethodInfo = MethodInfo_Info !(Word32) !(ParamTypes) !(Word32) !(MethodFlags) !(Optionals) !(ParamNames)
                deriving ( Eq,Ord,Show)
-- MethodInfos -------------------------------------------------
type MethodInfos = [MethodInfo]
-- MultinameInfo -----------------------------------------------
data MultinameInfo = MultinameInfo_QName !(Word32) !(Word32)
                   | MultinameInfo_QNameA !(Word32) !(Word32)
                   | MultinameInfo_RTQName !(Word32)
                   | MultinameInfo_RTQNameA !(Word32)
                   | MultinameInfo_RTQNameL
                   | MultinameInfo_RTQNameLA
                   | MultinameInfo_Multiname !(Word32) !(Word32)
                   | MultinameInfo_MultinameA !(Word32) !(Word32)
                   | MultinameInfo_MultinameL !(Word32)
                   | MultinameInfo_MultinameLA !(Word32)
                   | MultinameInfo_Generic !(Word32) !(ParamNames)
                   deriving ( Eq,Ord,Show)
-- MultinameInfos ----------------------------------------------
type MultinameInfos = [MultinameInfo]
-- MultinameKind -----------------------------------------------
data MultinameKind = MultinameKind_QName
                   | MultinameKind_QNameA
                   | MultinameKind_RTQName
                   | MultinameKind_RTQNameA
                   | MultinameKind_RTQNameL
                   | MultinameKind_RTQNameLA
                   | MultinameKind_Multiname
                   | MultinameKind_MultinameA
                   | MultinameKind_MultinameL
                   | MultinameKind_MultinameLA
                   | MultinameKind_Generic
                   deriving ( Eq,Ord,Show)
-- NamespaceInfo -----------------------------------------------
data NamespaceInfo = NamespaceInfo_Info !(NamespaceKind) !(Word32)
                   deriving ( Eq,Ord,Show)
-- NamespaceInfos ----------------------------------------------
type NamespaceInfos = [NamespaceInfo]
-- NamespaceKind -----------------------------------------------
data NamespaceKind = NamespaceKind_General
                   | NamespaceKind_Package
                   | NamespaceKind_Internal
                   | NamespaceKind_Protected
                   | NamespaceKind_Explicit
                   | NamespaceKind_Static
                   | NamespaceKind_Private
                   deriving ( Eq,Ord,Show)
-- NamespaceNames ----------------------------------------------
type NamespaceNames = [(Word32)]
-- Optional ----------------------------------------------------
data Optional = Optional_Detail !(Word32) !(ValueKind)
              deriving ( Eq,Ord,Show)
-- Optionals ---------------------------------------------------
type Optionals = [Optional]
-- ParamNames --------------------------------------------------
type ParamNames = [(Word32)]
-- ParamTypes --------------------------------------------------
type ParamTypes = [(Word32)]
-- PoolDoubles -------------------------------------------------
type PoolDoubles = [(Double)]
-- PoolInfo ----------------------------------------------------
data PoolInfo = PoolInfo_Info !(PoolInts) !(PoolUInts) !(PoolDoubles) !(PoolStrings) !(NamespaceInfos) !(SetInfos) !(MultinameInfos)
              deriving ( Eq,Ord,Show)
-- PoolInts ----------------------------------------------------
type PoolInts = [(Word32)]
-- PoolStrings -------------------------------------------------
type PoolStrings = [(ByteString)]
-- PoolUInts ---------------------------------------------------
type PoolUInts = [(Word32)]
-- Rect --------------------------------------------------------
data Rect = Rect_Rect !(Int) !(Word32) !(Word32) !(Word32) !(Word32)
          deriving ( Eq,Ord,Show)
-- ScriptInfo --------------------------------------------------
data ScriptInfo = ScriptInfo_Info !(Word32) !(Traits)
                deriving ( Eq,Ord,Show)
-- ScriptInfos -------------------------------------------------
type ScriptInfos = [ScriptInfo]
-- SetInfo -----------------------------------------------------
data SetInfo = SetInfo_Info !(NamespaceNames)
             deriving ( Eq,Ord,Show)
-- SetInfos ----------------------------------------------------
type SetInfos = [SetInfo]
-- SwfFile -----------------------------------------------------
data SwfFile = SwfFile_File !(Bool) !(Word8) !(Word32) !(Rect) !(Word16) !(Word16) !(Tags)
             deriving ( Eq,Ord,Show)
-- Tag ---------------------------------------------------------
data Tag = Tag_Abc !(AbcFlags) !(ByteString) !(AbcFile)
         | Tag_FileAttributes !(Bool) !(Bool) !(Bool) !(Bool) !(Bool)
         | Tag_Opaque !(TagKind) !(Word32) !(ByteString)
         | Tag_End
         deriving ( Eq,Ord,Show)
-- TagKind -----------------------------------------------------
data TagKind = TagKind_End
             | TagKind_ShowFrame
             | TagKind_DefineShape
             | TagKind_PlaceObject
             | TagKind_RemoveObject
             | TagKind_DefineBits
             | TagKind_DefineButton
             | TagKind_JPEGTables
             | TagKind_SetBackgroundColor
             | TagKind_DefineFont
             | TagKind_DefineText
             | TagKind_DoAction
             | TagKind_DefineFontInfo
             | TagKind_DefineSound
             | TagKind_StartSound
             | TagKind_DefineButtonSound
             | TagKind_SoundStreamHead
             | TagKind_SoundStreamBlock
             | TagKind_DefineBitsLossless
             | TagKind_DefineBitsJPEG2
             | TagKind_DefineShape2
             | TagKind_DefineButtonCxform
             | TagKind_Protect
             | TagKind_PlaceObject2
             | TagKind_RemoveObject2
             | TagKind_DefineShape3
             | TagKind_DefineText2
             | TagKind_DefineButton2
             | TagKind_DefineBitsJPEG3
             | TagKind_DefineBitsLossless2
             | TagKind_DefineEditText
             | TagKind_DefineSprite
             | TagKind_FrameLabel
             | TagKind_SoundStreamHead2
             | TagKind_DefineMorphShape
             | TagKind_DefineFont2
             | TagKind_ExportAssets
             | TagKind_ImportAssets
             | TagKind_EnableDebugger
             | TagKind_DoInitAction
             | TagKind_DefineVideoStream
             | TagKind_VideoFrame
             | TagKind_DefineFontInfo2
             | TagKind_EnableDebugger2
             | TagKind_ScriptLimits
             | TagKind_SetTabIndex
             | TagKind_FileAttributes
             | TagKind_PlaceObject3
             | TagKind_ImportAssets2
             | TagKind_DefineFontAlignZones
             | TagKind_CSMTextSettings
             | TagKind_DefineFont3
             | TagKind_SymbolClass
             | TagKind_Metadata
             | TagKind_DefineScalingGrid
             | TagKind_DoABC
             | TagKind_DefineShape4
             | TagKind_DefineMorphShape2
             | TagKind_DefineSceneAndFrameLabelData
             | TagKind_DefineBinaryData
             | TagKind_DefineFontName
             | TagKind_StartSound2
             | TagKind_DefineBitsJPEG4
             | TagKind_DefineFont4
             | TagKind_Other !(Word16)
             deriving ( Eq,Ord,Show)
-- Tags --------------------------------------------------------
type Tags = [Tag]
-- Trait -------------------------------------------------------
data Trait = Trait_Trait !(Word32) !(TraitData) !(TraitAttrs) !(TraitMeta)
           deriving ( Eq,Ord,Show)
-- TraitAttr ---------------------------------------------------
data TraitAttr = TraitAttr_Final
               | TraitAttr_Override
               | TraitAttr_Metadata
               deriving ( Eq,Ord,Show)
-- TraitAttrs --------------------------------------------------
type TraitAttrs = [TraitAttr]
-- TraitData ---------------------------------------------------
data TraitData = TraitData_Slot !(Word32) !(Word32) !(Word32) !(ValueKind)
               | TraitData_Const !(Word32) !(Word32) !(Word32) !(ValueKind)
               | TraitData_Method !(Word32) !(Word32)
               | TraitData_Getter !(Word32) !(Word32)
               | TraitData_Setter !(Word32) !(Word32)
               | TraitData_Function !(Word32) !(Word32)
               | TraitData_Class !(Word32) !(Word32)
               deriving ( Eq,Ord,Show)
-- TraitKind ---------------------------------------------------
data TraitKind = TraitKind_Slot
               | TraitKind_Method
               | TraitKind_Getter
               | TraitKind_Setter
               | TraitKind_Class
               | TraitKind_Function
               | TraitKind_Const
               deriving ( Eq,Ord,Show)
-- TraitMeta ---------------------------------------------------
type TraitMeta = [(Word32)]
-- Traits ------------------------------------------------------
type Traits = [Trait]
-- ValueKind ---------------------------------------------------
data ValueKind = ValueKind_Int
               | ValueKind_UInt
               | ValueKind_Double
               | ValueKind_Utf8
               | ValueKind_True
               | ValueKind_False
               | ValueKind_Null
               | ValueKind_Undefined
               | ValueKind_Namespace
               | ValueKind_Package
               | ValueKind_Internal
               | ValueKind_Protected
               | ValueKind_Explicit
               | ValueKind_Static
               | ValueKind_Private
               deriving ( Eq,Ord,Show)