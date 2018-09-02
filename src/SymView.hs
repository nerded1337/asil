

-- UUAGC 0.9.52.1 (src/SymView.ag)
module SymView where

{-# LINE 6 "src\\SymView.ag" #-}

import Data.Word
import ProgInfo
import Data.Maybe
{-# LINE 12 "src/SymView.hs" #-}
{-# LINE 17 "src\\SymView.ag" #-}


tablesView :: [SymbolTables] -> FileV
tablesView tbls = FileV_File $ map tableView tbls

tableView :: SymbolTables -> TableV
tableView tbls = TableV_Table classes methods where
  classes = map (classView tbls) (map Ref $ keysEnv $ tableClasses tbls)
  methods = map (methodView tbls) (map Ref $ keysEnv $ tableSigs tbls)


classView :: SymbolTables -> ClassRef -> ClassV
classView tbls ref = res where
  c         = lookupClass ref tbls
  nm        = nameView tbls (clName c)
  mbSuper   = fmap (SuperV_Super . TypeV_Type (refNull $ fromJust $ clSuper c)) $ mbNameView tbls $ clSuper c
  itfs      = map (\tpRef -> ItfV_Itf $ TypeV_Type (refNull tpRef) $ nameView tbls tpRef) (clInterfaces c)
  dynTraits = map (traitView tbls) (clDynTraits c)
  staTraits = map (traitView tbls) (clStaTraits c)
  res       = ClassV_Class ref nm mbSuper itfs dynTraits staTraits

traitView :: SymbolTables -> TraitDescr -> TraitV
traitView tbls tr = case trData tr of
  TraitMethod mRef -> TraitV_Method nm $ sigView tbls mRef
  TraitField tpRef -> TraitV_Field nm $ TypeV_Type (refNull tpRef) $ nameView tbls tpRef
  TraitClass _     -> TraitV_Other nm
  where nm = nameView tbls (trName tr)


methodView :: SymbolTables -> MethodRef -> MethodV
methodView tbls ref = res where
  m    = lookupMethod ref tbls
  mbNm = mbStringView tbls $ sigName m
  sig  = sigView' tbls ref m
  res  = MethodV_Method mbNm sig

sigView :: SymbolTables -> MethodRef -> SigV
sigView tbls ref = sigView' tbls ref m where
  m    = lookupMethod ref tbls

sigView' :: SymbolTables -> MethodRef -> Sig -> SigV
sigView' tbls ref m = res where
  mbNm = mbStringView tbls $ sigName m
  tRef = sigReturn m
  ret  = TypeV_Type (refNull tRef) $ nameView tbls tRef
  ps   = map (paramView tbls) (sigParams m)
  res  = SigV_Sig ref ret ps

paramView :: SymbolTables -> SigParam -> ParamV
paramView tbls p = ParamV_Param (mbStringView tbls $ spName p) (TypeV_Type (refNull tpRef) $ nameView tbls tpRef)
  where tpRef = spType p


mbNameView :: SymbolTables -> Maybe NameRef -> MbNmV
mbNameView tbls = maybe Nothing (Just . nameView tbls)

nameView :: SymbolTables -> NameRef -> NmV
nameView tbls ref = case nmStr nm of
  Nothing -> NmV_Other ref
  Just strRef -> case nmQual nm of
    QualNs nsRef -> NmV_Qual ref (namespaceView tbls nsRef) (stringView tbls strRef)
    QualNss sRef -> case nsSpaces $ lookupNameset sRef tbls of
      [nsRef]    -> NmV_Qual ref (namespaceView tbls nsRef) (stringView tbls strRef)
      _          -> NmV_Quals ref (namesetsView tbls  sRef) (stringView tbls strRef)
    _            -> NmV_Other ref
 where nm = lookupName ref tbls

mbStringView :: SymbolTables -> Maybe StringRef -> MbStrV
mbStringView tbls = maybe Nothing (Just . stringView tbls)

stringView :: SymbolTables -> StringRef -> StrV
stringView tbls ref = StrV_Str ref (lookupString ref tbls)

namespaceView :: SymbolTables -> NamespaceRef -> NsV
namespaceView tbls ref = NsV_Ns ref (stringView tbls $ nsName $ lookupNamespace ref tbls)

namesetsView :: SymbolTables -> NamesetRef -> NsSetV
namesetsView tbls ref = NsSetV_Set ref views where
  spaces = nsSpaces $ lookupNameset ref tbls
  views  = map (namespaceView tbls) spaces
{-# LINE 94 "src/SymView.hs" #-}
-- ClassV ------------------------------------------------------
data ClassV = ClassV_Class !(ClassRef) !(NmV) !(MbSuperV) !(ItfsV) !(TraitsV) !(TraitsV)
            deriving ( Eq,Show)
-- ClassesV ----------------------------------------------------
type ClassesV = [ClassV]
-- FileV -------------------------------------------------------
data FileV = FileV_File !(TablesV)
           deriving ( Eq,Show)
-- ItfV --------------------------------------------------------
data ItfV = ItfV_Itf !(TypeV)
          deriving ( Eq,Show)
-- ItfsV -------------------------------------------------------
type ItfsV = [ItfV]
-- MbNmV -------------------------------------------------------
type MbNmV = Maybe (NmV)
-- MbStrV ------------------------------------------------------
type MbStrV = Maybe (StrV)
-- MbSuperV ----------------------------------------------------
type MbSuperV = Maybe (SuperV)
-- MethodV -----------------------------------------------------
data MethodV = MethodV_Method !(MbStrV) !(SigV)
             deriving ( Eq,Show)
-- MethodsV ----------------------------------------------------
type MethodsV = [MethodV]
-- NmV ---------------------------------------------------------
data NmV = NmV_Qual !(NameRef) !(NsV) !(StrV)
         | NmV_Quals !(NameRef) !(NsSetV) !(StrV)
         | NmV_Other !(NameRef)
         deriving ( Eq,Show)
-- NmsV --------------------------------------------------------
type NmsV = [NmV]
-- NsSetV ------------------------------------------------------
data NsSetV = NsSetV_Set !(NamesetRef) !(NssV)
            deriving ( Eq,Show)
-- NsV ---------------------------------------------------------
data NsV = NsV_Ns !(NamespaceRef) !(StrV)
         deriving ( Eq,Show)
-- NssV --------------------------------------------------------
type NssV = [NsV]
-- ParamV ------------------------------------------------------
data ParamV = ParamV_Param !(MbStrV) !(TypeV)
            deriving ( Eq,Show)
-- ParamsV -----------------------------------------------------
type ParamsV = [ParamV]
-- SigV --------------------------------------------------------
data SigV = SigV_Sig !(MethodRef) !(TypeV) !(ParamsV)
          deriving ( Eq,Show)
-- StrV --------------------------------------------------------
data StrV = StrV_Str !(StringRef) !(String)
          deriving ( Eq,Show)
-- SuperV ------------------------------------------------------
data SuperV = SuperV_Super !(TypeV)
            deriving ( Eq,Show)
-- TableV ------------------------------------------------------
data TableV = TableV_Table !(ClassesV) !(MethodsV)
            deriving ( Eq,Show)
-- TablesV -----------------------------------------------------
type TablesV = [TableV]
-- TraitV ------------------------------------------------------
data TraitV = TraitV_Method !(NmV) !(SigV)
            | TraitV_Field !(NmV) !(TypeV)
            | TraitV_Other !(NmV)
            deriving ( Eq,Show)
-- TraitsV -----------------------------------------------------
type TraitsV = [TraitV]
-- TypeV -------------------------------------------------------
data TypeV = TypeV_Type !(Bool) !(NmV)
           deriving ( Eq,Show)