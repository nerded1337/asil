

-- UUAGC 0.9.52.1 (src/SymbolTables.ag)
module SymbolTables(symInfoAbc, symInfoSwf) where

{-# LINE 16 "src\\SymbolTables.ag" #-}

import Data.ByteString.Lazy(ByteString,unpack)
import ByteCode
import Data.Monoid
import Data.Word
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Codec.Binary.UTF8.String
import Env
import ProgInfo
{-# LINE 20 "src/SymbolTables.hs" #-}
{-# LINE 34 "src\\SymbolTables.ag" #-}

-- | Extracts symbol tables from an Abc module
symInfoAbc :: AbcFile -> SymbolTables
symInfoAbc abc = tables_Syn_AbcFile syn where
  inh = Inh_AbcFile {}
  sem = sem_AbcFile abc
  syn = wrap_AbcFile sem inh

-- | Extracts all symbol tables from a flash file
symInfoSwf :: SwfFile -> [SymbolTables]
symInfoSwf swf = allTables_Syn_SwfFile syn where
  inh = Inh_SwfFile {}
  sem = sem_SwfFile swf
  syn = wrap_SwfFile sem inh
{-# LINE 36 "src/SymbolTables.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = ( ([SymbolTables]),SymbolTables)
data Inh_AbcFile = Inh_AbcFile {}
data Syn_AbcFile = Syn_AbcFile {allTables_Syn_AbcFile :: !(([SymbolTables])),tables_Syn_AbcFile :: !(SymbolTables)}
wrap_AbcFile :: T_AbcFile ->
                Inh_AbcFile ->
                Syn_AbcFile
wrap_AbcFile sem (Inh_AbcFile) =
    (let ( _lhsOallTables,_lhsOtables) | True = sem
     in  (Syn_AbcFile _lhsOallTables _lhsOtables))
sem_AbcFile_File :: Word16 ->
                    Word16 ->
                    T_PoolInfo ->
                    T_MethodInfos ->
                    T_MetaInfos ->
                    T_InstanceInfos ->
                    T_ClassInfos ->
                    T_ScriptInfos ->
                    T_BodyInfos ->
                    T_AbcFile
sem_AbcFile_File minorVersion_ majorVersion_ constantPool_ methods_ metadatas_ instances_ classes_ scripts_ bodies_ =
    (case (({-# LINE 185 "src\\SymbolTables.ag" #-}
            0
            {-# LINE 66 "src/SymbolTables.hs" #-}
            )) of
     { _classesOindex | _classesOindex `seq` (True) ->
     (case (({-# LINE 184 "src\\SymbolTables.ag" #-}
             0
             {-# LINE 71 "src/SymbolTables.hs" #-}
             )) of
      { _instancesOindex | _instancesOindex `seq` (True) ->
      (case (classes_ _classesOindex) of
       { ( _classesIgathTraits) | True ->
           (case (instances_ _instancesOindex) of
            { ( _instancesIgathInsts) | True ->
                (case (({-# LINE 180 "src\\SymbolTables.ag" #-}
                        mapEnv (\n c -> c { clStaTraits = Map.findWithDefault [] n _classesIgathTraits }) _instancesIgathInsts
                        {-# LINE 80 "src/SymbolTables.hs" #-}
                        )) of
                 { _classInfos | _classInfos `seq` (True) ->
                 (case (({-# LINE 158 "src\\SymbolTables.ag" #-}
                         0
                         {-# LINE 85 "src/SymbolTables.hs" #-}
                         )) of
                  { _methodsOsigIndex | _methodsOsigIndex `seq` (True) ->
                  (case (constantPool_) of
                   { ( _constantPoolIgathDoublePool,_constantPoolIgathIntPool,_constantPoolIgathNamePool,_constantPoolIgathNamesetsPool,_constantPoolIgathNamespacePool,_constantPoolIgathStringPool,_constantPoolIgathUIntPool) | True ->
                       (case (({-# LINE 149 "src\\SymbolTables.ag" #-}
                               singleEnv 0 (Namespace $ Ref 0) `mappend` _constantPoolIgathNamespacePool
                               {-# LINE 92 "src/SymbolTables.hs" #-}
                               )) of
                        { _spacesPool | _spacesPool `seq` (True) ->
                        (case (({-# LINE 132 "src\\SymbolTables.ag" #-}
                                singleEnv 0 (Nameset []) `mappend` _constantPoolIgathNamesetsPool
                                {-# LINE 97 "src/SymbolTables.hs" #-}
                                )) of
                         { _setsPool | _setsPool `seq` (True) ->
                         (case (({-# LINE 113 "src\\SymbolTables.ag" #-}
                                 singleEnv 0 (Name (QualNs $ Ref 0) (Just $ Ref 0)) `mappend` _constantPoolIgathNamePool
                                 {-# LINE 102 "src/SymbolTables.hs" #-}
                                 )) of
                          { _namePool | _namePool `seq` (True) ->
                          (case (({-# LINE 90 "src\\SymbolTables.ag" #-}
                                  singleEnv 0 "" `mappend` _constantPoolIgathStringPool
                                  {-# LINE 107 "src/SymbolTables.hs" #-}
                                  )) of
                           { _stringPool | _stringPool `seq` (True) ->
                           (case (({-# LINE 89 "src\\SymbolTables.ag" #-}
                                   singleEnv 0 0 `mappend` _constantPoolIgathDoublePool
                                   {-# LINE 112 "src/SymbolTables.hs" #-}
                                   )) of
                            { _doublePool | _doublePool `seq` (True) ->
                            (case (({-# LINE 88 "src\\SymbolTables.ag" #-}
                                    singleEnv 0 0 `mappend` _constantPoolIgathUIntPool
                                    {-# LINE 117 "src/SymbolTables.hs" #-}
                                    )) of
                             { _uintPool | _uintPool `seq` (True) ->
                             (case (({-# LINE 87 "src\\SymbolTables.ag" #-}
                                     singleEnv 0 0 `mappend` _constantPoolIgathIntPool
                                     {-# LINE 122 "src/SymbolTables.hs" #-}
                                     )) of
                              { _intPool | _intPool `seq` (True) ->
                              (case (methods_ _methodsOsigIndex) of
                               { ( _methodsIgathSigs) | True ->
                                   (case (({-# LINE 57 "src\\SymbolTables.ag" #-}
                                           SymbolTables _intPool     _uintPool     _doublePool     _stringPool     _namePool
                                              _spacesPool     _setsPool     _methodsIgathSigs _classInfos
                                           {-# LINE 130 "src/SymbolTables.hs" #-}
                                           )) of
                                    { _tables | _tables `seq` (True) ->
                                    (case (({-# LINE 61 "src\\SymbolTables.ag" #-}
                                            [_tables    ]
                                            {-# LINE 135 "src/SymbolTables.hs" #-}
                                            )) of
                                     { _allTables | _allTables `seq` (True) ->
                                     (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
                                             _allTables
                                             {-# LINE 140 "src/SymbolTables.hs" #-}
                                             )) of
                                      { _lhsOallTables | _lhsOallTables `seq` (True) ->
                                      (case (({-# LINE 55 "src\\SymbolTables.ag" #-}
                                              _tables
                                              {-# LINE 145 "src/SymbolTables.hs" #-}
                                              )) of
                                       { _lhsOtables | _lhsOtables `seq` (True) ->
                                       ( _lhsOallTables,_lhsOtables) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })
-- AbcFlag -----------------------------------------------------
-- cata
sem_AbcFlag :: AbcFlag ->
               T_AbcFlag
sem_AbcFlag (AbcFlag_LazyInit) =
    (sem_AbcFlag_LazyInit)
-- semantic domain
type T_AbcFlag = ( )
sem_AbcFlag_LazyInit :: T_AbcFlag
sem_AbcFlag_LazyInit =
    ( )
-- AbcFlags ----------------------------------------------------
-- cata
sem_AbcFlags :: AbcFlags ->
                T_AbcFlags
sem_AbcFlags list =
    (Prelude.foldr sem_AbcFlags_Cons sem_AbcFlags_Nil (Prelude.map sem_AbcFlag list))
-- semantic domain
type T_AbcFlags = ( )
sem_AbcFlags_Cons :: T_AbcFlag ->
                     T_AbcFlags ->
                     T_AbcFlags
sem_AbcFlags_Cons hd_ tl_ =
    ( )
sem_AbcFlags_Nil :: T_AbcFlags
sem_AbcFlags_Nil =
    ( )
-- BodyInfo ----------------------------------------------------
-- cata
sem_BodyInfo :: BodyInfo ->
                T_BodyInfo
sem_BodyInfo (BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth _instructions _exceptions _traits) =
    (sem_BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth (sem_Instructions _instructions) (sem_Exceptions _exceptions) (sem_Traits _traits))
-- semantic domain
type T_BodyInfo = ( )
sem_BodyInfo_Info :: Word32 ->
                     Word32 ->
                     Word32 ->
                     Word32 ->
                     Word32 ->
                     T_Instructions ->
                     T_Exceptions ->
                     T_Traits ->
                     T_BodyInfo
sem_BodyInfo_Info method_ maxStack_ localCount_ initScopeDepth_ maxScopeDepth_ instructions_ exceptions_ traits_ =
    ( )
-- BodyInfos ---------------------------------------------------
-- cata
sem_BodyInfos :: BodyInfos ->
                 T_BodyInfos
sem_BodyInfos list =
    (Prelude.foldr sem_BodyInfos_Cons sem_BodyInfos_Nil (Prelude.map sem_BodyInfo list))
-- semantic domain
type T_BodyInfos = ( )
sem_BodyInfos_Cons :: T_BodyInfo ->
                      T_BodyInfos ->
                      T_BodyInfos
sem_BodyInfos_Cons hd_ tl_ =
    ( )
sem_BodyInfos_Nil :: T_BodyInfos
sem_BodyInfos_Nil =
    ( )
-- CaseOffsets -------------------------------------------------
-- cata
sem_CaseOffsets :: CaseOffsets ->
                   T_CaseOffsets
sem_CaseOffsets list =
    (Prelude.foldr sem_CaseOffsets_Cons sem_CaseOffsets_Nil list)
-- semantic domain
type T_CaseOffsets = ( )
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    ( )
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    ( )
-- ClassInfo ---------------------------------------------------
-- cata
sem_ClassInfo :: ClassInfo ->
                 T_ClassInfo
sem_ClassInfo (ClassInfo_Info _con _traits) =
    (sem_ClassInfo_Info _con (sem_Traits _traits))
-- semantic domain
type T_ClassInfo = ( TraitDescrs)
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    (case (traits_) of
     { ( _traitsIgathInfos) | True ->
         (case (({-# LINE 210 "src\\SymbolTables.ag" #-}
                 _traitsIgathInfos
                 {-# LINE 243 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOgathInfos | _lhsOgathInfos `seq` (True) ->
          ( _lhsOgathInfos) }) })
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = Word32 ->
                    ( (Map Word32 TraitDescrs))
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    (\ _lhsIindex ->
         (case (({-# LINE 188 "src\\SymbolTables.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 263 "src/SymbolTables.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (tl_ _tlOindex) of
           { ( _tlIgathTraits) | True ->
               (case (hd_) of
                { ( _hdIgathInfos) | True ->
                    (case (({-# LINE 207 "src\\SymbolTables.ag" #-}
                            Map.insert _lhsIindex _hdIgathInfos _tlIgathTraits
                            {-# LINE 272 "src/SymbolTables.hs" #-}
                            )) of
                     { _lhsOgathTraits | _lhsOgathTraits `seq` (True) ->
                     ( _lhsOgathTraits) }) }) }) }))
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    (\ _lhsIindex ->
         (case (({-# LINE 206 "src\\SymbolTables.ag" #-}
                 mempty
                 {-# LINE 281 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOgathTraits | _lhsOgathTraits `seq` (True) ->
          ( _lhsOgathTraits) }))
-- DebugType ---------------------------------------------------
-- cata
sem_DebugType :: DebugType ->
                 T_DebugType
sem_DebugType (DebugType_Local) =
    (sem_DebugType_Local)
-- semantic domain
type T_DebugType = ( )
sem_DebugType_Local :: T_DebugType
sem_DebugType_Local =
    ( )
-- Exception ---------------------------------------------------
-- cata
sem_Exception :: Exception ->
                 T_Exception
sem_Exception (Exception_Info _from _to _target _tp _name) =
    (sem_Exception_Info _from _to _target _tp _name)
-- semantic domain
type T_Exception = ( )
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    ( )
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = ( )
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    ( )
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    ( )
-- InstanceFlag ------------------------------------------------
-- cata
sem_InstanceFlag :: InstanceFlag ->
                    T_InstanceFlag
sem_InstanceFlag (InstanceFlag_ClassSealed) =
    (sem_InstanceFlag_ClassSealed)
sem_InstanceFlag (InstanceFlag_ClassFinal) =
    (sem_InstanceFlag_ClassFinal)
sem_InstanceFlag (InstanceFlag_ClassInterface) =
    (sem_InstanceFlag_ClassInterface)
sem_InstanceFlag (InstanceFlag_ClassProtected) =
    (sem_InstanceFlag_ClassProtected)
-- semantic domain
type T_InstanceFlag = ( )
sem_InstanceFlag_ClassSealed :: T_InstanceFlag
sem_InstanceFlag_ClassSealed =
    ( )
sem_InstanceFlag_ClassFinal :: T_InstanceFlag
sem_InstanceFlag_ClassFinal =
    ( )
sem_InstanceFlag_ClassInterface :: T_InstanceFlag
sem_InstanceFlag_ClassInterface =
    ( )
sem_InstanceFlag_ClassProtected :: T_InstanceFlag
sem_InstanceFlag_ClassProtected =
    ( )
-- InstanceFlags -----------------------------------------------
-- cata
sem_InstanceFlags :: InstanceFlags ->
                     T_InstanceFlags
sem_InstanceFlags list =
    (Prelude.foldr sem_InstanceFlags_Cons sem_InstanceFlags_Nil (Prelude.map sem_InstanceFlag list))
-- semantic domain
type T_InstanceFlags = ( )
sem_InstanceFlags_Cons :: T_InstanceFlag ->
                          T_InstanceFlags ->
                          T_InstanceFlags
sem_InstanceFlags_Cons hd_ tl_ =
    ( )
sem_InstanceFlags_Nil :: T_InstanceFlags
sem_InstanceFlags_Nil =
    ( )
-- InstanceInfo ------------------------------------------------
-- cata
sem_InstanceInfo :: InstanceInfo ->
                    T_InstanceInfo
sem_InstanceInfo (InstanceInfo_Info _name _super _flags _protectedNs _interfaces _constructor _traits) =
    (sem_InstanceInfo_Info _name _super (sem_InstanceFlags _flags) _protectedNs (sem_Interfaces _interfaces) _constructor (sem_Traits _traits))
-- semantic domain
type T_InstanceInfo = ( ClassDescr)
sem_InstanceInfo_Info :: Word32 ->
                         Word32 ->
                         T_InstanceFlags ->
                         Word32 ->
                         T_Interfaces ->
                         Word32 ->
                         T_Traits ->
                         T_InstanceInfo
sem_InstanceInfo_Info name_ super_ flags_ protectedNs_ interfaces_ constructor_ traits_ =
    (case (traits_) of
     { ( _traitsIgathInfos) | True ->
         (case (interfaces_) of
          { ( _interfacesIitfs) | True ->
              (case (({-# LINE 198 "src\\SymbolTables.ag" #-}
                      ClassDescr (Ref name_) (if super_ /= 0 then Just $ Ref super_ else Nothing) (map Ref _interfacesIitfs) _traitsIgathInfos []
                      {-# LINE 393 "src/SymbolTables.hs" #-}
                      )) of
               { _lhsOclassInfo | _lhsOclassInfo `seq` (True) ->
               ( _lhsOclassInfo) }) }) })
-- InstanceInfos -----------------------------------------------
-- cata
sem_InstanceInfos :: InstanceInfos ->
                     T_InstanceInfos
sem_InstanceInfos list =
    (Prelude.foldr sem_InstanceInfos_Cons sem_InstanceInfos_Nil (Prelude.map sem_InstanceInfo list))
-- semantic domain
type T_InstanceInfos = Word32 ->
                       ( ClassDescrs)
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    (\ _lhsIindex ->
         (case (({-# LINE 187 "src\\SymbolTables.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 413 "src/SymbolTables.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (tl_ _tlOindex) of
           { ( _tlIgathInsts) | True ->
               (case (hd_) of
                { ( _hdIclassInfo) | True ->
                    (case (({-# LINE 194 "src\\SymbolTables.ag" #-}
                            singleEnv _lhsIindex _hdIclassInfo `mappend` _tlIgathInsts
                            {-# LINE 422 "src/SymbolTables.hs" #-}
                            )) of
                     { _lhsOgathInsts | _lhsOgathInsts `seq` (True) ->
                     ( _lhsOgathInsts) }) }) }) }))
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    (\ _lhsIindex ->
         (case (({-# LINE 193 "src\\SymbolTables.ag" #-}
                 mempty
                 {-# LINE 431 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOgathInsts | _lhsOgathInsts `seq` (True) ->
          ( _lhsOgathInsts) }))
-- Instruction -------------------------------------------------
-- cata
sem_Instruction :: Instruction ->
                   T_Instruction
sem_Instruction (Instruction_Add) =
    (sem_Instruction_Add)
sem_Instruction (Instruction_Add_i) =
    (sem_Instruction_Add_i)
sem_Instruction (Instruction_Add_d) =
    (sem_Instruction_Add_d)
sem_Instruction (Instruction_ApplyType _name) =
    (sem_Instruction_ApplyType _name)
sem_Instruction (Instruction_AsType _name) =
    (sem_Instruction_AsType _name)
sem_Instruction (Instruction_AsTypeLate) =
    (sem_Instruction_AsTypeLate)
sem_Instruction (Instruction_Breakpoint) =
    (sem_Instruction_Breakpoint)
sem_Instruction (Instruction_BreakLine _line) =
    (sem_Instruction_BreakLine _line)
sem_Instruction (Instruction_BitAnd) =
    (sem_Instruction_BitAnd)
sem_Instruction (Instruction_BitNot) =
    (sem_Instruction_BitNot)
sem_Instruction (Instruction_BitOr) =
    (sem_Instruction_BitOr)
sem_Instruction (Instruction_BitXor) =
    (sem_Instruction_BitXor)
sem_Instruction (Instruction_Call _argCount) =
    (sem_Instruction_Call _argCount)
sem_Instruction (Instruction_CallInterface _name _argCount) =
    (sem_Instruction_CallInterface _name _argCount)
sem_Instruction (Instruction_CallMethod _index _argCount) =
    (sem_Instruction_CallMethod _index _argCount)
sem_Instruction (Instruction_CallProp _name _argCount) =
    (sem_Instruction_CallProp _name _argCount)
sem_Instruction (Instruction_CallPropLex _name _argCount) =
    (sem_Instruction_CallPropLex _name _argCount)
sem_Instruction (Instruction_CallPropVoid _name _argCount) =
    (sem_Instruction_CallPropVoid _name _argCount)
sem_Instruction (Instruction_CallStatic _method _argCount) =
    (sem_Instruction_CallStatic _method _argCount)
sem_Instruction (Instruction_CallSuper _name _argCount) =
    (sem_Instruction_CallSuper _name _argCount)
sem_Instruction (Instruction_CallSuperId) =
    (sem_Instruction_CallSuperId)
sem_Instruction (Instruction_CallSuperVoid _name _argCount) =
    (sem_Instruction_CallSuperVoid _name _argCount)
sem_Instruction (Instruction_CheckFilter) =
    (sem_Instruction_CheckFilter)
sem_Instruction (Instruction_Coerce _name) =
    (sem_Instruction_Coerce _name)
sem_Instruction (Instruction_Coerce_a) =
    (sem_Instruction_Coerce_a)
sem_Instruction (Instruction_Coerce_b) =
    (sem_Instruction_Coerce_b)
sem_Instruction (Instruction_Coerce_d) =
    (sem_Instruction_Coerce_d)
sem_Instruction (Instruction_Coerce_i) =
    (sem_Instruction_Coerce_i)
sem_Instruction (Instruction_Coerce_o) =
    (sem_Instruction_Coerce_o)
sem_Instruction (Instruction_Coerce_s) =
    (sem_Instruction_Coerce_s)
sem_Instruction (Instruction_Coerce_u) =
    (sem_Instruction_Coerce_u)
sem_Instruction (Instruction_Concat) =
    (sem_Instruction_Concat)
sem_Instruction (Instruction_Construct _argCount) =
    (sem_Instruction_Construct _argCount)
sem_Instruction (Instruction_ConstructProp _name _argCount) =
    (sem_Instruction_ConstructProp _name _argCount)
sem_Instruction (Instruction_ConstructSuper _argCount) =
    (sem_Instruction_ConstructSuper _argCount)
sem_Instruction (Instruction_Convert_b) =
    (sem_Instruction_Convert_b)
sem_Instruction (Instruction_Convert_i) =
    (sem_Instruction_Convert_i)
sem_Instruction (Instruction_Convert_d) =
    (sem_Instruction_Convert_d)
sem_Instruction (Instruction_Convert_o) =
    (sem_Instruction_Convert_o)
sem_Instruction (Instruction_Convert_u) =
    (sem_Instruction_Convert_u)
sem_Instruction (Instruction_Convert_s) =
    (sem_Instruction_Convert_s)
sem_Instruction (Instruction_Debug _tp _name _reg _extra) =
    (sem_Instruction_Debug (sem_DebugType _tp) _name _reg _extra)
sem_Instruction (Instruction_DebugFile _name) =
    (sem_Instruction_DebugFile _name)
sem_Instruction (Instruction_DebugLine _line) =
    (sem_Instruction_DebugLine _line)
sem_Instruction (Instruction_DecLocal _reg) =
    (sem_Instruction_DecLocal _reg)
sem_Instruction (Instruction_DecLocal_i _reg) =
    (sem_Instruction_DecLocal_i _reg)
sem_Instruction (Instruction_Decrement) =
    (sem_Instruction_Decrement)
sem_Instruction (Instruction_Decrement_i) =
    (sem_Instruction_Decrement_i)
sem_Instruction (Instruction_DeleteProperty _name) =
    (sem_Instruction_DeleteProperty _name)
sem_Instruction (Instruction_DeletePropertyLate) =
    (sem_Instruction_DeletePropertyLate)
sem_Instruction (Instruction_Divide) =
    (sem_Instruction_Divide)
sem_Instruction (Instruction_Dup) =
    (sem_Instruction_Dup)
sem_Instruction (Instruction_Dxns _name) =
    (sem_Instruction_Dxns _name)
sem_Instruction (Instruction_DxnsLate) =
    (sem_Instruction_DxnsLate)
sem_Instruction (Instruction_Equals) =
    (sem_Instruction_Equals)
sem_Instruction (Instruction_EscXAttr) =
    (sem_Instruction_EscXAttr)
sem_Instruction (Instruction_EscXElem) =
    (sem_Instruction_EscXElem)
sem_Instruction (Instruction_FindDef _name) =
    (sem_Instruction_FindDef _name)
sem_Instruction (Instruction_FindPropertyGlobalStrict _name) =
    (sem_Instruction_FindPropertyGlobalStrict _name)
sem_Instruction (Instruction_FindPropertyGlobal _name) =
    (sem_Instruction_FindPropertyGlobal _name)
sem_Instruction (Instruction_FindProperty _name) =
    (sem_Instruction_FindProperty _name)
sem_Instruction (Instruction_FindPropStrict _name) =
    (sem_Instruction_FindPropStrict _name)
sem_Instruction (Instruction_GetDescendants _name) =
    (sem_Instruction_GetDescendants _name)
sem_Instruction (Instruction_GetGlobalScope) =
    (sem_Instruction_GetGlobalScope)
sem_Instruction (Instruction_GetGlobalSlot _slot) =
    (sem_Instruction_GetGlobalSlot _slot)
sem_Instruction (Instruction_GetLex _name) =
    (sem_Instruction_GetLex _name)
sem_Instruction (Instruction_GetLocal _reg) =
    (sem_Instruction_GetLocal _reg)
sem_Instruction (Instruction_GetLocal0) =
    (sem_Instruction_GetLocal0)
sem_Instruction (Instruction_GetLocal1) =
    (sem_Instruction_GetLocal1)
sem_Instruction (Instruction_GetLocal2) =
    (sem_Instruction_GetLocal2)
sem_Instruction (Instruction_GetLocal3) =
    (sem_Instruction_GetLocal3)
sem_Instruction (Instruction_GetOuterScope _name) =
    (sem_Instruction_GetOuterScope _name)
sem_Instruction (Instruction_GetProperty _name) =
    (sem_Instruction_GetProperty _name)
sem_Instruction (Instruction_GetScopeObject _index) =
    (sem_Instruction_GetScopeObject _index)
sem_Instruction (Instruction_GetSlot _slot) =
    (sem_Instruction_GetSlot _slot)
sem_Instruction (Instruction_GetSuper _name) =
    (sem_Instruction_GetSuper _name)
sem_Instruction (Instruction_GreaterEquals) =
    (sem_Instruction_GreaterEquals)
sem_Instruction (Instruction_GreaterThan) =
    (sem_Instruction_GreaterThan)
sem_Instruction (Instruction_HasNext) =
    (sem_Instruction_HasNext)
sem_Instruction (Instruction_HasNext2 _objectReg _indexReg) =
    (sem_Instruction_HasNext2 _objectReg _indexReg)
sem_Instruction (Instruction_IfEq _offset) =
    (sem_Instruction_IfEq _offset)
sem_Instruction (Instruction_IfFalse _offset) =
    (sem_Instruction_IfFalse _offset)
sem_Instruction (Instruction_IfGe _offset) =
    (sem_Instruction_IfGe _offset)
sem_Instruction (Instruction_IfGt _offset) =
    (sem_Instruction_IfGt _offset)
sem_Instruction (Instruction_IfLe _offset) =
    (sem_Instruction_IfLe _offset)
sem_Instruction (Instruction_IfLt _offset) =
    (sem_Instruction_IfLt _offset)
sem_Instruction (Instruction_IfNGe _offset) =
    (sem_Instruction_IfNGe _offset)
sem_Instruction (Instruction_IfNGt _offset) =
    (sem_Instruction_IfNGt _offset)
sem_Instruction (Instruction_IfNLe _offset) =
    (sem_Instruction_IfNLe _offset)
sem_Instruction (Instruction_IfNLt _offset) =
    (sem_Instruction_IfNLt _offset)
sem_Instruction (Instruction_IfNe _offset) =
    (sem_Instruction_IfNe _offset)
sem_Instruction (Instruction_IfStrictEq _offset) =
    (sem_Instruction_IfStrictEq _offset)
sem_Instruction (Instruction_IfStrictNe _offset) =
    (sem_Instruction_IfStrictNe _offset)
sem_Instruction (Instruction_IfTrue _offset) =
    (sem_Instruction_IfTrue _offset)
sem_Instruction (Instruction_In) =
    (sem_Instruction_In)
sem_Instruction (Instruction_IncLocal _reg) =
    (sem_Instruction_IncLocal _reg)
sem_Instruction (Instruction_IncLocal_i _reg) =
    (sem_Instruction_IncLocal_i _reg)
sem_Instruction (Instruction_Increment) =
    (sem_Instruction_Increment)
sem_Instruction (Instruction_Increment_i) =
    (sem_Instruction_Increment_i)
sem_Instruction (Instruction_InitProperty _name) =
    (sem_Instruction_InitProperty _name)
sem_Instruction (Instruction_InstanceOf) =
    (sem_Instruction_InstanceOf)
sem_Instruction (Instruction_IsType _name) =
    (sem_Instruction_IsType _name)
sem_Instruction (Instruction_IsTypeLate) =
    (sem_Instruction_IsTypeLate)
sem_Instruction (Instruction_Jump _offset) =
    (sem_Instruction_Jump _offset)
sem_Instruction (Instruction_Kill _reg) =
    (sem_Instruction_Kill _reg)
sem_Instruction (Instruction_Label) =
    (sem_Instruction_Label)
sem_Instruction (Instruction_LessEquals) =
    (sem_Instruction_LessEquals)
sem_Instruction (Instruction_LessThan) =
    (sem_Instruction_LessThan)
sem_Instruction (Instruction_LoadFloat32) =
    (sem_Instruction_LoadFloat32)
sem_Instruction (Instruction_LoadFloat64) =
    (sem_Instruction_LoadFloat64)
sem_Instruction (Instruction_LoadIndirect8) =
    (sem_Instruction_LoadIndirect8)
sem_Instruction (Instruction_LoadIndirect16) =
    (sem_Instruction_LoadIndirect16)
sem_Instruction (Instruction_LoadIndirect32) =
    (sem_Instruction_LoadIndirect32)
sem_Instruction (Instruction_LookupSwitch _defaultOffset _caseOffsets) =
    (sem_Instruction_LookupSwitch _defaultOffset (sem_CaseOffsets _caseOffsets))
sem_Instruction (Instruction_Lshift) =
    (sem_Instruction_Lshift)
sem_Instruction (Instruction_Modulo) =
    (sem_Instruction_Modulo)
sem_Instruction (Instruction_Multiply) =
    (sem_Instruction_Multiply)
sem_Instruction (Instruction_Multiply_i) =
    (sem_Instruction_Multiply_i)
sem_Instruction (Instruction_Negate) =
    (sem_Instruction_Negate)
sem_Instruction (Instruction_Negate_i) =
    (sem_Instruction_Negate_i)
sem_Instruction (Instruction_NewActivation) =
    (sem_Instruction_NewActivation)
sem_Instruction (Instruction_NewArray _argCount) =
    (sem_Instruction_NewArray _argCount)
sem_Instruction (Instruction_NewCatch _exception) =
    (sem_Instruction_NewCatch _exception)
sem_Instruction (Instruction_NewClass _class) =
    (sem_Instruction_NewClass _class)
sem_Instruction (Instruction_NewFunction _method) =
    (sem_Instruction_NewFunction _method)
sem_Instruction (Instruction_NewObject _argCount) =
    (sem_Instruction_NewObject _argCount)
sem_Instruction (Instruction_NextName) =
    (sem_Instruction_NextName)
sem_Instruction (Instruction_NextValue) =
    (sem_Instruction_NextValue)
sem_Instruction (Instruction_Nop) =
    (sem_Instruction_Nop)
sem_Instruction (Instruction_Not) =
    (sem_Instruction_Not)
sem_Instruction (Instruction_Pop) =
    (sem_Instruction_Pop)
sem_Instruction (Instruction_PopScope) =
    (sem_Instruction_PopScope)
sem_Instruction (Instruction_PushByte _val) =
    (sem_Instruction_PushByte _val)
sem_Instruction (Instruction_PushDouble _name) =
    (sem_Instruction_PushDouble _name)
sem_Instruction (Instruction_PushFalse) =
    (sem_Instruction_PushFalse)
sem_Instruction (Instruction_PushInt _name) =
    (sem_Instruction_PushInt _name)
sem_Instruction (Instruction_PushNamespace _name) =
    (sem_Instruction_PushNamespace _name)
sem_Instruction (Instruction_PushNaN) =
    (sem_Instruction_PushNaN)
sem_Instruction (Instruction_PushNull) =
    (sem_Instruction_PushNull)
sem_Instruction (Instruction_PushScope) =
    (sem_Instruction_PushScope)
sem_Instruction (Instruction_PushShort _val) =
    (sem_Instruction_PushShort _val)
sem_Instruction (Instruction_PushString _name) =
    (sem_Instruction_PushString _name)
sem_Instruction (Instruction_PushTrue) =
    (sem_Instruction_PushTrue)
sem_Instruction (Instruction_PushUInt _name) =
    (sem_Instruction_PushUInt _name)
sem_Instruction (Instruction_PushUndefined) =
    (sem_Instruction_PushUndefined)
sem_Instruction (Instruction_PushWith) =
    (sem_Instruction_PushWith)
sem_Instruction (Instruction_ReturnValue) =
    (sem_Instruction_ReturnValue)
sem_Instruction (Instruction_ReturnVoid) =
    (sem_Instruction_ReturnVoid)
sem_Instruction (Instruction_Rshift) =
    (sem_Instruction_Rshift)
sem_Instruction (Instruction_SetLocal _reg) =
    (sem_Instruction_SetLocal _reg)
sem_Instruction (Instruction_SetLocal0) =
    (sem_Instruction_SetLocal0)
sem_Instruction (Instruction_SetLocal1) =
    (sem_Instruction_SetLocal1)
sem_Instruction (Instruction_SetLocal2) =
    (sem_Instruction_SetLocal2)
sem_Instruction (Instruction_SetLocal3) =
    (sem_Instruction_SetLocal3)
sem_Instruction (Instruction_SetGlobalSlot _slot) =
    (sem_Instruction_SetGlobalSlot _slot)
sem_Instruction (Instruction_SetProperty _name) =
    (sem_Instruction_SetProperty _name)
sem_Instruction (Instruction_SetPropertyLate) =
    (sem_Instruction_SetPropertyLate)
sem_Instruction (Instruction_SetSlot _slot) =
    (sem_Instruction_SetSlot _slot)
sem_Instruction (Instruction_SetSuper _name) =
    (sem_Instruction_SetSuper _name)
sem_Instruction (Instruction_SignExtend1) =
    (sem_Instruction_SignExtend1)
sem_Instruction (Instruction_SignExtend8) =
    (sem_Instruction_SignExtend8)
sem_Instruction (Instruction_SignExtend16) =
    (sem_Instruction_SignExtend16)
sem_Instruction (Instruction_StoreFloat32) =
    (sem_Instruction_StoreFloat32)
sem_Instruction (Instruction_StoreFloat64) =
    (sem_Instruction_StoreFloat64)
sem_Instruction (Instruction_StoreIndirect32) =
    (sem_Instruction_StoreIndirect32)
sem_Instruction (Instruction_StoreIndirect16) =
    (sem_Instruction_StoreIndirect16)
sem_Instruction (Instruction_StoreIndirect8) =
    (sem_Instruction_StoreIndirect8)
sem_Instruction (Instruction_StrictEquals) =
    (sem_Instruction_StrictEquals)
sem_Instruction (Instruction_Substract) =
    (sem_Instruction_Substract)
sem_Instruction (Instruction_Substract_i) =
    (sem_Instruction_Substract_i)
sem_Instruction (Instruction_Swap) =
    (sem_Instruction_Swap)
sem_Instruction (Instruction_Throw) =
    (sem_Instruction_Throw)
sem_Instruction (Instruction_Timestamp) =
    (sem_Instruction_Timestamp)
sem_Instruction (Instruction_TypeOf) =
    (sem_Instruction_TypeOf)
sem_Instruction (Instruction_Urshift) =
    (sem_Instruction_Urshift)
sem_Instruction (Instruction_Location _index) =
    (sem_Instruction_Location _index)
-- semantic domain
type T_Instruction = ( )
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    ( )
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    ( )
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    ( )
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    ( )
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    ( )
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    ( )
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    ( )
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    ( )
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    ( )
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    ( )
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    ( )
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    ( )
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    ( )
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    ( )
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    ( )
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    ( )
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    ( )
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    ( )
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    ( )
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    ( )
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    ( )
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    ( )
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    ( )
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    ( )
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    ( )
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    ( )
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    ( )
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    ( )
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    ( )
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    ( )
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    ( )
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    ( )
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    ( )
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    ( )
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    ( )
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    ( )
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    ( )
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    ( )
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    ( )
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    ( )
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    ( )
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    ( )
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    ( )
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    ( )
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    ( )
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    ( )
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    ( )
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    ( )
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    ( )
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    ( )
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    ( )
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    ( )
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    ( )
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    ( )
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    ( )
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    ( )
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    ( )
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    ( )
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    ( )
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    ( )
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    ( )
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    ( )
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    ( )
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    ( )
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    ( )
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    ( )
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    ( )
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    ( )
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    ( )
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    ( )
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    ( )
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    ( )
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    ( )
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    ( )
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    ( )
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    ( )
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    ( )
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    ( )
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    ( )
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    ( )
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    ( )
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    ( )
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    ( )
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    ( )
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    ( )
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    ( )
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    ( )
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    ( )
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    ( )
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    ( )
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    ( )
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    ( )
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    ( )
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    ( )
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    ( )
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    ( )
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    ( )
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    ( )
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    ( )
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    ( )
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    ( )
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    ( )
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    ( )
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    ( )
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    ( )
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    ( )
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    ( )
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    ( )
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    ( )
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    ( )
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    ( )
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    ( )
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    ( )
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    ( )
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    ( )
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    ( )
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    ( )
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    ( )
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    ( )
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    ( )
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    ( )
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    ( )
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    ( )
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    ( )
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    ( )
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    ( )
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    ( )
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    ( )
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    ( )
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    ( )
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    ( )
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    ( )
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    ( )
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    ( )
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    ( )
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    ( )
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    ( )
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    ( )
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    ( )
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    ( )
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    ( )
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    ( )
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    ( )
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    ( )
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    ( )
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    ( )
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    ( )
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    ( )
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    ( )
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    ( )
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    ( )
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    ( )
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    ( )
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    ( )
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    ( )
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    ( )
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    ( )
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    ( )
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    ( )
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    ( )
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    ( )
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    ( )
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    ( )
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    ( )
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    ( )
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    ( )
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    ( )
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    ( )
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    ( )
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    ( )
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    ( )
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    ( )
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    ( )
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    ( )
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    ( )
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    ( )
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ( )
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    ( )
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    ( )
-- Interfaces --------------------------------------------------
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil list)
-- semantic domain
type T_Interfaces = ( Interfaces)
sem_Interfaces_Cons :: Word32 ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIitfs) | True ->
         (case (({-# LINE 200 "src\\SymbolTables.ag" #-}
                 (:) hd_ _tlIitfs
                 {-# LINE 1444 "src/SymbolTables.hs" #-}
                 )) of
          { _itfs | _itfs `seq` (True) ->
          (case (({-# LINE 200 "src\\SymbolTables.ag" #-}
                  _itfs
                  {-# LINE 1449 "src/SymbolTables.hs" #-}
                  )) of
           { _lhsOitfs | _lhsOitfs `seq` (True) ->
           ( _lhsOitfs) }) }) })
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (case (({-# LINE 200 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 1457 "src/SymbolTables.hs" #-}
            )) of
     { _itfs | _itfs `seq` (True) ->
     (case (({-# LINE 200 "src\\SymbolTables.ag" #-}
             _itfs
             {-# LINE 1462 "src/SymbolTables.hs" #-}
             )) of
      { _lhsOitfs | _lhsOitfs `seq` (True) ->
      ( _lhsOitfs) }) })
-- MetaInfo ----------------------------------------------------
-- cata
sem_MetaInfo :: MetaInfo ->
                T_MetaInfo
sem_MetaInfo (MetaInfo_Info _name _items) =
    (sem_MetaInfo_Info _name (sem_MetaItems _items))
-- semantic domain
type T_MetaInfo = ( )
sem_MetaInfo_Info :: Word32 ->
                     T_MetaItems ->
                     T_MetaInfo
sem_MetaInfo_Info name_ items_ =
    ( )
-- MetaInfos ---------------------------------------------------
-- cata
sem_MetaInfos :: MetaInfos ->
                 T_MetaInfos
sem_MetaInfos list =
    (Prelude.foldr sem_MetaInfos_Cons sem_MetaInfos_Nil (Prelude.map sem_MetaInfo list))
-- semantic domain
type T_MetaInfos = ( )
sem_MetaInfos_Cons :: T_MetaInfo ->
                      T_MetaInfos ->
                      T_MetaInfos
sem_MetaInfos_Cons hd_ tl_ =
    ( )
sem_MetaInfos_Nil :: T_MetaInfos
sem_MetaInfos_Nil =
    ( )
-- MetaItem ----------------------------------------------------
-- cata
sem_MetaItem :: MetaItem ->
                T_MetaItem
sem_MetaItem (MetaItem_Item _key _value) =
    (sem_MetaItem_Item _key _value)
-- semantic domain
type T_MetaItem = ( )
sem_MetaItem_Item :: Word32 ->
                     Word32 ->
                     T_MetaItem
sem_MetaItem_Item key_ value_ =
    ( )
-- MetaItems ---------------------------------------------------
-- cata
sem_MetaItems :: MetaItems ->
                 T_MetaItems
sem_MetaItems list =
    (Prelude.foldr sem_MetaItems_Cons sem_MetaItems_Nil (Prelude.map sem_MetaItem list))
-- semantic domain
type T_MetaItems = ( )
sem_MetaItems_Cons :: T_MetaItem ->
                      T_MetaItems ->
                      T_MetaItems
sem_MetaItems_Cons hd_ tl_ =
    ( )
sem_MetaItems_Nil :: T_MetaItems
sem_MetaItems_Nil =
    ( )
-- MethodFlag --------------------------------------------------
-- cata
sem_MethodFlag :: MethodFlag ->
                  T_MethodFlag
sem_MethodFlag (MethodFlag_NeedArgs) =
    (sem_MethodFlag_NeedArgs)
sem_MethodFlag (MethodFlag_NeedAct) =
    (sem_MethodFlag_NeedAct)
sem_MethodFlag (MethodFlag_NeedRest) =
    (sem_MethodFlag_NeedRest)
sem_MethodFlag (MethodFlag_HasOptionals) =
    (sem_MethodFlag_HasOptionals)
sem_MethodFlag (MethodFlag_SetDXNS) =
    (sem_MethodFlag_SetDXNS)
sem_MethodFlag (MethodFlag_HasParamNames) =
    (sem_MethodFlag_HasParamNames)
-- semantic domain
type T_MethodFlag = ( )
sem_MethodFlag_NeedArgs :: T_MethodFlag
sem_MethodFlag_NeedArgs =
    ( )
sem_MethodFlag_NeedAct :: T_MethodFlag
sem_MethodFlag_NeedAct =
    ( )
sem_MethodFlag_NeedRest :: T_MethodFlag
sem_MethodFlag_NeedRest =
    ( )
sem_MethodFlag_HasOptionals :: T_MethodFlag
sem_MethodFlag_HasOptionals =
    ( )
sem_MethodFlag_SetDXNS :: T_MethodFlag
sem_MethodFlag_SetDXNS =
    ( )
sem_MethodFlag_HasParamNames :: T_MethodFlag
sem_MethodFlag_HasParamNames =
    ( )
-- MethodFlags -------------------------------------------------
-- cata
sem_MethodFlags :: MethodFlags ->
                   T_MethodFlags
sem_MethodFlags list =
    (Prelude.foldr sem_MethodFlags_Cons sem_MethodFlags_Nil (Prelude.map sem_MethodFlag list))
-- semantic domain
type T_MethodFlags = ( )
sem_MethodFlags_Cons :: T_MethodFlag ->
                        T_MethodFlags ->
                        T_MethodFlags
sem_MethodFlags_Cons hd_ tl_ =
    ( )
sem_MethodFlags_Nil :: T_MethodFlags
sem_MethodFlags_Nil =
    ( )
-- MethodInfo --------------------------------------------------
-- cata
sem_MethodInfo :: MethodInfo ->
                  T_MethodInfo
sem_MethodInfo (MethodInfo_Info _return _params _name _flags _options _names) =
    (sem_MethodInfo_Info _return (sem_ParamTypes _params) _name (sem_MethodFlags _flags) (sem_Optionals _options) (sem_ParamNames _names))
-- semantic domain
type T_MethodInfo = ( Sig)
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    (case (names_) of
     { ( _namesInames) | True ->
         (case (({-# LINE 167 "src\\SymbolTables.ag" #-}
                 map (Just . Ref) _namesInames
                 {-# LINE 1596 "src/SymbolTables.hs" #-}
                 )) of
          { _mbNamesL | _mbNamesL `seq` (True) ->
          (case (params_) of
           { ( _paramsItypes) | True ->
               (case (({-# LINE 168 "src\\SymbolTables.ag" #-}
                       zipWith SigParam (_mbNamesL     ++ repeat Nothing) (map Ref _paramsItypes)
                       {-# LINE 1603 "src/SymbolTables.hs" #-}
                       )) of
                { _paramSigs | _paramSigs `seq` (True) ->
                (case (({-# LINE 169 "src\\SymbolTables.ag" #-}
                        Sig (if name_ /= 0 then Just $ Ref name_ else Nothing) (Ref return_) _paramSigs
                        {-# LINE 1608 "src/SymbolTables.hs" #-}
                        )) of
                 { _lhsOgathSig | _lhsOgathSig `seq` (True) ->
                 ( _lhsOgathSig) }) }) }) }) })
-- MethodInfos -------------------------------------------------
-- cata
sem_MethodInfos :: MethodInfos ->
                   T_MethodInfos
sem_MethodInfos list =
    (Prelude.foldr sem_MethodInfos_Cons sem_MethodInfos_Nil (Prelude.map sem_MethodInfo list))
-- semantic domain
type T_MethodInfos = Word32 ->
                     ( Sigs)
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    (\ _lhsIsigIndex ->
         (case (({-# LINE 161 "src\\SymbolTables.ag" #-}
                 1 + _lhsIsigIndex
                 {-# LINE 1628 "src/SymbolTables.hs" #-}
                 )) of
          { _tlOsigIndex | _tlOsigIndex `seq` (True) ->
          (case (tl_ _tlOsigIndex) of
           { ( _tlIgathSigs) | True ->
               (case (hd_) of
                { ( _hdIgathSig) | True ->
                    (case (({-# LINE 162 "src\\SymbolTables.ag" #-}
                            singleEnv _lhsIsigIndex _hdIgathSig `mappend` _tlIgathSigs
                            {-# LINE 1637 "src/SymbolTables.hs" #-}
                            )) of
                     { _lhsOgathSigs | _lhsOgathSigs `seq` (True) ->
                     ( _lhsOgathSigs) }) }) }) }))
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    (\ _lhsIsigIndex ->
         (case (({-# LINE 163 "src\\SymbolTables.ag" #-}
                 mempty
                 {-# LINE 1646 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOgathSigs | _lhsOgathSigs `seq` (True) ->
          ( _lhsOgathSigs) }))
-- MultinameInfo -----------------------------------------------
-- cata
sem_MultinameInfo :: MultinameInfo ->
                     T_MultinameInfo
sem_MultinameInfo (MultinameInfo_QName _namespace _name) =
    (sem_MultinameInfo_QName _namespace _name)
sem_MultinameInfo (MultinameInfo_QNameA _namespace _name) =
    (sem_MultinameInfo_QNameA _namespace _name)
sem_MultinameInfo (MultinameInfo_RTQName _name) =
    (sem_MultinameInfo_RTQName _name)
sem_MultinameInfo (MultinameInfo_RTQNameA _name) =
    (sem_MultinameInfo_RTQNameA _name)
sem_MultinameInfo (MultinameInfo_RTQNameL) =
    (sem_MultinameInfo_RTQNameL)
sem_MultinameInfo (MultinameInfo_RTQNameLA) =
    (sem_MultinameInfo_RTQNameLA)
sem_MultinameInfo (MultinameInfo_Multiname _name _set) =
    (sem_MultinameInfo_Multiname _name _set)
sem_MultinameInfo (MultinameInfo_MultinameA _name _set) =
    (sem_MultinameInfo_MultinameA _name _set)
sem_MultinameInfo (MultinameInfo_MultinameL _set) =
    (sem_MultinameInfo_MultinameL _set)
sem_MultinameInfo (MultinameInfo_MultinameLA _set) =
    (sem_MultinameInfo_MultinameLA _set)
sem_MultinameInfo (MultinameInfo_Generic _name _params) =
    (sem_MultinameInfo_Generic _name (sem_ParamNames _params))
-- semantic domain
type T_MultinameInfo = ( Name)
sem_MultinameInfo_QName :: Word32 ->
                           Word32 ->
                           T_MultinameInfo
sem_MultinameInfo_QName namespace_ name_ =
    (case (({-# LINE 105 "src\\SymbolTables.ag" #-}
            Name (QualNs $ Ref namespace_) (Just $ Ref name_)
            {-# LINE 1684 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_QNameA :: Word32 ->
                            Word32 ->
                            T_MultinameInfo
sem_MultinameInfo_QNameA namespace_ name_ =
    (case (({-# LINE 105 "src\\SymbolTables.ag" #-}
            Name (QualNs $ Ref namespace_) (Just $ Ref name_)
            {-# LINE 1694 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_RTQName :: Word32 ->
                             T_MultinameInfo
sem_MultinameInfo_RTQName name_ =
    (case (({-# LINE 106 "src\\SymbolTables.ag" #-}
            Name QualLate (Just $ Ref name_)
            {-# LINE 1703 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_RTQNameA :: Word32 ->
                              T_MultinameInfo
sem_MultinameInfo_RTQNameA name_ =
    (case (({-# LINE 106 "src\\SymbolTables.ag" #-}
            Name QualLate (Just $ Ref name_)
            {-# LINE 1712 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_RTQNameL :: T_MultinameInfo
sem_MultinameInfo_RTQNameL =
    (case (({-# LINE 107 "src\\SymbolTables.ag" #-}
            Name QualLate Nothing
            {-# LINE 1720 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_RTQNameLA :: T_MultinameInfo
sem_MultinameInfo_RTQNameLA =
    (case (({-# LINE 107 "src\\SymbolTables.ag" #-}
            Name QualLate Nothing
            {-# LINE 1728 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_Multiname :: Word32 ->
                               Word32 ->
                               T_MultinameInfo
sem_MultinameInfo_Multiname name_ set_ =
    (case (({-# LINE 108 "src\\SymbolTables.ag" #-}
            Name (QualNss $ Ref set_) (Just $ Ref name_)
            {-# LINE 1738 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_MultinameA :: Word32 ->
                                Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameA name_ set_ =
    (case (({-# LINE 108 "src\\SymbolTables.ag" #-}
            Name (QualNss $ Ref set_) (Just $ Ref name_)
            {-# LINE 1748 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_MultinameL :: Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameL set_ =
    (case (({-# LINE 109 "src\\SymbolTables.ag" #-}
            Name (QualNss $ Ref set_) Nothing
            {-# LINE 1757 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_MultinameLA :: Word32 ->
                                 T_MultinameInfo
sem_MultinameInfo_MultinameLA set_ =
    (case (({-# LINE 109 "src\\SymbolTables.ag" #-}
            Name (QualNss $ Ref set_) Nothing
            {-# LINE 1766 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
sem_MultinameInfo_Generic :: Word32 ->
                             T_ParamNames ->
                             T_MultinameInfo
sem_MultinameInfo_Generic name_ params_ =
    (case (({-# LINE 110 "src\\SymbolTables.ag" #-}
            Name QualOther Nothing
            {-# LINE 1776 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
-- MultinameInfos ----------------------------------------------
-- cata
sem_MultinameInfos :: MultinameInfos ->
                      T_MultinameInfos
sem_MultinameInfos list =
    (Prelude.foldr sem_MultinameInfos_Cons sem_MultinameInfos_Nil (Prelude.map sem_MultinameInfo list))
-- semantic domain
type T_MultinameInfos = ( ([Name]))
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlInames) | True ->
         (case (({-# LINE 101 "src\\SymbolTables.ag" #-}
                 _tlInames
                 {-# LINE 1796 "src/SymbolTables.hs" #-}
                 )) of
          { _names_augmented_syn | _names_augmented_syn `seq` (True) ->
          (case (hd_) of
           { ( _hdIname) | True ->
               (case (({-# LINE 101 "src\\SymbolTables.ag" #-}
                       (_hdIname :)
                       {-# LINE 1803 "src/SymbolTables.hs" #-}
                       )) of
                { _names_augmented_f1 | _names_augmented_f1 `seq` (True) ->
                (case (({-# LINE 101 "src\\SymbolTables.ag" #-}
                        foldr ($) _names_augmented_syn [_names_augmented_f1]
                        {-# LINE 1808 "src/SymbolTables.hs" #-}
                        )) of
                 { _lhsOnames | _lhsOnames `seq` (True) ->
                 ( _lhsOnames) }) }) }) }) })
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    (case (({-# LINE 100 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 1816 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOnames | _lhsOnames `seq` (True) ->
     ( _lhsOnames) })
-- MultinameKind -----------------------------------------------
-- cata
sem_MultinameKind :: MultinameKind ->
                     T_MultinameKind
sem_MultinameKind (MultinameKind_QName) =
    (sem_MultinameKind_QName)
sem_MultinameKind (MultinameKind_QNameA) =
    (sem_MultinameKind_QNameA)
sem_MultinameKind (MultinameKind_RTQName) =
    (sem_MultinameKind_RTQName)
sem_MultinameKind (MultinameKind_RTQNameA) =
    (sem_MultinameKind_RTQNameA)
sem_MultinameKind (MultinameKind_RTQNameL) =
    (sem_MultinameKind_RTQNameL)
sem_MultinameKind (MultinameKind_RTQNameLA) =
    (sem_MultinameKind_RTQNameLA)
sem_MultinameKind (MultinameKind_Multiname) =
    (sem_MultinameKind_Multiname)
sem_MultinameKind (MultinameKind_MultinameA) =
    (sem_MultinameKind_MultinameA)
sem_MultinameKind (MultinameKind_MultinameL) =
    (sem_MultinameKind_MultinameL)
sem_MultinameKind (MultinameKind_MultinameLA) =
    (sem_MultinameKind_MultinameLA)
sem_MultinameKind (MultinameKind_Generic) =
    (sem_MultinameKind_Generic)
-- semantic domain
type T_MultinameKind = ( )
sem_MultinameKind_QName :: T_MultinameKind
sem_MultinameKind_QName =
    ( )
sem_MultinameKind_QNameA :: T_MultinameKind
sem_MultinameKind_QNameA =
    ( )
sem_MultinameKind_RTQName :: T_MultinameKind
sem_MultinameKind_RTQName =
    ( )
sem_MultinameKind_RTQNameA :: T_MultinameKind
sem_MultinameKind_RTQNameA =
    ( )
sem_MultinameKind_RTQNameL :: T_MultinameKind
sem_MultinameKind_RTQNameL =
    ( )
sem_MultinameKind_RTQNameLA :: T_MultinameKind
sem_MultinameKind_RTQNameLA =
    ( )
sem_MultinameKind_Multiname :: T_MultinameKind
sem_MultinameKind_Multiname =
    ( )
sem_MultinameKind_MultinameA :: T_MultinameKind
sem_MultinameKind_MultinameA =
    ( )
sem_MultinameKind_MultinameL :: T_MultinameKind
sem_MultinameKind_MultinameL =
    ( )
sem_MultinameKind_MultinameLA :: T_MultinameKind
sem_MultinameKind_MultinameLA =
    ( )
sem_MultinameKind_Generic :: T_MultinameKind
sem_MultinameKind_Generic =
    ( )
-- NamespaceInfo -----------------------------------------------
-- cata
sem_NamespaceInfo :: NamespaceInfo ->
                     T_NamespaceInfo
sem_NamespaceInfo (NamespaceInfo_Info _kind _name) =
    (sem_NamespaceInfo_Info (sem_NamespaceKind _kind) _name)
-- semantic domain
type T_NamespaceInfo = ( Namespace)
sem_NamespaceInfo_Info :: T_NamespaceKind ->
                          Word32 ->
                          T_NamespaceInfo
sem_NamespaceInfo_Info kind_ name_ =
    (case (({-# LINE 146 "src\\SymbolTables.ag" #-}
            Namespace $ Ref name_
            {-# LINE 1895 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOname | _lhsOname `seq` (True) ->
     ( _lhsOname) })
-- NamespaceInfos ----------------------------------------------
-- cata
sem_NamespaceInfos :: NamespaceInfos ->
                      T_NamespaceInfos
sem_NamespaceInfos list =
    (Prelude.foldr sem_NamespaceInfos_Cons sem_NamespaceInfos_Nil (Prelude.map sem_NamespaceInfo list))
-- semantic domain
type T_NamespaceInfos = ( ([Namespace]))
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIspaces) | True ->
         (case (({-# LINE 143 "src\\SymbolTables.ag" #-}
                 _tlIspaces
                 {-# LINE 1915 "src/SymbolTables.hs" #-}
                 )) of
          { _spaces_augmented_syn | _spaces_augmented_syn `seq` (True) ->
          (case (hd_) of
           { ( _hdIname) | True ->
               (case (({-# LINE 143 "src\\SymbolTables.ag" #-}
                       (_hdIname :)
                       {-# LINE 1922 "src/SymbolTables.hs" #-}
                       )) of
                { _spaces_augmented_f1 | _spaces_augmented_f1 `seq` (True) ->
                (case (({-# LINE 143 "src\\SymbolTables.ag" #-}
                        foldr ($) _spaces_augmented_syn [_spaces_augmented_f1]
                        {-# LINE 1927 "src/SymbolTables.hs" #-}
                        )) of
                 { _lhsOspaces | _lhsOspaces `seq` (True) ->
                 ( _lhsOspaces) }) }) }) }) })
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    (case (({-# LINE 142 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 1935 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOspaces | _lhsOspaces `seq` (True) ->
     ( _lhsOspaces) })
-- NamespaceKind -----------------------------------------------
-- cata
sem_NamespaceKind :: NamespaceKind ->
                     T_NamespaceKind
sem_NamespaceKind (NamespaceKind_General) =
    (sem_NamespaceKind_General)
sem_NamespaceKind (NamespaceKind_Package) =
    (sem_NamespaceKind_Package)
sem_NamespaceKind (NamespaceKind_Internal) =
    (sem_NamespaceKind_Internal)
sem_NamespaceKind (NamespaceKind_Protected) =
    (sem_NamespaceKind_Protected)
sem_NamespaceKind (NamespaceKind_Explicit) =
    (sem_NamespaceKind_Explicit)
sem_NamespaceKind (NamespaceKind_Static) =
    (sem_NamespaceKind_Static)
sem_NamespaceKind (NamespaceKind_Private) =
    (sem_NamespaceKind_Private)
-- semantic domain
type T_NamespaceKind = ( )
sem_NamespaceKind_General :: T_NamespaceKind
sem_NamespaceKind_General =
    ( )
sem_NamespaceKind_Package :: T_NamespaceKind
sem_NamespaceKind_Package =
    ( )
sem_NamespaceKind_Internal :: T_NamespaceKind
sem_NamespaceKind_Internal =
    ( )
sem_NamespaceKind_Protected :: T_NamespaceKind
sem_NamespaceKind_Protected =
    ( )
sem_NamespaceKind_Explicit :: T_NamespaceKind
sem_NamespaceKind_Explicit =
    ( )
sem_NamespaceKind_Static :: T_NamespaceKind
sem_NamespaceKind_Static =
    ( )
sem_NamespaceKind_Private :: T_NamespaceKind
sem_NamespaceKind_Private =
    ( )
-- NamespaceNames ----------------------------------------------
-- cata
sem_NamespaceNames :: NamespaceNames ->
                      T_NamespaceNames
sem_NamespaceNames list =
    (Prelude.foldr sem_NamespaceNames_Cons sem_NamespaceNames_Nil list)
-- semantic domain
type T_NamespaceNames = ( NamespaceNames)
sem_NamespaceNames_Cons :: Word32 ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlInames) | True ->
         (case (({-# LINE 129 "src\\SymbolTables.ag" #-}
                 (:) hd_ _tlInames
                 {-# LINE 1996 "src/SymbolTables.hs" #-}
                 )) of
          { _names | _names `seq` (True) ->
          (case (({-# LINE 129 "src\\SymbolTables.ag" #-}
                  _names
                  {-# LINE 2001 "src/SymbolTables.hs" #-}
                  )) of
           { _lhsOnames | _lhsOnames `seq` (True) ->
           ( _lhsOnames) }) }) })
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (case (({-# LINE 129 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2009 "src/SymbolTables.hs" #-}
            )) of
     { _names | _names `seq` (True) ->
     (case (({-# LINE 129 "src\\SymbolTables.ag" #-}
             _names
             {-# LINE 2014 "src/SymbolTables.hs" #-}
             )) of
      { _lhsOnames | _lhsOnames `seq` (True) ->
      ( _lhsOnames) }) })
-- Optional ----------------------------------------------------
-- cata
sem_Optional :: Optional ->
                T_Optional
sem_Optional (Optional_Detail _val _kind) =
    (sem_Optional_Detail _val (sem_ValueKind _kind))
-- semantic domain
type T_Optional = ( )
sem_Optional_Detail :: Word32 ->
                       T_ValueKind ->
                       T_Optional
sem_Optional_Detail val_ kind_ =
    ( )
-- Optionals ---------------------------------------------------
-- cata
sem_Optionals :: Optionals ->
                 T_Optionals
sem_Optionals list =
    (Prelude.foldr sem_Optionals_Cons sem_Optionals_Nil (Prelude.map sem_Optional list))
-- semantic domain
type T_Optionals = ( )
sem_Optionals_Cons :: T_Optional ->
                      T_Optionals ->
                      T_Optionals
sem_Optionals_Cons hd_ tl_ =
    ( )
sem_Optionals_Nil :: T_Optionals
sem_Optionals_Nil =
    ( )
-- ParamNames --------------------------------------------------
-- cata
sem_ParamNames :: ParamNames ->
                  T_ParamNames
sem_ParamNames list =
    (Prelude.foldr sem_ParamNames_Cons sem_ParamNames_Nil list)
-- semantic domain
type T_ParamNames = ( ParamNames)
sem_ParamNames_Cons :: Word32 ->
                       T_ParamNames ->
                       T_ParamNames
sem_ParamNames_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlInames) | True ->
         (case (({-# LINE 171 "src\\SymbolTables.ag" #-}
                 (:) hd_ _tlInames
                 {-# LINE 2063 "src/SymbolTables.hs" #-}
                 )) of
          { _names | _names `seq` (True) ->
          (case (({-# LINE 171 "src\\SymbolTables.ag" #-}
                  _names
                  {-# LINE 2068 "src/SymbolTables.hs" #-}
                  )) of
           { _lhsOnames | _lhsOnames `seq` (True) ->
           ( _lhsOnames) }) }) })
sem_ParamNames_Nil :: T_ParamNames
sem_ParamNames_Nil =
    (case (({-# LINE 171 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2076 "src/SymbolTables.hs" #-}
            )) of
     { _names | _names `seq` (True) ->
     (case (({-# LINE 171 "src\\SymbolTables.ag" #-}
             _names
             {-# LINE 2081 "src/SymbolTables.hs" #-}
             )) of
      { _lhsOnames | _lhsOnames `seq` (True) ->
      ( _lhsOnames) }) })
-- ParamTypes --------------------------------------------------
-- cata
sem_ParamTypes :: ParamTypes ->
                  T_ParamTypes
sem_ParamTypes list =
    (Prelude.foldr sem_ParamTypes_Cons sem_ParamTypes_Nil list)
-- semantic domain
type T_ParamTypes = ( ParamTypes)
sem_ParamTypes_Cons :: Word32 ->
                       T_ParamTypes ->
                       T_ParamTypes
sem_ParamTypes_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlItypes) | True ->
         (case (({-# LINE 172 "src\\SymbolTables.ag" #-}
                 (:) hd_ _tlItypes
                 {-# LINE 2101 "src/SymbolTables.hs" #-}
                 )) of
          { _types | _types `seq` (True) ->
          (case (({-# LINE 172 "src\\SymbolTables.ag" #-}
                  _types
                  {-# LINE 2106 "src/SymbolTables.hs" #-}
                  )) of
           { _lhsOtypes | _lhsOtypes `seq` (True) ->
           ( _lhsOtypes) }) }) })
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    (case (({-# LINE 172 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2114 "src/SymbolTables.hs" #-}
            )) of
     { _types | _types `seq` (True) ->
     (case (({-# LINE 172 "src\\SymbolTables.ag" #-}
             _types
             {-# LINE 2119 "src/SymbolTables.hs" #-}
             )) of
      { _lhsOtypes | _lhsOtypes `seq` (True) ->
      ( _lhsOtypes) }) })
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = ( ([Double]))
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIvalues) | True ->
         (case (({-# LINE 84 "src\\SymbolTables.ag" #-}
                 _tlIvalues
                 {-# LINE 2139 "src/SymbolTables.hs" #-}
                 )) of
          { _values_augmented_syn | _values_augmented_syn `seq` (True) ->
          (case (({-# LINE 84 "src\\SymbolTables.ag" #-}
                  (hd_ :)
                  {-# LINE 2144 "src/SymbolTables.hs" #-}
                  )) of
           { _values_augmented_f1 | _values_augmented_f1 `seq` (True) ->
           (case (({-# LINE 84 "src\\SymbolTables.ag" #-}
                   foldr ($) _values_augmented_syn [_values_augmented_f1]
                   {-# LINE 2149 "src/SymbolTables.hs" #-}
                   )) of
            { _lhsOvalues | _lhsOvalues `seq` (True) ->
            ( _lhsOvalues) }) }) }) })
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    (case (({-# LINE 83 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2157 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOvalues | _lhsOvalues `seq` (True) ->
     ( _lhsOvalues) })
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = ( DoublePool,IntPool,NamePool,NamesetsPool,NamespacePool,StringPool,UIntPool)
sem_PoolInfo_Info :: T_PoolInts ->
                     T_PoolUInts ->
                     T_PoolDoubles ->
                     T_PoolStrings ->
                     T_NamespaceInfos ->
                     T_SetInfos ->
                     T_MultinameInfos ->
                     T_PoolInfo
sem_PoolInfo_Info integers_ uintegers_ doubles_ strings_ namespaces_ namesets_ multinames_ =
    (case (doubles_) of
     { ( _doublesIvalues) | True ->
         (case (({-# LINE 72 "src\\SymbolTables.ag" #-}
                 listEnv $ zip [1..] _doublesIvalues
                 {-# LINE 2182 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOgathDoublePool | _lhsOgathDoublePool `seq` (True) ->
          (case (integers_) of
           { ( _integersIvalues) | True ->
               (case (({-# LINE 70 "src\\SymbolTables.ag" #-}
                       listEnv $ zip [1..] _integersIvalues
                       {-# LINE 2189 "src/SymbolTables.hs" #-}
                       )) of
                { _lhsOgathIntPool | _lhsOgathIntPool `seq` (True) ->
                (case (multinames_) of
                 { ( _multinamesInames) | True ->
                     (case (({-# LINE 98 "src\\SymbolTables.ag" #-}
                             listEnv $ zip [1..] _multinamesInames
                             {-# LINE 2196 "src/SymbolTables.hs" #-}
                             )) of
                      { _lhsOgathNamePool | _lhsOgathNamePool `seq` (True) ->
                      (case (namesets_) of
                       { ( _namesetsIsets) | True ->
                           (case (({-# LINE 121 "src\\SymbolTables.ag" #-}
                                   listEnv $ zip [1..] _namesetsIsets
                                   {-# LINE 2203 "src/SymbolTables.hs" #-}
                                   )) of
                            { _lhsOgathNamesetsPool | _lhsOgathNamesetsPool `seq` (True) ->
                            (case (namespaces_) of
                             { ( _namespacesIspaces) | True ->
                                 (case (({-# LINE 140 "src\\SymbolTables.ag" #-}
                                         listEnv $ zip [1..] _namespacesIspaces
                                         {-# LINE 2210 "src/SymbolTables.hs" #-}
                                         )) of
                                  { _lhsOgathNamespacePool | _lhsOgathNamespacePool `seq` (True) ->
                                  (case (strings_) of
                                   { ( _stringsIvalues) | True ->
                                       (case (({-# LINE 69 "src\\SymbolTables.ag" #-}
                                               listEnv $ zip [1..] _stringsIvalues
                                               {-# LINE 2217 "src/SymbolTables.hs" #-}
                                               )) of
                                        { _lhsOgathStringPool | _lhsOgathStringPool `seq` (True) ->
                                        (case (uintegers_) of
                                         { ( _uintegersIvalues) | True ->
                                             (case (({-# LINE 71 "src\\SymbolTables.ag" #-}
                                                     listEnv $ zip [1..] _uintegersIvalues
                                                     {-# LINE 2224 "src/SymbolTables.hs" #-}
                                                     )) of
                                              { _lhsOgathUIntPool | _lhsOgathUIntPool `seq` (True) ->
                                              ( _lhsOgathDoublePool,_lhsOgathIntPool,_lhsOgathNamePool,_lhsOgathNamesetsPool,_lhsOgathNamespacePool,_lhsOgathStringPool,_lhsOgathUIntPool) }) }) }) }) }) }) }) }) }) }) }) }) }) })
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = ( ([Word32]))
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIvalues) | True ->
         (case (({-# LINE 78 "src\\SymbolTables.ag" #-}
                 _tlIvalues
                 {-# LINE 2244 "src/SymbolTables.hs" #-}
                 )) of
          { _values_augmented_syn | _values_augmented_syn `seq` (True) ->
          (case (({-# LINE 78 "src\\SymbolTables.ag" #-}
                  (hd_ :)
                  {-# LINE 2249 "src/SymbolTables.hs" #-}
                  )) of
           { _values_augmented_f1 | _values_augmented_f1 `seq` (True) ->
           (case (({-# LINE 78 "src\\SymbolTables.ag" #-}
                   foldr ($) _values_augmented_syn [_values_augmented_f1]
                   {-# LINE 2254 "src/SymbolTables.hs" #-}
                   )) of
            { _lhsOvalues | _lhsOvalues `seq` (True) ->
            ( _lhsOvalues) }) }) }) })
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    (case (({-# LINE 77 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2262 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOvalues | _lhsOvalues `seq` (True) ->
     ( _lhsOvalues) })
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = ( ([String]))
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIvalues) | True ->
         (case (({-# LINE 75 "src\\SymbolTables.ag" #-}
                 _tlIvalues
                 {-# LINE 2282 "src/SymbolTables.hs" #-}
                 )) of
          { _values_augmented_syn | _values_augmented_syn `seq` (True) ->
          (case (({-# LINE 75 "src\\SymbolTables.ag" #-}
                  ((decode $ unpack hd_) :)
                  {-# LINE 2287 "src/SymbolTables.hs" #-}
                  )) of
           { _values_augmented_f1 | _values_augmented_f1 `seq` (True) ->
           (case (({-# LINE 75 "src\\SymbolTables.ag" #-}
                   foldr ($) _values_augmented_syn [_values_augmented_f1]
                   {-# LINE 2292 "src/SymbolTables.hs" #-}
                   )) of
            { _lhsOvalues | _lhsOvalues `seq` (True) ->
            ( _lhsOvalues) }) }) }) })
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    (case (({-# LINE 74 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2300 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOvalues | _lhsOvalues `seq` (True) ->
     ( _lhsOvalues) })
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = ( ([Word32]))
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIvalues) | True ->
         (case (({-# LINE 81 "src\\SymbolTables.ag" #-}
                 _tlIvalues
                 {-# LINE 2320 "src/SymbolTables.hs" #-}
                 )) of
          { _values_augmented_syn | _values_augmented_syn `seq` (True) ->
          (case (({-# LINE 81 "src\\SymbolTables.ag" #-}
                  (hd_ :)
                  {-# LINE 2325 "src/SymbolTables.hs" #-}
                  )) of
           { _values_augmented_f1 | _values_augmented_f1 `seq` (True) ->
           (case (({-# LINE 81 "src\\SymbolTables.ag" #-}
                   foldr ($) _values_augmented_syn [_values_augmented_f1]
                   {-# LINE 2330 "src/SymbolTables.hs" #-}
                   )) of
            { _lhsOvalues | _lhsOvalues `seq` (True) ->
            ( _lhsOvalues) }) }) }) })
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    (case (({-# LINE 80 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2338 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOvalues | _lhsOvalues `seq` (True) ->
     ( _lhsOvalues) })
-- Rect --------------------------------------------------------
-- cata
sem_Rect :: Rect ->
            T_Rect
sem_Rect (Rect_Rect _bits _xMin _xMax _yMin _yMax) =
    (sem_Rect_Rect _bits _xMin _xMax _yMin _yMax)
-- semantic domain
type T_Rect = ( )
sem_Rect_Rect :: Int ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 T_Rect
sem_Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_ =
    ( )
-- ScriptInfo --------------------------------------------------
-- cata
sem_ScriptInfo :: ScriptInfo ->
                  T_ScriptInfo
sem_ScriptInfo (ScriptInfo_Info _method _traits) =
    (sem_ScriptInfo_Info _method (sem_Traits _traits))
-- semantic domain
type T_ScriptInfo = ( )
sem_ScriptInfo_Info :: Word32 ->
                       T_Traits ->
                       T_ScriptInfo
sem_ScriptInfo_Info method_ traits_ =
    ( )
-- ScriptInfos -------------------------------------------------
-- cata
sem_ScriptInfos :: ScriptInfos ->
                   T_ScriptInfos
sem_ScriptInfos list =
    (Prelude.foldr sem_ScriptInfos_Cons sem_ScriptInfos_Nil (Prelude.map sem_ScriptInfo list))
-- semantic domain
type T_ScriptInfos = ( )
sem_ScriptInfos_Cons :: T_ScriptInfo ->
                        T_ScriptInfos ->
                        T_ScriptInfos
sem_ScriptInfos_Cons hd_ tl_ =
    ( )
sem_ScriptInfos_Nil :: T_ScriptInfos
sem_ScriptInfos_Nil =
    ( )
-- SetInfo -----------------------------------------------------
-- cata
sem_SetInfo :: SetInfo ->
               T_SetInfo
sem_SetInfo (SetInfo_Info _names) =
    (sem_SetInfo_Info (sem_NamespaceNames _names))
-- semantic domain
type T_SetInfo = ( Nameset)
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    (case (names_) of
     { ( _namesInames) | True ->
         (case (({-# LINE 127 "src\\SymbolTables.ag" #-}
                 Nameset (map Ref _namesInames)
                 {-# LINE 2402 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOset | _lhsOset `seq` (True) ->
          ( _lhsOset) }) })
-- SetInfos ----------------------------------------------------
-- cata
sem_SetInfos :: SetInfos ->
                T_SetInfos
sem_SetInfos list =
    (Prelude.foldr sem_SetInfos_Cons sem_SetInfos_Nil (Prelude.map sem_SetInfo list))
-- semantic domain
type T_SetInfos = ( ([Nameset]))
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIsets) | True ->
         (case (({-# LINE 124 "src\\SymbolTables.ag" #-}
                 _tlIsets
                 {-# LINE 2422 "src/SymbolTables.hs" #-}
                 )) of
          { _sets_augmented_syn | _sets_augmented_syn `seq` (True) ->
          (case (hd_) of
           { ( _hdIset) | True ->
               (case (({-# LINE 124 "src\\SymbolTables.ag" #-}
                       (_hdIset :)
                       {-# LINE 2429 "src/SymbolTables.hs" #-}
                       )) of
                { _sets_augmented_f1 | _sets_augmented_f1 `seq` (True) ->
                (case (({-# LINE 124 "src\\SymbolTables.ag" #-}
                        foldr ($) _sets_augmented_syn [_sets_augmented_f1]
                        {-# LINE 2434 "src/SymbolTables.hs" #-}
                        )) of
                 { _lhsOsets | _lhsOsets `seq` (True) ->
                 ( _lhsOsets) }) }) }) }) })
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    (case (({-# LINE 123 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2442 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOsets | _lhsOsets `seq` (True) ->
     ( _lhsOsets) })
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = ( ([SymbolTables]))
data Inh_SwfFile = Inh_SwfFile {}
data Syn_SwfFile = Syn_SwfFile {allTables_Syn_SwfFile :: !(([SymbolTables]))}
wrap_SwfFile :: T_SwfFile ->
                Inh_SwfFile ->
                Syn_SwfFile
wrap_SwfFile sem (Inh_SwfFile) =
    (let ( _lhsOallTables) | True = sem
     in  (Syn_SwfFile _lhsOallTables))
sem_SwfFile_File :: Bool ->
                    Word8 ->
                    Word32 ->
                    T_Rect ->
                    Word16 ->
                    Word16 ->
                    T_Tags ->
                    T_SwfFile
sem_SwfFile_File compressed_ version_ length_ size_ rate_ count_ tags_ =
    (case (tags_) of
     { ( _tagsIallTables) | True ->
         (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
                 _tagsIallTables
                 {-# LINE 2475 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOallTables | _lhsOallTables `seq` (True) ->
          ( _lhsOallTables) }) })
-- Tag ---------------------------------------------------------
-- cata
sem_Tag :: Tag ->
           T_Tag
sem_Tag (Tag_Abc _flags _name _file) =
    (sem_Tag_Abc (sem_AbcFlags _flags) _name (sem_AbcFile _file))
sem_Tag (Tag_FileAttributes _useDirectBlit _useGPU _hasMetaData _hasAS3 _useNetwork) =
    (sem_Tag_FileAttributes _useDirectBlit _useGPU _hasMetaData _hasAS3 _useNetwork)
sem_Tag (Tag_Opaque _kind _length _body) =
    (sem_Tag_Opaque (sem_TagKind _kind) _length _body)
sem_Tag (Tag_End) =
    (sem_Tag_End)
-- semantic domain
type T_Tag = ( ([SymbolTables]))
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    (case (file_) of
     { ( _fileIallTables,_fileItables) | True ->
         (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
                 _fileIallTables
                 {-# LINE 2502 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOallTables | _lhsOallTables `seq` (True) ->
          ( _lhsOallTables) }) })
sem_Tag_FileAttributes :: Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          T_Tag
sem_Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_ =
    (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2515 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOallTables | _lhsOallTables `seq` (True) ->
     ( _lhsOallTables) })
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2526 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOallTables | _lhsOallTables `seq` (True) ->
     ( _lhsOallTables) })
sem_Tag_End :: T_Tag
sem_Tag_End =
    (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2534 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOallTables | _lhsOallTables `seq` (True) ->
     ( _lhsOallTables) })
-- TagKind -----------------------------------------------------
-- cata
sem_TagKind :: TagKind ->
               T_TagKind
sem_TagKind (TagKind_End) =
    (sem_TagKind_End)
sem_TagKind (TagKind_ShowFrame) =
    (sem_TagKind_ShowFrame)
sem_TagKind (TagKind_DefineShape) =
    (sem_TagKind_DefineShape)
sem_TagKind (TagKind_PlaceObject) =
    (sem_TagKind_PlaceObject)
sem_TagKind (TagKind_RemoveObject) =
    (sem_TagKind_RemoveObject)
sem_TagKind (TagKind_DefineBits) =
    (sem_TagKind_DefineBits)
sem_TagKind (TagKind_DefineButton) =
    (sem_TagKind_DefineButton)
sem_TagKind (TagKind_JPEGTables) =
    (sem_TagKind_JPEGTables)
sem_TagKind (TagKind_SetBackgroundColor) =
    (sem_TagKind_SetBackgroundColor)
sem_TagKind (TagKind_DefineFont) =
    (sem_TagKind_DefineFont)
sem_TagKind (TagKind_DefineText) =
    (sem_TagKind_DefineText)
sem_TagKind (TagKind_DoAction) =
    (sem_TagKind_DoAction)
sem_TagKind (TagKind_DefineFontInfo) =
    (sem_TagKind_DefineFontInfo)
sem_TagKind (TagKind_DefineSound) =
    (sem_TagKind_DefineSound)
sem_TagKind (TagKind_StartSound) =
    (sem_TagKind_StartSound)
sem_TagKind (TagKind_DefineButtonSound) =
    (sem_TagKind_DefineButtonSound)
sem_TagKind (TagKind_SoundStreamHead) =
    (sem_TagKind_SoundStreamHead)
sem_TagKind (TagKind_SoundStreamBlock) =
    (sem_TagKind_SoundStreamBlock)
sem_TagKind (TagKind_DefineBitsLossless) =
    (sem_TagKind_DefineBitsLossless)
sem_TagKind (TagKind_DefineBitsJPEG2) =
    (sem_TagKind_DefineBitsJPEG2)
sem_TagKind (TagKind_DefineShape2) =
    (sem_TagKind_DefineShape2)
sem_TagKind (TagKind_DefineButtonCxform) =
    (sem_TagKind_DefineButtonCxform)
sem_TagKind (TagKind_Protect) =
    (sem_TagKind_Protect)
sem_TagKind (TagKind_PlaceObject2) =
    (sem_TagKind_PlaceObject2)
sem_TagKind (TagKind_RemoveObject2) =
    (sem_TagKind_RemoveObject2)
sem_TagKind (TagKind_DefineShape3) =
    (sem_TagKind_DefineShape3)
sem_TagKind (TagKind_DefineText2) =
    (sem_TagKind_DefineText2)
sem_TagKind (TagKind_DefineButton2) =
    (sem_TagKind_DefineButton2)
sem_TagKind (TagKind_DefineBitsJPEG3) =
    (sem_TagKind_DefineBitsJPEG3)
sem_TagKind (TagKind_DefineBitsLossless2) =
    (sem_TagKind_DefineBitsLossless2)
sem_TagKind (TagKind_DefineEditText) =
    (sem_TagKind_DefineEditText)
sem_TagKind (TagKind_DefineSprite) =
    (sem_TagKind_DefineSprite)
sem_TagKind (TagKind_FrameLabel) =
    (sem_TagKind_FrameLabel)
sem_TagKind (TagKind_SoundStreamHead2) =
    (sem_TagKind_SoundStreamHead2)
sem_TagKind (TagKind_DefineMorphShape) =
    (sem_TagKind_DefineMorphShape)
sem_TagKind (TagKind_DefineFont2) =
    (sem_TagKind_DefineFont2)
sem_TagKind (TagKind_ExportAssets) =
    (sem_TagKind_ExportAssets)
sem_TagKind (TagKind_ImportAssets) =
    (sem_TagKind_ImportAssets)
sem_TagKind (TagKind_EnableDebugger) =
    (sem_TagKind_EnableDebugger)
sem_TagKind (TagKind_DoInitAction) =
    (sem_TagKind_DoInitAction)
sem_TagKind (TagKind_DefineVideoStream) =
    (sem_TagKind_DefineVideoStream)
sem_TagKind (TagKind_VideoFrame) =
    (sem_TagKind_VideoFrame)
sem_TagKind (TagKind_DefineFontInfo2) =
    (sem_TagKind_DefineFontInfo2)
sem_TagKind (TagKind_EnableDebugger2) =
    (sem_TagKind_EnableDebugger2)
sem_TagKind (TagKind_ScriptLimits) =
    (sem_TagKind_ScriptLimits)
sem_TagKind (TagKind_SetTabIndex) =
    (sem_TagKind_SetTabIndex)
sem_TagKind (TagKind_FileAttributes) =
    (sem_TagKind_FileAttributes)
sem_TagKind (TagKind_PlaceObject3) =
    (sem_TagKind_PlaceObject3)
sem_TagKind (TagKind_ImportAssets2) =
    (sem_TagKind_ImportAssets2)
sem_TagKind (TagKind_DefineFontAlignZones) =
    (sem_TagKind_DefineFontAlignZones)
sem_TagKind (TagKind_CSMTextSettings) =
    (sem_TagKind_CSMTextSettings)
sem_TagKind (TagKind_DefineFont3) =
    (sem_TagKind_DefineFont3)
sem_TagKind (TagKind_SymbolClass) =
    (sem_TagKind_SymbolClass)
sem_TagKind (TagKind_Metadata) =
    (sem_TagKind_Metadata)
sem_TagKind (TagKind_DefineScalingGrid) =
    (sem_TagKind_DefineScalingGrid)
sem_TagKind (TagKind_DoABC) =
    (sem_TagKind_DoABC)
sem_TagKind (TagKind_DefineShape4) =
    (sem_TagKind_DefineShape4)
sem_TagKind (TagKind_DefineMorphShape2) =
    (sem_TagKind_DefineMorphShape2)
sem_TagKind (TagKind_DefineSceneAndFrameLabelData) =
    (sem_TagKind_DefineSceneAndFrameLabelData)
sem_TagKind (TagKind_DefineBinaryData) =
    (sem_TagKind_DefineBinaryData)
sem_TagKind (TagKind_DefineFontName) =
    (sem_TagKind_DefineFontName)
sem_TagKind (TagKind_StartSound2) =
    (sem_TagKind_StartSound2)
sem_TagKind (TagKind_DefineBitsJPEG4) =
    (sem_TagKind_DefineBitsJPEG4)
sem_TagKind (TagKind_DefineFont4) =
    (sem_TagKind_DefineFont4)
sem_TagKind (TagKind_Other _code) =
    (sem_TagKind_Other _code)
-- semantic domain
type T_TagKind = ( )
sem_TagKind_End :: T_TagKind
sem_TagKind_End =
    ( )
sem_TagKind_ShowFrame :: T_TagKind
sem_TagKind_ShowFrame =
    ( )
sem_TagKind_DefineShape :: T_TagKind
sem_TagKind_DefineShape =
    ( )
sem_TagKind_PlaceObject :: T_TagKind
sem_TagKind_PlaceObject =
    ( )
sem_TagKind_RemoveObject :: T_TagKind
sem_TagKind_RemoveObject =
    ( )
sem_TagKind_DefineBits :: T_TagKind
sem_TagKind_DefineBits =
    ( )
sem_TagKind_DefineButton :: T_TagKind
sem_TagKind_DefineButton =
    ( )
sem_TagKind_JPEGTables :: T_TagKind
sem_TagKind_JPEGTables =
    ( )
sem_TagKind_SetBackgroundColor :: T_TagKind
sem_TagKind_SetBackgroundColor =
    ( )
sem_TagKind_DefineFont :: T_TagKind
sem_TagKind_DefineFont =
    ( )
sem_TagKind_DefineText :: T_TagKind
sem_TagKind_DefineText =
    ( )
sem_TagKind_DoAction :: T_TagKind
sem_TagKind_DoAction =
    ( )
sem_TagKind_DefineFontInfo :: T_TagKind
sem_TagKind_DefineFontInfo =
    ( )
sem_TagKind_DefineSound :: T_TagKind
sem_TagKind_DefineSound =
    ( )
sem_TagKind_StartSound :: T_TagKind
sem_TagKind_StartSound =
    ( )
sem_TagKind_DefineButtonSound :: T_TagKind
sem_TagKind_DefineButtonSound =
    ( )
sem_TagKind_SoundStreamHead :: T_TagKind
sem_TagKind_SoundStreamHead =
    ( )
sem_TagKind_SoundStreamBlock :: T_TagKind
sem_TagKind_SoundStreamBlock =
    ( )
sem_TagKind_DefineBitsLossless :: T_TagKind
sem_TagKind_DefineBitsLossless =
    ( )
sem_TagKind_DefineBitsJPEG2 :: T_TagKind
sem_TagKind_DefineBitsJPEG2 =
    ( )
sem_TagKind_DefineShape2 :: T_TagKind
sem_TagKind_DefineShape2 =
    ( )
sem_TagKind_DefineButtonCxform :: T_TagKind
sem_TagKind_DefineButtonCxform =
    ( )
sem_TagKind_Protect :: T_TagKind
sem_TagKind_Protect =
    ( )
sem_TagKind_PlaceObject2 :: T_TagKind
sem_TagKind_PlaceObject2 =
    ( )
sem_TagKind_RemoveObject2 :: T_TagKind
sem_TagKind_RemoveObject2 =
    ( )
sem_TagKind_DefineShape3 :: T_TagKind
sem_TagKind_DefineShape3 =
    ( )
sem_TagKind_DefineText2 :: T_TagKind
sem_TagKind_DefineText2 =
    ( )
sem_TagKind_DefineButton2 :: T_TagKind
sem_TagKind_DefineButton2 =
    ( )
sem_TagKind_DefineBitsJPEG3 :: T_TagKind
sem_TagKind_DefineBitsJPEG3 =
    ( )
sem_TagKind_DefineBitsLossless2 :: T_TagKind
sem_TagKind_DefineBitsLossless2 =
    ( )
sem_TagKind_DefineEditText :: T_TagKind
sem_TagKind_DefineEditText =
    ( )
sem_TagKind_DefineSprite :: T_TagKind
sem_TagKind_DefineSprite =
    ( )
sem_TagKind_FrameLabel :: T_TagKind
sem_TagKind_FrameLabel =
    ( )
sem_TagKind_SoundStreamHead2 :: T_TagKind
sem_TagKind_SoundStreamHead2 =
    ( )
sem_TagKind_DefineMorphShape :: T_TagKind
sem_TagKind_DefineMorphShape =
    ( )
sem_TagKind_DefineFont2 :: T_TagKind
sem_TagKind_DefineFont2 =
    ( )
sem_TagKind_ExportAssets :: T_TagKind
sem_TagKind_ExportAssets =
    ( )
sem_TagKind_ImportAssets :: T_TagKind
sem_TagKind_ImportAssets =
    ( )
sem_TagKind_EnableDebugger :: T_TagKind
sem_TagKind_EnableDebugger =
    ( )
sem_TagKind_DoInitAction :: T_TagKind
sem_TagKind_DoInitAction =
    ( )
sem_TagKind_DefineVideoStream :: T_TagKind
sem_TagKind_DefineVideoStream =
    ( )
sem_TagKind_VideoFrame :: T_TagKind
sem_TagKind_VideoFrame =
    ( )
sem_TagKind_DefineFontInfo2 :: T_TagKind
sem_TagKind_DefineFontInfo2 =
    ( )
sem_TagKind_EnableDebugger2 :: T_TagKind
sem_TagKind_EnableDebugger2 =
    ( )
sem_TagKind_ScriptLimits :: T_TagKind
sem_TagKind_ScriptLimits =
    ( )
sem_TagKind_SetTabIndex :: T_TagKind
sem_TagKind_SetTabIndex =
    ( )
sem_TagKind_FileAttributes :: T_TagKind
sem_TagKind_FileAttributes =
    ( )
sem_TagKind_PlaceObject3 :: T_TagKind
sem_TagKind_PlaceObject3 =
    ( )
sem_TagKind_ImportAssets2 :: T_TagKind
sem_TagKind_ImportAssets2 =
    ( )
sem_TagKind_DefineFontAlignZones :: T_TagKind
sem_TagKind_DefineFontAlignZones =
    ( )
sem_TagKind_CSMTextSettings :: T_TagKind
sem_TagKind_CSMTextSettings =
    ( )
sem_TagKind_DefineFont3 :: T_TagKind
sem_TagKind_DefineFont3 =
    ( )
sem_TagKind_SymbolClass :: T_TagKind
sem_TagKind_SymbolClass =
    ( )
sem_TagKind_Metadata :: T_TagKind
sem_TagKind_Metadata =
    ( )
sem_TagKind_DefineScalingGrid :: T_TagKind
sem_TagKind_DefineScalingGrid =
    ( )
sem_TagKind_DoABC :: T_TagKind
sem_TagKind_DoABC =
    ( )
sem_TagKind_DefineShape4 :: T_TagKind
sem_TagKind_DefineShape4 =
    ( )
sem_TagKind_DefineMorphShape2 :: T_TagKind
sem_TagKind_DefineMorphShape2 =
    ( )
sem_TagKind_DefineSceneAndFrameLabelData :: T_TagKind
sem_TagKind_DefineSceneAndFrameLabelData =
    ( )
sem_TagKind_DefineBinaryData :: T_TagKind
sem_TagKind_DefineBinaryData =
    ( )
sem_TagKind_DefineFontName :: T_TagKind
sem_TagKind_DefineFontName =
    ( )
sem_TagKind_StartSound2 :: T_TagKind
sem_TagKind_StartSound2 =
    ( )
sem_TagKind_DefineBitsJPEG4 :: T_TagKind
sem_TagKind_DefineBitsJPEG4 =
    ( )
sem_TagKind_DefineFont4 :: T_TagKind
sem_TagKind_DefineFont4 =
    ( )
sem_TagKind_Other :: Word16 ->
                     T_TagKind
sem_TagKind_Other code_ =
    ( )
-- Tags --------------------------------------------------------
-- cata
sem_Tags :: Tags ->
            T_Tags
sem_Tags list =
    (Prelude.foldr sem_Tags_Cons sem_Tags_Nil (Prelude.map sem_Tag list))
-- semantic domain
type T_Tags = ( ([SymbolTables]))
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIallTables) | True ->
         (case (hd_) of
          { ( _hdIallTables) | True ->
              (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
                      _hdIallTables ++ _tlIallTables
                      {-# LINE 2888 "src/SymbolTables.hs" #-}
                      )) of
               { _lhsOallTables | _lhsOallTables `seq` (True) ->
               ( _lhsOallTables) }) }) })
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    (case (({-# LINE 60 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 2896 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOallTables | _lhsOallTables `seq` (True) ->
     ( _lhsOallTables) })
-- Trait -------------------------------------------------------
-- cata
sem_Trait :: Trait ->
             T_Trait
sem_Trait (Trait_Trait _name _data _attrs _meta) =
    (sem_Trait_Trait _name (sem_TraitData _data) (sem_TraitAttrs _attrs) (sem_TraitMeta _meta))
-- semantic domain
type T_Trait = ( TraitDescr)
sem_Trait_Trait :: Word32 ->
                   T_TraitData ->
                   T_TraitAttrs ->
                   T_TraitMeta ->
                   T_Trait
sem_Trait_Trait name_ data_ attrs_ meta_ =
    (case (data_) of
     { ( _dataIbody) | True ->
         (case (({-# LINE 218 "src\\SymbolTables.ag" #-}
                 TraitDescr (Ref name_) _dataIbody
                 {-# LINE 2918 "src/SymbolTables.hs" #-}
                 )) of
          { _lhsOtraitInfo | _lhsOtraitInfo `seq` (True) ->
          ( _lhsOtraitInfo) }) })
-- TraitAttr ---------------------------------------------------
-- cata
sem_TraitAttr :: TraitAttr ->
                 T_TraitAttr
sem_TraitAttr (TraitAttr_Final) =
    (sem_TraitAttr_Final)
sem_TraitAttr (TraitAttr_Override) =
    (sem_TraitAttr_Override)
sem_TraitAttr (TraitAttr_Metadata) =
    (sem_TraitAttr_Metadata)
-- semantic domain
type T_TraitAttr = ( )
sem_TraitAttr_Final :: T_TraitAttr
sem_TraitAttr_Final =
    ( )
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    ( )
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    ( )
-- TraitAttrs --------------------------------------------------
-- cata
sem_TraitAttrs :: TraitAttrs ->
                  T_TraitAttrs
sem_TraitAttrs list =
    (Prelude.foldr sem_TraitAttrs_Cons sem_TraitAttrs_Nil (Prelude.map sem_TraitAttr list))
-- semantic domain
type T_TraitAttrs = ( )
sem_TraitAttrs_Cons :: T_TraitAttr ->
                       T_TraitAttrs ->
                       T_TraitAttrs
sem_TraitAttrs_Cons hd_ tl_ =
    ( )
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    ( )
-- TraitData ---------------------------------------------------
-- cata
sem_TraitData :: TraitData ->
                 T_TraitData
sem_TraitData (TraitData_Slot _slotId _tp _vindex _vkind) =
    (sem_TraitData_Slot _slotId _tp _vindex (sem_ValueKind _vkind))
sem_TraitData (TraitData_Const _slotId _tp _vindex _vkind) =
    (sem_TraitData_Const _slotId _tp _vindex (sem_ValueKind _vkind))
sem_TraitData (TraitData_Method _dispId _method) =
    (sem_TraitData_Method _dispId _method)
sem_TraitData (TraitData_Getter _dispId _method) =
    (sem_TraitData_Getter _dispId _method)
sem_TraitData (TraitData_Setter _dispId _method) =
    (sem_TraitData_Setter _dispId _method)
sem_TraitData (TraitData_Function _dispId _method) =
    (sem_TraitData_Function _dispId _method)
sem_TraitData (TraitData_Class _slotId _class) =
    (sem_TraitData_Class _slotId _class)
-- semantic domain
type T_TraitData = ( TraitBody)
sem_TraitData_Slot :: Word32 ->
                      Word32 ->
                      Word32 ->
                      T_ValueKind ->
                      T_TraitData
sem_TraitData_Slot slotId_ tp_ vindex_ vkind_ =
    (case (({-# LINE 222 "src\\SymbolTables.ag" #-}
            TraitField $ Ref tp_
            {-# LINE 2987 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Const :: Word32 ->
                       Word32 ->
                       Word32 ->
                       T_ValueKind ->
                       T_TraitData
sem_TraitData_Const slotId_ tp_ vindex_ vkind_ =
    (case (({-# LINE 222 "src\\SymbolTables.ag" #-}
            TraitField $ Ref tp_
            {-# LINE 2999 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (case (({-# LINE 223 "src\\SymbolTables.ag" #-}
            TraitMethod $ Ref method_
            {-# LINE 3009 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (case (({-# LINE 223 "src\\SymbolTables.ag" #-}
            TraitMethod $ Ref method_
            {-# LINE 3019 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (case (({-# LINE 223 "src\\SymbolTables.ag" #-}
            TraitMethod $ Ref method_
            {-# LINE 3029 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (case (({-# LINE 223 "src\\SymbolTables.ag" #-}
            TraitMethod $ Ref method_
            {-# LINE 3039 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (case (({-# LINE 224 "src\\SymbolTables.ag" #-}
            TraitClass $ Ref class_
            {-# LINE 3049 "src/SymbolTables.hs" #-}
            )) of
     { _lhsObody | _lhsObody `seq` (True) ->
     ( _lhsObody) })
-- TraitKind ---------------------------------------------------
-- cata
sem_TraitKind :: TraitKind ->
                 T_TraitKind
sem_TraitKind (TraitKind_Slot) =
    (sem_TraitKind_Slot)
sem_TraitKind (TraitKind_Method) =
    (sem_TraitKind_Method)
sem_TraitKind (TraitKind_Getter) =
    (sem_TraitKind_Getter)
sem_TraitKind (TraitKind_Setter) =
    (sem_TraitKind_Setter)
sem_TraitKind (TraitKind_Class) =
    (sem_TraitKind_Class)
sem_TraitKind (TraitKind_Function) =
    (sem_TraitKind_Function)
sem_TraitKind (TraitKind_Const) =
    (sem_TraitKind_Const)
-- semantic domain
type T_TraitKind = ( )
sem_TraitKind_Slot :: T_TraitKind
sem_TraitKind_Slot =
    ( )
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    ( )
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    ( )
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    ( )
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    ( )
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    ( )
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    ( )
-- TraitMeta ---------------------------------------------------
-- cata
sem_TraitMeta :: TraitMeta ->
                 T_TraitMeta
sem_TraitMeta list =
    (Prelude.foldr sem_TraitMeta_Cons sem_TraitMeta_Nil list)
-- semantic domain
type T_TraitMeta = ( )
sem_TraitMeta_Cons :: Word32 ->
                      T_TraitMeta ->
                      T_TraitMeta
sem_TraitMeta_Cons hd_ tl_ =
    ( )
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    ( )
-- Traits ------------------------------------------------------
-- cata
sem_Traits :: Traits ->
              T_Traits
sem_Traits list =
    (Prelude.foldr sem_Traits_Cons sem_Traits_Nil (Prelude.map sem_Trait list))
-- semantic domain
type T_Traits = ( TraitDescrs)
sem_Traits_Cons :: T_Trait ->
                   T_Traits ->
                   T_Traits
sem_Traits_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIgathInfos) | True ->
         (case (hd_) of
          { ( _hdItraitInfo) | True ->
              (case (({-# LINE 214 "src\\SymbolTables.ag" #-}
                      _hdItraitInfo : _tlIgathInfos
                      {-# LINE 3128 "src/SymbolTables.hs" #-}
                      )) of
               { _lhsOgathInfos | _lhsOgathInfos `seq` (True) ->
               ( _lhsOgathInfos) }) }) })
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (case (({-# LINE 213 "src\\SymbolTables.ag" #-}
            []
            {-# LINE 3136 "src/SymbolTables.hs" #-}
            )) of
     { _lhsOgathInfos | _lhsOgathInfos `seq` (True) ->
     ( _lhsOgathInfos) })
-- ValueKind ---------------------------------------------------
-- cata
sem_ValueKind :: ValueKind ->
                 T_ValueKind
sem_ValueKind (ValueKind_Int) =
    (sem_ValueKind_Int)
sem_ValueKind (ValueKind_UInt) =
    (sem_ValueKind_UInt)
sem_ValueKind (ValueKind_Double) =
    (sem_ValueKind_Double)
sem_ValueKind (ValueKind_Utf8) =
    (sem_ValueKind_Utf8)
sem_ValueKind (ValueKind_True) =
    (sem_ValueKind_True)
sem_ValueKind (ValueKind_False) =
    (sem_ValueKind_False)
sem_ValueKind (ValueKind_Null) =
    (sem_ValueKind_Null)
sem_ValueKind (ValueKind_Undefined) =
    (sem_ValueKind_Undefined)
sem_ValueKind (ValueKind_Namespace) =
    (sem_ValueKind_Namespace)
sem_ValueKind (ValueKind_Package) =
    (sem_ValueKind_Package)
sem_ValueKind (ValueKind_Internal) =
    (sem_ValueKind_Internal)
sem_ValueKind (ValueKind_Protected) =
    (sem_ValueKind_Protected)
sem_ValueKind (ValueKind_Explicit) =
    (sem_ValueKind_Explicit)
sem_ValueKind (ValueKind_Static) =
    (sem_ValueKind_Static)
sem_ValueKind (ValueKind_Private) =
    (sem_ValueKind_Private)
-- semantic domain
type T_ValueKind = ( )
sem_ValueKind_Int :: T_ValueKind
sem_ValueKind_Int =
    ( )
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    ( )
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    ( )
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    ( )
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    ( )
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    ( )
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    ( )
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    ( )
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    ( )
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    ( )
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    ( )
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    ( )
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    ( )
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    ( )
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    ( )