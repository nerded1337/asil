

-- UUAGC 0.9.52.1 (src/TrfInjectAbc.ag)
module TrfInjectAbc(injectAbc) where

{-# LINE 6 "src\\TrfInjectAbc.ag" #-}

import Data.ByteString.Lazy(ByteString,pack)
import ByteCode
import Data.Word
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Codec.Binary.UTF8.String
{-# LINE 17 "src/TrfInjectAbc.hs" #-}
{-# LINE 20 "src\\TrfInjectAbc.ag" #-}

-- | Injects the given .abc code after the last .abc block in the flash file.
--   If you want to inject multiple chunks of .abc code, subjected
--   to a partial order: convert it in a total order and inject
--   in that order.
injectAbc :: String -> AbcFile -> SwfFile -> SwfFile
injectAbc name code file = out where
  inh = Inh_SwfFile { abc_Inh_SwfFile = code, name_Inh_SwfFile = name }
  sem = sem_SwfFile file
  syn = wrap_SwfFile sem inh
  out = output_Syn_SwfFile syn
{-# LINE 30 "src/TrfInjectAbc.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = ( AbcFile)
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
    (case (bodies_) of
     { ( _bodiesIoutput) | True ->
         (case (scripts_) of
          { ( _scriptsIoutput) | True ->
              (case (classes_) of
               { ( _classesIoutput) | True ->
                   (case (instances_) of
                    { ( _instancesIoutput) | True ->
                        (case (metadatas_) of
                         { ( _metadatasIoutput) | True ->
                             (case (methods_) of
                              { ( _methodsIoutput) | True ->
                                  (case (constantPool_) of
                                   { ( _constantPoolIoutput) | True ->
                                       (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                               AbcFile_File minorVersion_ majorVersion_ _constantPoolIoutput _methodsIoutput _metadatasIoutput _instancesIoutput _classesIoutput _scriptsIoutput _bodiesIoutput
                                               {-# LINE 66 "src/TrfInjectAbc.hs" #-}
                                               )) of
                                        { _output | _output `seq` (True) ->
                                        (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                                _output
                                                {-# LINE 71 "src/TrfInjectAbc.hs" #-}
                                                )) of
                                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                         ( _lhsOoutput) }) }) }) }) }) }) }) }) })
-- AbcFlag -----------------------------------------------------
-- cata
sem_AbcFlag :: AbcFlag ->
               T_AbcFlag
sem_AbcFlag (AbcFlag_LazyInit) =
    (sem_AbcFlag_LazyInit)
-- semantic domain
type T_AbcFlag = ( AbcFlag)
sem_AbcFlag_LazyInit :: T_AbcFlag
sem_AbcFlag_LazyInit =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            AbcFlag_LazyInit
            {-# LINE 87 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 92 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- AbcFlags ----------------------------------------------------
-- cata
sem_AbcFlags :: AbcFlags ->
                T_AbcFlags
sem_AbcFlags list =
    (Prelude.foldr sem_AbcFlags_Cons sem_AbcFlags_Nil (Prelude.map sem_AbcFlag list))
-- semantic domain
type T_AbcFlags = ( AbcFlags)
sem_AbcFlags_Cons :: T_AbcFlag ->
                     T_AbcFlags ->
                     T_AbcFlags
sem_AbcFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 114 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 119 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_AbcFlags_Nil :: T_AbcFlags
sem_AbcFlags_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 127 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 132 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- BodyInfo ----------------------------------------------------
-- cata
sem_BodyInfo :: BodyInfo ->
                T_BodyInfo
sem_BodyInfo (BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth _instructions _exceptions _traits) =
    (sem_BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth (sem_Instructions _instructions) (sem_Exceptions _exceptions) (sem_Traits _traits))
-- semantic domain
type T_BodyInfo = ( BodyInfo)
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
    (case (traits_) of
     { ( _traitsIoutput) | True ->
         (case (exceptions_) of
          { ( _exceptionsIoutput) | True ->
              (case (instructions_) of
               { ( _instructionsIoutput) | True ->
                   (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                           BodyInfo_Info method_ maxStack_ localCount_ initScopeDepth_ maxScopeDepth_ _instructionsIoutput _exceptionsIoutput _traitsIoutput
                           {-# LINE 162 "src/TrfInjectAbc.hs" #-}
                           )) of
                    { _output | _output `seq` (True) ->
                    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                            _output
                            {-# LINE 167 "src/TrfInjectAbc.hs" #-}
                            )) of
                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                     ( _lhsOoutput) }) }) }) }) })
-- BodyInfos ---------------------------------------------------
-- cata
sem_BodyInfos :: BodyInfos ->
                 T_BodyInfos
sem_BodyInfos list =
    (Prelude.foldr sem_BodyInfos_Cons sem_BodyInfos_Nil (Prelude.map sem_BodyInfo list))
-- semantic domain
type T_BodyInfos = ( BodyInfos)
sem_BodyInfos_Cons :: T_BodyInfo ->
                      T_BodyInfos ->
                      T_BodyInfos
sem_BodyInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 189 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 194 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_BodyInfos_Nil :: T_BodyInfos
sem_BodyInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 202 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 207 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- CaseOffsets -------------------------------------------------
-- cata
sem_CaseOffsets :: CaseOffsets ->
                   T_CaseOffsets
sem_CaseOffsets list =
    (Prelude.foldr sem_CaseOffsets_Cons sem_CaseOffsets_Nil list)
-- semantic domain
type T_CaseOffsets = ( CaseOffsets)
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 227 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 232 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 240 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 245 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- ClassInfo ---------------------------------------------------
-- cata
sem_ClassInfo :: ClassInfo ->
                 T_ClassInfo
sem_ClassInfo (ClassInfo_Info _con _traits) =
    (sem_ClassInfo_Info _con (sem_Traits _traits))
-- semantic domain
type T_ClassInfo = ( ClassInfo)
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    (case (traits_) of
     { ( _traitsIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 ClassInfo_Info con_ _traitsIoutput
                 {-# LINE 265 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 270 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = ( ClassInfos)
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 292 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 297 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 305 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 310 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- DebugType ---------------------------------------------------
-- cata
sem_DebugType :: DebugType ->
                 T_DebugType
sem_DebugType (DebugType_Local) =
    (sem_DebugType_Local)
-- semantic domain
type T_DebugType = ( DebugType)
sem_DebugType_Local :: T_DebugType
sem_DebugType_Local =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            DebugType_Local
            {-# LINE 326 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 331 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Exception ---------------------------------------------------
-- cata
sem_Exception :: Exception ->
                 T_Exception
sem_Exception (Exception_Info _from _to _target _tp _name) =
    (sem_Exception_Info _from _to _target _tp _name)
-- semantic domain
type T_Exception = ( Exception)
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Exception_Info from_ to_ target_ tp_ name_
            {-# LINE 352 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 357 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = ( Exceptions)
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 379 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 384 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 392 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 397 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_InstanceFlag = ( InstanceFlag)
sem_InstanceFlag_ClassSealed :: T_InstanceFlag
sem_InstanceFlag_ClassSealed =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            InstanceFlag_ClassSealed
            {-# LINE 419 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 424 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_InstanceFlag_ClassFinal :: T_InstanceFlag
sem_InstanceFlag_ClassFinal =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            InstanceFlag_ClassFinal
            {-# LINE 432 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 437 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_InstanceFlag_ClassInterface :: T_InstanceFlag
sem_InstanceFlag_ClassInterface =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            InstanceFlag_ClassInterface
            {-# LINE 445 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 450 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_InstanceFlag_ClassProtected :: T_InstanceFlag
sem_InstanceFlag_ClassProtected =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            InstanceFlag_ClassProtected
            {-# LINE 458 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 463 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- InstanceFlags -----------------------------------------------
-- cata
sem_InstanceFlags :: InstanceFlags ->
                     T_InstanceFlags
sem_InstanceFlags list =
    (Prelude.foldr sem_InstanceFlags_Cons sem_InstanceFlags_Nil (Prelude.map sem_InstanceFlag list))
-- semantic domain
type T_InstanceFlags = ( InstanceFlags)
sem_InstanceFlags_Cons :: T_InstanceFlag ->
                          T_InstanceFlags ->
                          T_InstanceFlags
sem_InstanceFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 485 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 490 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_InstanceFlags_Nil :: T_InstanceFlags
sem_InstanceFlags_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 498 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 503 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- InstanceInfo ------------------------------------------------
-- cata
sem_InstanceInfo :: InstanceInfo ->
                    T_InstanceInfo
sem_InstanceInfo (InstanceInfo_Info _name _super _flags _protectedNs _interfaces _constructor _traits) =
    (sem_InstanceInfo_Info _name _super (sem_InstanceFlags _flags) _protectedNs (sem_Interfaces _interfaces) _constructor (sem_Traits _traits))
-- semantic domain
type T_InstanceInfo = ( InstanceInfo)
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
     { ( _traitsIoutput) | True ->
         (case (interfaces_) of
          { ( _interfacesIoutput) | True ->
              (case (flags_) of
               { ( _flagsIoutput) | True ->
                   (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                           InstanceInfo_Info name_ super_ _flagsIoutput protectedNs_ _interfacesIoutput constructor_ _traitsIoutput
                           {-# LINE 532 "src/TrfInjectAbc.hs" #-}
                           )) of
                    { _output | _output `seq` (True) ->
                    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                            _output
                            {-# LINE 537 "src/TrfInjectAbc.hs" #-}
                            )) of
                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                     ( _lhsOoutput) }) }) }) }) })
-- InstanceInfos -----------------------------------------------
-- cata
sem_InstanceInfos :: InstanceInfos ->
                     T_InstanceInfos
sem_InstanceInfos list =
    (Prelude.foldr sem_InstanceInfos_Cons sem_InstanceInfos_Nil (Prelude.map sem_InstanceInfo list))
-- semantic domain
type T_InstanceInfos = ( InstanceInfos)
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 559 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 564 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 572 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 577 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_Instruction = ( Instruction)
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Add
            {-# LINE 943 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 948 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Add_i
            {-# LINE 956 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 961 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Add_d
            {-# LINE 969 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 974 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_ApplyType name_
            {-# LINE 983 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 988 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_AsType name_
            {-# LINE 997 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1002 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_AsTypeLate
            {-# LINE 1010 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1015 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Breakpoint
            {-# LINE 1023 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1028 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_BreakLine line_
            {-# LINE 1037 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1042 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_BitAnd
            {-# LINE 1050 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1055 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_BitNot
            {-# LINE 1063 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1068 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_BitOr
            {-# LINE 1076 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1081 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_BitXor
            {-# LINE 1089 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1094 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Call argCount_
            {-# LINE 1103 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1108 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallInterface name_ argCount_
            {-# LINE 1118 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1123 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallMethod index_ argCount_
            {-# LINE 1133 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1138 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallProp name_ argCount_
            {-# LINE 1148 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1153 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallPropLex name_ argCount_
            {-# LINE 1163 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1168 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallPropVoid name_ argCount_
            {-# LINE 1178 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1183 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallStatic method_ argCount_
            {-# LINE 1193 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1198 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallSuper name_ argCount_
            {-# LINE 1208 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1213 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallSuperId
            {-# LINE 1221 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1226 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CallSuperVoid name_ argCount_
            {-# LINE 1236 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1241 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_CheckFilter
            {-# LINE 1249 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1254 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce name_
            {-# LINE 1263 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1268 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_a
            {-# LINE 1276 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1281 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_b
            {-# LINE 1289 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1294 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_d
            {-# LINE 1302 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1307 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_i
            {-# LINE 1315 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1320 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_o
            {-# LINE 1328 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1333 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_s
            {-# LINE 1341 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1346 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Coerce_u
            {-# LINE 1354 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1359 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Concat
            {-# LINE 1367 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1372 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Construct argCount_
            {-# LINE 1381 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1386 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_ConstructProp name_ argCount_
            {-# LINE 1396 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1401 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_ConstructSuper argCount_
            {-# LINE 1410 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1415 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_b
            {-# LINE 1423 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1428 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_i
            {-# LINE 1436 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1441 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_d
            {-# LINE 1449 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1454 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_o
            {-# LINE 1462 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1467 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_u
            {-# LINE 1475 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1480 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Convert_s
            {-# LINE 1488 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1493 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    (case (tp_) of
     { ( _tpIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 Instruction_Debug _tpIoutput name_ reg_ extra_
                 {-# LINE 1507 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 1512 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DebugFile name_
            {-# LINE 1521 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1526 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DebugLine line_
            {-# LINE 1535 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1540 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DecLocal reg_
            {-# LINE 1549 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1554 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DecLocal_i reg_
            {-# LINE 1563 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1568 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Decrement
            {-# LINE 1576 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1581 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Decrement_i
            {-# LINE 1589 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1594 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DeleteProperty name_
            {-# LINE 1603 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1608 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DeletePropertyLate
            {-# LINE 1616 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1621 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Divide
            {-# LINE 1629 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1634 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Dup
            {-# LINE 1642 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1647 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Dxns name_
            {-# LINE 1656 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1661 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_DxnsLate
            {-# LINE 1669 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1674 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Equals
            {-# LINE 1682 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1687 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_EscXAttr
            {-# LINE 1695 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1700 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_EscXElem
            {-# LINE 1708 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1713 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_FindDef name_
            {-# LINE 1722 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1727 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_FindPropertyGlobalStrict name_
            {-# LINE 1736 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1741 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_FindPropertyGlobal name_
            {-# LINE 1750 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1755 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_FindProperty name_
            {-# LINE 1764 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1769 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_FindPropStrict name_
            {-# LINE 1778 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1783 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetDescendants name_
            {-# LINE 1792 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1797 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetGlobalScope
            {-# LINE 1805 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1810 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetGlobalSlot slot_
            {-# LINE 1819 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1824 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLex name_
            {-# LINE 1833 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1838 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLocal reg_
            {-# LINE 1847 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1852 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLocal0
            {-# LINE 1860 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1865 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLocal1
            {-# LINE 1873 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1878 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLocal2
            {-# LINE 1886 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1891 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetLocal3
            {-# LINE 1899 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1904 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetOuterScope name_
            {-# LINE 1913 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1918 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetProperty name_
            {-# LINE 1927 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1932 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetScopeObject index_
            {-# LINE 1941 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1946 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetSlot slot_
            {-# LINE 1955 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1960 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GetSuper name_
            {-# LINE 1969 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1974 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GreaterEquals
            {-# LINE 1982 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 1987 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_GreaterThan
            {-# LINE 1995 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2000 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_HasNext
            {-# LINE 2008 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2013 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_HasNext2 objectReg_ indexReg_
            {-# LINE 2023 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2028 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfEq offset_
            {-# LINE 2037 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2042 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfFalse offset_
            {-# LINE 2051 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2056 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfGe offset_
            {-# LINE 2065 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2070 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfGt offset_
            {-# LINE 2079 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2084 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfLe offset_
            {-# LINE 2093 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2098 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfLt offset_
            {-# LINE 2107 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2112 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfNGe offset_
            {-# LINE 2121 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2126 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfNGt offset_
            {-# LINE 2135 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2140 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfNLe offset_
            {-# LINE 2149 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2154 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfNLt offset_
            {-# LINE 2163 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2168 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfNe offset_
            {-# LINE 2177 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2182 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfStrictEq offset_
            {-# LINE 2191 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2196 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfStrictNe offset_
            {-# LINE 2205 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2210 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IfTrue offset_
            {-# LINE 2219 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2224 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_In
            {-# LINE 2232 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2237 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IncLocal reg_
            {-# LINE 2246 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2251 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IncLocal_i reg_
            {-# LINE 2260 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2265 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Increment
            {-# LINE 2273 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2278 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Increment_i
            {-# LINE 2286 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2291 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_InitProperty name_
            {-# LINE 2300 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2305 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_InstanceOf
            {-# LINE 2313 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2318 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IsType name_
            {-# LINE 2327 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2332 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_IsTypeLate
            {-# LINE 2340 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2345 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Jump offset_
            {-# LINE 2354 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2359 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Kill reg_
            {-# LINE 2368 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2373 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Label
            {-# LINE 2381 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2386 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LessEquals
            {-# LINE 2394 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2399 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LessThan
            {-# LINE 2407 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2412 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LoadFloat32
            {-# LINE 2420 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2425 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LoadFloat64
            {-# LINE 2433 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2438 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LoadIndirect8
            {-# LINE 2446 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2451 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LoadIndirect16
            {-# LINE 2459 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2464 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_LoadIndirect32
            {-# LINE 2472 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2477 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    (case (caseOffsets_) of
     { ( _caseOffsetsIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 Instruction_LookupSwitch defaultOffset_ _caseOffsetsIoutput
                 {-# LINE 2489 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 2494 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Lshift
            {-# LINE 2502 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2507 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Modulo
            {-# LINE 2515 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2520 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Multiply
            {-# LINE 2528 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2533 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Multiply_i
            {-# LINE 2541 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2546 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Negate
            {-# LINE 2554 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2559 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Negate_i
            {-# LINE 2567 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2572 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewActivation
            {-# LINE 2580 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2585 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewArray argCount_
            {-# LINE 2594 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2599 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewCatch exception_
            {-# LINE 2608 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2613 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewClass class_
            {-# LINE 2622 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2627 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewFunction method_
            {-# LINE 2636 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2641 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NewObject argCount_
            {-# LINE 2650 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2655 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NextName
            {-# LINE 2663 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2668 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_NextValue
            {-# LINE 2676 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2681 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Nop
            {-# LINE 2689 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2694 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Not
            {-# LINE 2702 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2707 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Pop
            {-# LINE 2715 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2720 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PopScope
            {-# LINE 2728 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2733 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushByte val_
            {-# LINE 2742 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2747 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushDouble name_
            {-# LINE 2756 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2761 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushFalse
            {-# LINE 2769 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2774 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushInt name_
            {-# LINE 2783 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2788 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushNamespace name_
            {-# LINE 2797 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2802 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushNaN
            {-# LINE 2810 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2815 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushNull
            {-# LINE 2823 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2828 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushScope
            {-# LINE 2836 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2841 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushShort val_
            {-# LINE 2850 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2855 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushString name_
            {-# LINE 2864 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2869 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushTrue
            {-# LINE 2877 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2882 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushUInt name_
            {-# LINE 2891 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2896 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushUndefined
            {-# LINE 2904 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2909 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_PushWith
            {-# LINE 2917 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2922 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_ReturnValue
            {-# LINE 2930 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2935 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_ReturnVoid
            {-# LINE 2943 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2948 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Rshift
            {-# LINE 2956 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2961 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetLocal reg_
            {-# LINE 2970 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2975 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetLocal0
            {-# LINE 2983 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 2988 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetLocal1
            {-# LINE 2996 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3001 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetLocal2
            {-# LINE 3009 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3014 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetLocal3
            {-# LINE 3022 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3027 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetGlobalSlot slot_
            {-# LINE 3036 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3041 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetProperty name_
            {-# LINE 3050 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3055 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetPropertyLate
            {-# LINE 3063 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3068 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetSlot slot_
            {-# LINE 3077 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3082 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SetSuper name_
            {-# LINE 3091 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3096 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SignExtend1
            {-# LINE 3104 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3109 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SignExtend8
            {-# LINE 3117 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3122 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_SignExtend16
            {-# LINE 3130 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3135 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StoreFloat32
            {-# LINE 3143 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3148 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StoreFloat64
            {-# LINE 3156 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3161 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StoreIndirect32
            {-# LINE 3169 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3174 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StoreIndirect16
            {-# LINE 3182 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3187 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StoreIndirect8
            {-# LINE 3195 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3200 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_StrictEquals
            {-# LINE 3208 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3213 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Substract
            {-# LINE 3221 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3226 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Substract_i
            {-# LINE 3234 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3239 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Swap
            {-# LINE 3247 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3252 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Throw
            {-# LINE 3260 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3265 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Timestamp
            {-# LINE 3273 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3278 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_TypeOf
            {-# LINE 3286 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3291 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Urshift
            {-# LINE 3299 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3304 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Instruction_Location index_
            {-# LINE 3313 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3318 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ( Instructions)
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3340 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3345 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3353 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3358 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 3378 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 3383 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3391 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3396 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- MetaInfo ----------------------------------------------------
-- cata
sem_MetaInfo :: MetaInfo ->
                T_MetaInfo
sem_MetaInfo (MetaInfo_Info _name _items) =
    (sem_MetaInfo_Info _name (sem_MetaItems _items))
-- semantic domain
type T_MetaInfo = ( MetaInfo)
sem_MetaInfo_Info :: Word32 ->
                     T_MetaItems ->
                     T_MetaInfo
sem_MetaInfo_Info name_ items_ =
    (case (items_) of
     { ( _itemsIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 MetaInfo_Info name_ _itemsIoutput
                 {-# LINE 3416 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 3421 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- MetaInfos ---------------------------------------------------
-- cata
sem_MetaInfos :: MetaInfos ->
                 T_MetaInfos
sem_MetaInfos list =
    (Prelude.foldr sem_MetaInfos_Cons sem_MetaInfos_Nil (Prelude.map sem_MetaInfo list))
-- semantic domain
type T_MetaInfos = ( MetaInfos)
sem_MetaInfos_Cons :: T_MetaInfo ->
                      T_MetaInfos ->
                      T_MetaInfos
sem_MetaInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3443 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3448 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_MetaInfos_Nil :: T_MetaInfos
sem_MetaInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3456 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3461 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- MetaItem ----------------------------------------------------
-- cata
sem_MetaItem :: MetaItem ->
                T_MetaItem
sem_MetaItem (MetaItem_Item _key _value) =
    (sem_MetaItem_Item _key _value)
-- semantic domain
type T_MetaItem = ( MetaItem)
sem_MetaItem_Item :: Word32 ->
                     Word32 ->
                     T_MetaItem
sem_MetaItem_Item key_ value_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MetaItem_Item key_ value_
            {-# LINE 3479 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3484 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- MetaItems ---------------------------------------------------
-- cata
sem_MetaItems :: MetaItems ->
                 T_MetaItems
sem_MetaItems list =
    (Prelude.foldr sem_MetaItems_Cons sem_MetaItems_Nil (Prelude.map sem_MetaItem list))
-- semantic domain
type T_MetaItems = ( MetaItems)
sem_MetaItems_Cons :: T_MetaItem ->
                      T_MetaItems ->
                      T_MetaItems
sem_MetaItems_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3506 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3511 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_MetaItems_Nil :: T_MetaItems
sem_MetaItems_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3519 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3524 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_MethodFlag = ( MethodFlag)
sem_MethodFlag_NeedArgs :: T_MethodFlag
sem_MethodFlag_NeedArgs =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_NeedArgs
            {-# LINE 3550 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3555 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MethodFlag_NeedAct :: T_MethodFlag
sem_MethodFlag_NeedAct =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_NeedAct
            {-# LINE 3563 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3568 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MethodFlag_NeedRest :: T_MethodFlag
sem_MethodFlag_NeedRest =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_NeedRest
            {-# LINE 3576 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3581 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MethodFlag_HasOptionals :: T_MethodFlag
sem_MethodFlag_HasOptionals =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_HasOptionals
            {-# LINE 3589 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3594 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MethodFlag_SetDXNS :: T_MethodFlag
sem_MethodFlag_SetDXNS =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_SetDXNS
            {-# LINE 3602 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3607 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MethodFlag_HasParamNames :: T_MethodFlag
sem_MethodFlag_HasParamNames =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MethodFlag_HasParamNames
            {-# LINE 3615 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3620 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- MethodFlags -------------------------------------------------
-- cata
sem_MethodFlags :: MethodFlags ->
                   T_MethodFlags
sem_MethodFlags list =
    (Prelude.foldr sem_MethodFlags_Cons sem_MethodFlags_Nil (Prelude.map sem_MethodFlag list))
-- semantic domain
type T_MethodFlags = ( MethodFlags)
sem_MethodFlags_Cons :: T_MethodFlag ->
                        T_MethodFlags ->
                        T_MethodFlags
sem_MethodFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3642 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3647 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_MethodFlags_Nil :: T_MethodFlags
sem_MethodFlags_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3655 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3660 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- MethodInfo --------------------------------------------------
-- cata
sem_MethodInfo :: MethodInfo ->
                  T_MethodInfo
sem_MethodInfo (MethodInfo_Info _return _params _name _flags _options _names) =
    (sem_MethodInfo_Info _return (sem_ParamTypes _params) _name (sem_MethodFlags _flags) (sem_Optionals _options) (sem_ParamNames _names))
-- semantic domain
type T_MethodInfo = ( MethodInfo)
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    (case (names_) of
     { ( _namesIoutput) | True ->
         (case (options_) of
          { ( _optionsIoutput) | True ->
              (case (flags_) of
               { ( _flagsIoutput) | True ->
                   (case (params_) of
                    { ( _paramsIoutput) | True ->
                        (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                MethodInfo_Info return_ _paramsIoutput name_ _flagsIoutput _optionsIoutput _namesIoutput
                                {-# LINE 3690 "src/TrfInjectAbc.hs" #-}
                                )) of
                         { _output | _output `seq` (True) ->
                         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                 _output
                                 {-# LINE 3695 "src/TrfInjectAbc.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) })
-- MethodInfos -------------------------------------------------
-- cata
sem_MethodInfos :: MethodInfos ->
                   T_MethodInfos
sem_MethodInfos list =
    (Prelude.foldr sem_MethodInfos_Cons sem_MethodInfos_Nil (Prelude.map sem_MethodInfo list))
-- semantic domain
type T_MethodInfos = ( MethodInfos)
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3717 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3722 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3730 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3735 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_MultinameInfo = ( MultinameInfo)
sem_MultinameInfo_QName :: Word32 ->
                           Word32 ->
                           T_MultinameInfo
sem_MultinameInfo_QName namespace_ name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_QName namespace_ name_
            {-# LINE 3773 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3778 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_QNameA :: Word32 ->
                            Word32 ->
                            T_MultinameInfo
sem_MultinameInfo_QNameA namespace_ name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_QNameA namespace_ name_
            {-# LINE 3788 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3793 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_RTQName :: Word32 ->
                             T_MultinameInfo
sem_MultinameInfo_RTQName name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_RTQName name_
            {-# LINE 3802 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3807 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_RTQNameA :: Word32 ->
                              T_MultinameInfo
sem_MultinameInfo_RTQNameA name_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_RTQNameA name_
            {-# LINE 3816 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3821 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_RTQNameL :: T_MultinameInfo
sem_MultinameInfo_RTQNameL =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_RTQNameL
            {-# LINE 3829 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3834 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_RTQNameLA :: T_MultinameInfo
sem_MultinameInfo_RTQNameLA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_RTQNameLA
            {-# LINE 3842 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3847 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_Multiname :: Word32 ->
                               Word32 ->
                               T_MultinameInfo
sem_MultinameInfo_Multiname name_ set_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_Multiname name_ set_
            {-# LINE 3857 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3862 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_MultinameA :: Word32 ->
                                Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameA name_ set_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_MultinameA name_ set_
            {-# LINE 3872 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3877 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_MultinameL :: Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameL set_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_MultinameL set_
            {-# LINE 3886 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3891 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_MultinameLA :: Word32 ->
                                 T_MultinameInfo
sem_MultinameInfo_MultinameLA set_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameInfo_MultinameLA set_
            {-# LINE 3900 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3905 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameInfo_Generic :: Word32 ->
                             T_ParamNames ->
                             T_MultinameInfo
sem_MultinameInfo_Generic name_ params_ =
    (case (params_) of
     { ( _paramsIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 MultinameInfo_Generic name_ _paramsIoutput
                 {-# LINE 3917 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 3922 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- MultinameInfos ----------------------------------------------
-- cata
sem_MultinameInfos :: MultinameInfos ->
                      T_MultinameInfos
sem_MultinameInfos list =
    (Prelude.foldr sem_MultinameInfos_Cons sem_MultinameInfos_Nil (Prelude.map sem_MultinameInfo list))
-- semantic domain
type T_MultinameInfos = ( MultinameInfos)
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 3944 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 3949 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 3957 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 3962 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_MultinameKind = ( MultinameKind)
sem_MultinameKind_QName :: T_MultinameKind
sem_MultinameKind_QName =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_QName
            {-# LINE 3998 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4003 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_QNameA :: T_MultinameKind
sem_MultinameKind_QNameA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_QNameA
            {-# LINE 4011 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4016 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_RTQName :: T_MultinameKind
sem_MultinameKind_RTQName =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_RTQName
            {-# LINE 4024 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4029 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_RTQNameA :: T_MultinameKind
sem_MultinameKind_RTQNameA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_RTQNameA
            {-# LINE 4037 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4042 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_RTQNameL :: T_MultinameKind
sem_MultinameKind_RTQNameL =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_RTQNameL
            {-# LINE 4050 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4055 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_RTQNameLA :: T_MultinameKind
sem_MultinameKind_RTQNameLA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_RTQNameLA
            {-# LINE 4063 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4068 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_Multiname :: T_MultinameKind
sem_MultinameKind_Multiname =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_Multiname
            {-# LINE 4076 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4081 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_MultinameA :: T_MultinameKind
sem_MultinameKind_MultinameA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_MultinameA
            {-# LINE 4089 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4094 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_MultinameL :: T_MultinameKind
sem_MultinameKind_MultinameL =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_MultinameL
            {-# LINE 4102 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4107 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_MultinameLA :: T_MultinameKind
sem_MultinameKind_MultinameLA =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_MultinameLA
            {-# LINE 4115 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4120 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_MultinameKind_Generic :: T_MultinameKind
sem_MultinameKind_Generic =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            MultinameKind_Generic
            {-# LINE 4128 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4133 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- NamespaceInfo -----------------------------------------------
-- cata
sem_NamespaceInfo :: NamespaceInfo ->
                     T_NamespaceInfo
sem_NamespaceInfo (NamespaceInfo_Info _kind _name) =
    (sem_NamespaceInfo_Info (sem_NamespaceKind _kind) _name)
-- semantic domain
type T_NamespaceInfo = ( NamespaceInfo)
sem_NamespaceInfo_Info :: T_NamespaceKind ->
                          Word32 ->
                          T_NamespaceInfo
sem_NamespaceInfo_Info kind_ name_ =
    (case (kind_) of
     { ( _kindIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 NamespaceInfo_Info _kindIoutput name_
                 {-# LINE 4153 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4158 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- NamespaceInfos ----------------------------------------------
-- cata
sem_NamespaceInfos :: NamespaceInfos ->
                      T_NamespaceInfos
sem_NamespaceInfos list =
    (Prelude.foldr sem_NamespaceInfos_Cons sem_NamespaceInfos_Nil (Prelude.map sem_NamespaceInfo list))
-- semantic domain
type T_NamespaceInfos = ( NamespaceInfos)
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 4180 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 4185 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4193 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4198 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_NamespaceKind = ( NamespaceKind)
sem_NamespaceKind_General :: T_NamespaceKind
sem_NamespaceKind_General =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_General
            {-# LINE 4226 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4231 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Package :: T_NamespaceKind
sem_NamespaceKind_Package =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Package
            {-# LINE 4239 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4244 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Internal :: T_NamespaceKind
sem_NamespaceKind_Internal =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Internal
            {-# LINE 4252 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4257 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Protected :: T_NamespaceKind
sem_NamespaceKind_Protected =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Protected
            {-# LINE 4265 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4270 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Explicit :: T_NamespaceKind
sem_NamespaceKind_Explicit =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Explicit
            {-# LINE 4278 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4283 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Static :: T_NamespaceKind
sem_NamespaceKind_Static =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Static
            {-# LINE 4291 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4296 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_NamespaceKind_Private :: T_NamespaceKind
sem_NamespaceKind_Private =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            NamespaceKind_Private
            {-# LINE 4304 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4309 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4329 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4334 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4342 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4347 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Optional ----------------------------------------------------
-- cata
sem_Optional :: Optional ->
                T_Optional
sem_Optional (Optional_Detail _val _kind) =
    (sem_Optional_Detail _val (sem_ValueKind _kind))
-- semantic domain
type T_Optional = ( Optional)
sem_Optional_Detail :: Word32 ->
                       T_ValueKind ->
                       T_Optional
sem_Optional_Detail val_ kind_ =
    (case (kind_) of
     { ( _kindIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 Optional_Detail val_ _kindIoutput
                 {-# LINE 4367 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4372 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- Optionals ---------------------------------------------------
-- cata
sem_Optionals :: Optionals ->
                 T_Optionals
sem_Optionals list =
    (Prelude.foldr sem_Optionals_Cons sem_Optionals_Nil (Prelude.map sem_Optional list))
-- semantic domain
type T_Optionals = ( Optionals)
sem_Optionals_Cons :: T_Optional ->
                      T_Optionals ->
                      T_Optionals
sem_Optionals_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 4394 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 4399 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_Optionals_Nil :: T_Optionals
sem_Optionals_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4407 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4412 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4432 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4437 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_ParamNames_Nil :: T_ParamNames
sem_ParamNames_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4445 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4450 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4470 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4475 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4483 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4488 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = ( PoolDoubles)
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4508 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4513 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4521 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4526 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = ( PoolInfo)
sem_PoolInfo_Info :: T_PoolInts ->
                     T_PoolUInts ->
                     T_PoolDoubles ->
                     T_PoolStrings ->
                     T_NamespaceInfos ->
                     T_SetInfos ->
                     T_MultinameInfos ->
                     T_PoolInfo
sem_PoolInfo_Info integers_ uintegers_ doubles_ strings_ namespaces_ namesets_ multinames_ =
    (case (multinames_) of
     { ( _multinamesIoutput) | True ->
         (case (namesets_) of
          { ( _namesetsIoutput) | True ->
              (case (namespaces_) of
               { ( _namespacesIoutput) | True ->
                   (case (strings_) of
                    { ( _stringsIoutput) | True ->
                        (case (doubles_) of
                         { ( _doublesIoutput) | True ->
                             (case (uintegers_) of
                              { ( _uintegersIoutput) | True ->
                                  (case (integers_) of
                                   { ( _integersIoutput) | True ->
                                       (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                               PoolInfo_Info _integersIoutput _uintegersIoutput _doublesIoutput _stringsIoutput _namespacesIoutput _namesetsIoutput _multinamesIoutput
                                               {-# LINE 4563 "src/TrfInjectAbc.hs" #-}
                                               )) of
                                        { _output | _output `seq` (True) ->
                                        (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                                _output
                                                {-# LINE 4568 "src/TrfInjectAbc.hs" #-}
                                                )) of
                                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                         ( _lhsOoutput) }) }) }) }) }) }) }) }) })
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = ( PoolInts)
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4588 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4593 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4601 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4606 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = ( PoolStrings)
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4626 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4631 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4639 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4644 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = ( PoolUInts)
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 4664 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4669 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4677 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4682 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Rect --------------------------------------------------------
-- cata
sem_Rect :: Rect ->
            T_Rect
sem_Rect (Rect_Rect _bits _xMin _xMax _yMin _yMax) =
    (sem_Rect_Rect _bits _xMin _xMax _yMin _yMax)
-- semantic domain
type T_Rect = ( Rect)
sem_Rect_Rect :: Int ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 T_Rect
sem_Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_
            {-# LINE 4703 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4708 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- ScriptInfo --------------------------------------------------
-- cata
sem_ScriptInfo :: ScriptInfo ->
                  T_ScriptInfo
sem_ScriptInfo (ScriptInfo_Info _method _traits) =
    (sem_ScriptInfo_Info _method (sem_Traits _traits))
-- semantic domain
type T_ScriptInfo = ( ScriptInfo)
sem_ScriptInfo_Info :: Word32 ->
                       T_Traits ->
                       T_ScriptInfo
sem_ScriptInfo_Info method_ traits_ =
    (case (traits_) of
     { ( _traitsIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 ScriptInfo_Info method_ _traitsIoutput
                 {-# LINE 4728 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4733 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- ScriptInfos -------------------------------------------------
-- cata
sem_ScriptInfos :: ScriptInfos ->
                   T_ScriptInfos
sem_ScriptInfos list =
    (Prelude.foldr sem_ScriptInfos_Cons sem_ScriptInfos_Nil (Prelude.map sem_ScriptInfo list))
-- semantic domain
type T_ScriptInfos = ( ScriptInfos)
sem_ScriptInfos_Cons :: T_ScriptInfo ->
                        T_ScriptInfos ->
                        T_ScriptInfos
sem_ScriptInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 4755 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 4760 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_ScriptInfos_Nil :: T_ScriptInfos
sem_ScriptInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4768 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4773 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- SetInfo -----------------------------------------------------
-- cata
sem_SetInfo :: SetInfo ->
               T_SetInfo
sem_SetInfo (SetInfo_Info _names) =
    (sem_SetInfo_Info (sem_NamespaceNames _names))
-- semantic domain
type T_SetInfo = ( SetInfo)
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    (case (names_) of
     { ( _namesIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 SetInfo_Info _namesIoutput
                 {-# LINE 4792 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 4797 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
-- SetInfos ----------------------------------------------------
-- cata
sem_SetInfos :: SetInfos ->
                T_SetInfos
sem_SetInfos list =
    (Prelude.foldr sem_SetInfos_Cons sem_SetInfos_Nil (Prelude.map sem_SetInfo list))
-- semantic domain
type T_SetInfos = ( SetInfos)
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 4819 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 4824 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 4832 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 4837 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = AbcFile ->
                 String ->
                 ( SwfFile)
data Inh_SwfFile = Inh_SwfFile {abc_Inh_SwfFile :: !(AbcFile),name_Inh_SwfFile :: !(String)}
data Syn_SwfFile = Syn_SwfFile {output_Syn_SwfFile :: !(SwfFile)}
wrap_SwfFile :: T_SwfFile ->
                Inh_SwfFile ->
                Syn_SwfFile
wrap_SwfFile sem (Inh_SwfFile _lhsIabc _lhsIname) =
    (let ( _lhsOoutput) | True = sem _lhsIabc _lhsIname
     in  (Syn_SwfFile _lhsOoutput))
sem_SwfFile_File :: Bool ->
                    Word8 ->
                    Word32 ->
                    T_Rect ->
                    Word16 ->
                    Word16 ->
                    T_Tags ->
                    T_SwfFile
sem_SwfFile_File compressed_ version_ length_ size_ rate_ count_ tags_ =
    (\ _lhsIabc
       _lhsIname ->
         (case (({-# LINE 42 "src\\TrfInjectAbc.ag" #-}
                 Tag_Abc [] (pack $ encode _lhsIname) _lhsIabc
                 {-# LINE 4872 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _tagsOtag | _tagsOtag `seq` (True) ->
          (case (({-# LINE 37 "src\\TrfInjectAbc.ag" #-}
                  _lhsIname
                  {-# LINE 4877 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _tagsOname | _tagsOname `seq` (True) ->
           (case (({-# LINE 37 "src\\TrfInjectAbc.ag" #-}
                   _lhsIabc
                   {-# LINE 4882 "src/TrfInjectAbc.hs" #-}
                   )) of
            { _tagsOabc | _tagsOabc `seq` (True) ->
            (case (tags_ _tagsOabc _tagsOname _tagsOtag) of
             { ( _tagsIisLast,_tagsIoutput) | True ->
                 (case (size_) of
                  { ( _sizeIoutput) | True ->
                      (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                              SwfFile_File compressed_ version_ length_ _sizeIoutput rate_ count_ _tagsIoutput
                              {-# LINE 4891 "src/TrfInjectAbc.hs" #-}
                              )) of
                       { _output | _output `seq` (True) ->
                       (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                               _output
                               {-# LINE 4896 "src/TrfInjectAbc.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
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
type T_Tag = Bool ->
             ( Bool,Bool,Tag)
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    (\ _lhsIisLast ->
         (case (({-# LINE 55 "src\\TrfInjectAbc.ag" #-}
                 True
                 {-# LINE 4923 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _lhsOisAbc | _lhsOisAbc `seq` (True) ->
          (case (({-# LINE 51 "src\\TrfInjectAbc.ag" #-}
                  False
                  {-# LINE 4928 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOisLast | _lhsOisLast `seq` (True) ->
           (case (file_) of
            { ( _fileIoutput) | True ->
                (case (flags_) of
                 { ( _flagsIoutput) | True ->
                     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                             Tag_Abc _flagsIoutput name_ _fileIoutput
                             {-# LINE 4937 "src/TrfInjectAbc.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                              _output
                              {-# LINE 4942 "src/TrfInjectAbc.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOisAbc,_lhsOisLast,_lhsOoutput) }) }) }) }) }) }))
sem_Tag_FileAttributes :: Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          T_Tag
sem_Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_ =
    (\ _lhsIisLast ->
         (case (({-# LINE 54 "src\\TrfInjectAbc.ag" #-}
                 False
                 {-# LINE 4956 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _lhsOisAbc | _lhsOisAbc `seq` (True) ->
          (case (({-# LINE 39 "src\\TrfInjectAbc.ag" #-}
                  _lhsIisLast
                  {-# LINE 4961 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOisLast | _lhsOisLast `seq` (True) ->
           (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                   Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_
                   {-# LINE 4966 "src/TrfInjectAbc.hs" #-}
                   )) of
            { _output | _output `seq` (True) ->
            (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                    _output
                    {-# LINE 4971 "src/TrfInjectAbc.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOisAbc,_lhsOisLast,_lhsOoutput) }) }) }) }))
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    (\ _lhsIisLast ->
         (case (({-# LINE 54 "src\\TrfInjectAbc.ag" #-}
                 False
                 {-# LINE 4983 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _lhsOisAbc | _lhsOisAbc `seq` (True) ->
          (case (({-# LINE 39 "src\\TrfInjectAbc.ag" #-}
                  _lhsIisLast
                  {-# LINE 4988 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOisLast | _lhsOisLast `seq` (True) ->
           (case (kind_) of
            { ( _kindIoutput) | True ->
                (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                        Tag_Opaque _kindIoutput length_ body_
                        {-# LINE 4995 "src/TrfInjectAbc.hs" #-}
                        )) of
                 { _output | _output `seq` (True) ->
                 (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                         _output
                         {-# LINE 5000 "src/TrfInjectAbc.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOisAbc,_lhsOisLast,_lhsOoutput) }) }) }) }) }))
sem_Tag_End :: T_Tag
sem_Tag_End =
    (\ _lhsIisLast ->
         (case (({-# LINE 54 "src\\TrfInjectAbc.ag" #-}
                 False
                 {-# LINE 5009 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _lhsOisAbc | _lhsOisAbc `seq` (True) ->
          (case (({-# LINE 39 "src\\TrfInjectAbc.ag" #-}
                  _lhsIisLast
                  {-# LINE 5014 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOisLast | _lhsOisLast `seq` (True) ->
           (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                   Tag_End
                   {-# LINE 5019 "src/TrfInjectAbc.hs" #-}
                   )) of
            { _output | _output `seq` (True) ->
            (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                    _output
                    {-# LINE 5024 "src/TrfInjectAbc.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOisAbc,_lhsOisLast,_lhsOoutput) }) }) }) }))
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
type T_TagKind = ( TagKind)
sem_TagKind_End :: T_TagKind
sem_TagKind_End =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_End
            {-# LINE 5168 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5173 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ShowFrame :: T_TagKind
sem_TagKind_ShowFrame =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_ShowFrame
            {-# LINE 5181 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5186 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape :: T_TagKind
sem_TagKind_DefineShape =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineShape
            {-# LINE 5194 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5199 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject :: T_TagKind
sem_TagKind_PlaceObject =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_PlaceObject
            {-# LINE 5207 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5212 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_RemoveObject :: T_TagKind
sem_TagKind_RemoveObject =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_RemoveObject
            {-# LINE 5220 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5225 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBits :: T_TagKind
sem_TagKind_DefineBits =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBits
            {-# LINE 5233 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5238 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButton :: T_TagKind
sem_TagKind_DefineButton =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineButton
            {-# LINE 5246 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5251 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_JPEGTables :: T_TagKind
sem_TagKind_JPEGTables =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_JPEGTables
            {-# LINE 5259 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5264 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SetBackgroundColor :: T_TagKind
sem_TagKind_SetBackgroundColor =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SetBackgroundColor
            {-# LINE 5272 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5277 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont :: T_TagKind
sem_TagKind_DefineFont =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFont
            {-# LINE 5285 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5290 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineText :: T_TagKind
sem_TagKind_DefineText =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineText
            {-# LINE 5298 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5303 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoAction :: T_TagKind
sem_TagKind_DoAction =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DoAction
            {-# LINE 5311 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5316 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontInfo :: T_TagKind
sem_TagKind_DefineFontInfo =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFontInfo
            {-# LINE 5324 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5329 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSound :: T_TagKind
sem_TagKind_DefineSound =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineSound
            {-# LINE 5337 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5342 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_StartSound :: T_TagKind
sem_TagKind_StartSound =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_StartSound
            {-# LINE 5350 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5355 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButtonSound :: T_TagKind
sem_TagKind_DefineButtonSound =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineButtonSound
            {-# LINE 5363 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5368 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamHead :: T_TagKind
sem_TagKind_SoundStreamHead =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SoundStreamHead
            {-# LINE 5376 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5381 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamBlock :: T_TagKind
sem_TagKind_SoundStreamBlock =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SoundStreamBlock
            {-# LINE 5389 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5394 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsLossless :: T_TagKind
sem_TagKind_DefineBitsLossless =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBitsLossless
            {-# LINE 5402 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5407 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG2 :: T_TagKind
sem_TagKind_DefineBitsJPEG2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBitsJPEG2
            {-# LINE 5415 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5420 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape2 :: T_TagKind
sem_TagKind_DefineShape2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineShape2
            {-# LINE 5428 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5433 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButtonCxform :: T_TagKind
sem_TagKind_DefineButtonCxform =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineButtonCxform
            {-# LINE 5441 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5446 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Protect :: T_TagKind
sem_TagKind_Protect =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_Protect
            {-# LINE 5454 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5459 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject2 :: T_TagKind
sem_TagKind_PlaceObject2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_PlaceObject2
            {-# LINE 5467 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5472 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_RemoveObject2 :: T_TagKind
sem_TagKind_RemoveObject2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_RemoveObject2
            {-# LINE 5480 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5485 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape3 :: T_TagKind
sem_TagKind_DefineShape3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineShape3
            {-# LINE 5493 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5498 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineText2 :: T_TagKind
sem_TagKind_DefineText2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineText2
            {-# LINE 5506 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5511 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButton2 :: T_TagKind
sem_TagKind_DefineButton2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineButton2
            {-# LINE 5519 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5524 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG3 :: T_TagKind
sem_TagKind_DefineBitsJPEG3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBitsJPEG3
            {-# LINE 5532 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5537 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsLossless2 :: T_TagKind
sem_TagKind_DefineBitsLossless2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBitsLossless2
            {-# LINE 5545 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5550 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineEditText :: T_TagKind
sem_TagKind_DefineEditText =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineEditText
            {-# LINE 5558 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5563 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSprite :: T_TagKind
sem_TagKind_DefineSprite =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineSprite
            {-# LINE 5571 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5576 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_FrameLabel :: T_TagKind
sem_TagKind_FrameLabel =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_FrameLabel
            {-# LINE 5584 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5589 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamHead2 :: T_TagKind
sem_TagKind_SoundStreamHead2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SoundStreamHead2
            {-# LINE 5597 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5602 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineMorphShape :: T_TagKind
sem_TagKind_DefineMorphShape =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineMorphShape
            {-# LINE 5610 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5615 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont2 :: T_TagKind
sem_TagKind_DefineFont2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFont2
            {-# LINE 5623 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5628 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ExportAssets :: T_TagKind
sem_TagKind_ExportAssets =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_ExportAssets
            {-# LINE 5636 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5641 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ImportAssets :: T_TagKind
sem_TagKind_ImportAssets =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_ImportAssets
            {-# LINE 5649 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5654 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_EnableDebugger :: T_TagKind
sem_TagKind_EnableDebugger =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_EnableDebugger
            {-# LINE 5662 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5667 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoInitAction :: T_TagKind
sem_TagKind_DoInitAction =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DoInitAction
            {-# LINE 5675 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5680 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineVideoStream :: T_TagKind
sem_TagKind_DefineVideoStream =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineVideoStream
            {-# LINE 5688 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5693 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_VideoFrame :: T_TagKind
sem_TagKind_VideoFrame =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_VideoFrame
            {-# LINE 5701 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5706 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontInfo2 :: T_TagKind
sem_TagKind_DefineFontInfo2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFontInfo2
            {-# LINE 5714 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5719 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_EnableDebugger2 :: T_TagKind
sem_TagKind_EnableDebugger2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_EnableDebugger2
            {-# LINE 5727 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5732 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ScriptLimits :: T_TagKind
sem_TagKind_ScriptLimits =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_ScriptLimits
            {-# LINE 5740 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5745 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SetTabIndex :: T_TagKind
sem_TagKind_SetTabIndex =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SetTabIndex
            {-# LINE 5753 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5758 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_FileAttributes :: T_TagKind
sem_TagKind_FileAttributes =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_FileAttributes
            {-# LINE 5766 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5771 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject3 :: T_TagKind
sem_TagKind_PlaceObject3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_PlaceObject3
            {-# LINE 5779 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5784 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ImportAssets2 :: T_TagKind
sem_TagKind_ImportAssets2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_ImportAssets2
            {-# LINE 5792 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5797 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontAlignZones :: T_TagKind
sem_TagKind_DefineFontAlignZones =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFontAlignZones
            {-# LINE 5805 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5810 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_CSMTextSettings :: T_TagKind
sem_TagKind_CSMTextSettings =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_CSMTextSettings
            {-# LINE 5818 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5823 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont3 :: T_TagKind
sem_TagKind_DefineFont3 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFont3
            {-# LINE 5831 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5836 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SymbolClass :: T_TagKind
sem_TagKind_SymbolClass =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_SymbolClass
            {-# LINE 5844 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5849 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Metadata :: T_TagKind
sem_TagKind_Metadata =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_Metadata
            {-# LINE 5857 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5862 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineScalingGrid :: T_TagKind
sem_TagKind_DefineScalingGrid =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineScalingGrid
            {-# LINE 5870 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5875 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoABC :: T_TagKind
sem_TagKind_DoABC =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DoABC
            {-# LINE 5883 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5888 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape4 :: T_TagKind
sem_TagKind_DefineShape4 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineShape4
            {-# LINE 5896 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5901 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineMorphShape2 :: T_TagKind
sem_TagKind_DefineMorphShape2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineMorphShape2
            {-# LINE 5909 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5914 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSceneAndFrameLabelData :: T_TagKind
sem_TagKind_DefineSceneAndFrameLabelData =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineSceneAndFrameLabelData
            {-# LINE 5922 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5927 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBinaryData :: T_TagKind
sem_TagKind_DefineBinaryData =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBinaryData
            {-# LINE 5935 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5940 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontName :: T_TagKind
sem_TagKind_DefineFontName =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFontName
            {-# LINE 5948 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5953 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_StartSound2 :: T_TagKind
sem_TagKind_StartSound2 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_StartSound2
            {-# LINE 5961 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5966 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG4 :: T_TagKind
sem_TagKind_DefineBitsJPEG4 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineBitsJPEG4
            {-# LINE 5974 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5979 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont4 :: T_TagKind
sem_TagKind_DefineFont4 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_DefineFont4
            {-# LINE 5987 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 5992 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Other :: Word16 ->
                     T_TagKind
sem_TagKind_Other code_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TagKind_Other code_
            {-# LINE 6001 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6006 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Tags --------------------------------------------------------
-- cata
sem_Tags :: Tags ->
            T_Tags
sem_Tags list =
    (Prelude.foldr sem_Tags_Cons sem_Tags_Nil (Prelude.map sem_Tag list))
-- semantic domain
type T_Tags = AbcFile ->
              String ->
              Tag ->
              ( Bool,Tags)
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    (\ _lhsIabc
       _lhsIname
       _lhsItag ->
         (case (({-# LINE 38 "src\\TrfInjectAbc.ag" #-}
                 _lhsItag
                 {-# LINE 6030 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _tlOtag | _tlOtag `seq` (True) ->
          (case (({-# LINE 37 "src\\TrfInjectAbc.ag" #-}
                  _lhsIname
                  {-# LINE 6035 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _tlOname | _tlOname `seq` (True) ->
           (case (({-# LINE 37 "src\\TrfInjectAbc.ag" #-}
                   _lhsIabc
                   {-# LINE 6040 "src/TrfInjectAbc.hs" #-}
                   )) of
            { _tlOabc | _tlOabc `seq` (True) ->
            (case (tl_ _tlOabc _tlOname _tlOtag) of
             { ( _tlIisLast,_tlIoutput) | True ->
                 (case (({-# LINE 47 "src\\TrfInjectAbc.ag" #-}
                         _tlIisLast
                         {-# LINE 6047 "src/TrfInjectAbc.hs" #-}
                         )) of
                  { _hdOisLast | _hdOisLast `seq` (True) ->
                  (case (hd_ _hdOisLast) of
                   { ( _hdIisAbc,_hdIisLast,_hdIoutput) | True ->
                       (case (({-# LINE 48 "src\\TrfInjectAbc.ag" #-}
                               _hdIisLast
                               {-# LINE 6054 "src/TrfInjectAbc.hs" #-}
                               )) of
                        { _lhsOisLast | _lhsOisLast `seq` (True) ->
                        (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                                (:) _hdIoutput _tlIoutput
                                {-# LINE 6059 "src/TrfInjectAbc.hs" #-}
                                )) of
                         { _output | _output `seq` (True) ->
                         (case (({-# LINE 58 "src\\TrfInjectAbc.ag" #-}
                                 if _hdIisAbc && _tlIisLast
                                 then _hdIoutput : _lhsItag : _tlIoutput
                                 else _output
                                 {-# LINE 6066 "src/TrfInjectAbc.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOisLast,_lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    (\ _lhsIabc
       _lhsIname
       _lhsItag ->
         (case (({-# LINE 46 "src\\TrfInjectAbc.ag" #-}
                 True
                 {-# LINE 6077 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _lhsOisLast | _lhsOisLast `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  []
                  {-# LINE 6082 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _output | _output `seq` (True) ->
           (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                   _output
                   {-# LINE 6087 "src/TrfInjectAbc.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOisLast,_lhsOoutput) }) }) }))
-- Trait -------------------------------------------------------
-- cata
sem_Trait :: Trait ->
             T_Trait
sem_Trait (Trait_Trait _name _data _attrs _meta) =
    (sem_Trait_Trait _name (sem_TraitData _data) (sem_TraitAttrs _attrs) (sem_TraitMeta _meta))
-- semantic domain
type T_Trait = ( Trait)
sem_Trait_Trait :: Word32 ->
                   T_TraitData ->
                   T_TraitAttrs ->
                   T_TraitMeta ->
                   T_Trait
sem_Trait_Trait name_ data_ attrs_ meta_ =
    (case (meta_) of
     { ( _metaIoutput) | True ->
         (case (attrs_) of
          { ( _attrsIoutput) | True ->
              (case (data_) of
               { ( _dataIoutput) | True ->
                   (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                           Trait_Trait name_ _dataIoutput _attrsIoutput _metaIoutput
                           {-# LINE 6113 "src/TrfInjectAbc.hs" #-}
                           )) of
                    { _output | _output `seq` (True) ->
                    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                            _output
                            {-# LINE 6118 "src/TrfInjectAbc.hs" #-}
                            )) of
                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                     ( _lhsOoutput) }) }) }) }) })
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
type T_TraitAttr = ( TraitAttr)
sem_TraitAttr_Final :: T_TraitAttr
sem_TraitAttr_Final =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitAttr_Final
            {-# LINE 6138 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6143 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitAttr_Override
            {-# LINE 6151 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6156 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitAttr_Metadata
            {-# LINE 6164 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6169 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- TraitAttrs --------------------------------------------------
-- cata
sem_TraitAttrs :: TraitAttrs ->
                  T_TraitAttrs
sem_TraitAttrs list =
    (Prelude.foldr sem_TraitAttrs_Cons sem_TraitAttrs_Nil (Prelude.map sem_TraitAttr list))
-- semantic domain
type T_TraitAttrs = ( TraitAttrs)
sem_TraitAttrs_Cons :: T_TraitAttr ->
                       T_TraitAttrs ->
                       T_TraitAttrs
sem_TraitAttrs_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 6191 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 6196 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 6204 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6209 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_TraitData = ( TraitData)
sem_TraitData_Slot :: Word32 ->
                      Word32 ->
                      Word32 ->
                      T_ValueKind ->
                      T_TraitData
sem_TraitData_Slot slotId_ tp_ vindex_ vkind_ =
    (case (vkind_) of
     { ( _vkindIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 TraitData_Slot slotId_ tp_ vindex_ _vkindIoutput
                 {-# LINE 6243 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 6248 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_TraitData_Const :: Word32 ->
                       Word32 ->
                       Word32 ->
                       T_ValueKind ->
                       T_TraitData
sem_TraitData_Const slotId_ tp_ vindex_ vkind_ =
    (case (vkind_) of
     { ( _vkindIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 TraitData_Const slotId_ tp_ vindex_ _vkindIoutput
                 {-# LINE 6262 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 6267 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitData_Method dispId_ method_
            {-# LINE 6277 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6282 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitData_Getter dispId_ method_
            {-# LINE 6292 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6297 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitData_Setter dispId_ method_
            {-# LINE 6307 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6312 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitData_Function dispId_ method_
            {-# LINE 6322 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6327 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitData_Class slotId_ class_
            {-# LINE 6337 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6342 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_TraitKind = ( TraitKind)
sem_TraitKind_Slot :: T_TraitKind
sem_TraitKind_Slot =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Slot
            {-# LINE 6370 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6375 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Method
            {-# LINE 6383 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6388 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Getter
            {-# LINE 6396 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6401 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Setter
            {-# LINE 6409 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6414 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Class
            {-# LINE 6422 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6427 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Function
            {-# LINE 6435 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6440 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            TraitKind_Const
            {-# LINE 6448 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6453 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- TraitMeta ---------------------------------------------------
-- cata
sem_TraitMeta :: TraitMeta ->
                 T_TraitMeta
sem_TraitMeta list =
    (Prelude.foldr sem_TraitMeta_Cons sem_TraitMeta_Nil list)
-- semantic domain
type T_TraitMeta = ( TraitMeta)
sem_TraitMeta_Cons :: Word32 ->
                      T_TraitMeta ->
                      T_TraitMeta
sem_TraitMeta_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 6473 "src/TrfInjectAbc.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                  _output
                  {-# LINE 6478 "src/TrfInjectAbc.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 6486 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6491 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
-- Traits ------------------------------------------------------
-- cata
sem_Traits :: Traits ->
              T_Traits
sem_Traits list =
    (Prelude.foldr sem_Traits_Cons sem_Traits_Nil (Prelude.map sem_Trait list))
-- semantic domain
type T_Traits = ( Traits)
sem_Traits_Cons :: T_Trait ->
                   T_Traits ->
                   T_Traits
sem_Traits_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 6513 "src/TrfInjectAbc.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
                       _output
                       {-# LINE 6518 "src/TrfInjectAbc.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            []
            {-# LINE 6526 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6531 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
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
type T_ValueKind = ( ValueKind)
sem_ValueKind_Int :: T_ValueKind
sem_ValueKind_Int =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Int
            {-# LINE 6575 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6580 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_UInt
            {-# LINE 6588 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6593 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Double
            {-# LINE 6601 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6606 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Utf8
            {-# LINE 6614 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6619 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_True
            {-# LINE 6627 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6632 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_False
            {-# LINE 6640 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6645 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Null
            {-# LINE 6653 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6658 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Undefined
            {-# LINE 6666 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6671 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Namespace
            {-# LINE 6679 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6684 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Package
            {-# LINE 6692 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6697 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Internal
            {-# LINE 6705 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6710 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Protected
            {-# LINE 6718 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6723 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Explicit
            {-# LINE 6731 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6736 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Static
            {-# LINE 6744 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6749 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
            ValueKind_Private
            {-# LINE 6757 "src/TrfInjectAbc.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 34 "src\\TrfInjectAbc.ag" #-}
             _output
             {-# LINE 6762 "src/TrfInjectAbc.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })