

-- UUAGC 0.9.52.1 (src/TrfInjectRefl.ag)
module TrfInjectRefl(injectReflSwf,injectReflAbc) where

{-# LINE 7 "src\\TrfInjectRefl.ag" #-}

import Data.ByteString.Lazy(ByteString,pack)
import ByteCode
import CFG
import Data.Word
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Codec.Binary.UTF8.String
import ProgInfo
{-# LINE 19 "src/TrfInjectRefl.hs" #-}
{-# LINE 24 "src\\TrfInjectRefl.ag" #-}

injectReflSwf :: [SymbolTables] -> SwfFile -> SwfFile
injectReflSwf tbls m = out where
  inh = Inh_SwfFile { tbls_Inh_SwfFile = tbls }
  sem = sem_SwfFile m
  syn = wrap_SwfFile sem inh
  out = output_Syn_SwfFile syn

injectReflAbc :: [SymbolTables] -> AbcFile -> AbcFile
injectReflAbc tbls m = out where
  inh = Inh_AbcFile { tbls_Inh_AbcFile = tbls }
  sem = sem_AbcFile m
  syn = wrap_AbcFile sem inh
  out = output_Syn_AbcFile syn
{-# LINE 35 "src/TrfInjectRefl.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = ([SymbolTables]) ->
                 ( AbcFile,([SymbolTables]))
data Inh_AbcFile = Inh_AbcFile {tbls_Inh_AbcFile :: !(([SymbolTables]))}
data Syn_AbcFile = Syn_AbcFile {output_Syn_AbcFile :: !(AbcFile),tbls_Syn_AbcFile :: !(([SymbolTables]))}
wrap_AbcFile :: T_AbcFile ->
                Inh_AbcFile ->
                Syn_AbcFile
wrap_AbcFile sem (Inh_AbcFile _lhsItbls) =
    (let ( _lhsOoutput,_lhsOtbls) | True = sem _lhsItbls
     in  (Syn_AbcFile _lhsOoutput _lhsOtbls))
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
    (\ _lhsItbls ->
         (case (({-# LINE 56 "src\\TrfInjectRefl.ag" #-}
                 head _lhsItbls
                 {-# LINE 67 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tbls | _tbls `seq` (True) ->
          (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                  _tbls
                  {-# LINE 72 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _bodiesOtbls | _bodiesOtbls `seq` (True) ->
           (case (bodies_ _bodiesOtbls) of
            { ( _bodiesIoutput) | True ->
                (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                        _tbls
                        {-# LINE 79 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _scriptsOtbls | _scriptsOtbls `seq` (True) ->
                 (case (scripts_ _scriptsOtbls) of
                  { ( _scriptsIoutput) | True ->
                      (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                              _tbls
                              {-# LINE 86 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _classesOtbls | _classesOtbls `seq` (True) ->
                       (case (classes_ _classesOtbls) of
                        { ( _classesIoutput) | True ->
                            (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                    _tbls
                                    {-# LINE 93 "src/TrfInjectRefl.hs" #-}
                                    )) of
                             { _instancesOtbls | _instancesOtbls `seq` (True) ->
                             (case (instances_ _instancesOtbls) of
                              { ( _instancesIoutput) | True ->
                                  (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                          _tbls
                                          {-# LINE 100 "src/TrfInjectRefl.hs" #-}
                                          )) of
                                   { _metadatasOtbls | _metadatasOtbls `seq` (True) ->
                                   (case (metadatas_ _metadatasOtbls) of
                                    { ( _metadatasIoutput) | True ->
                                        (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                                _tbls
                                                {-# LINE 107 "src/TrfInjectRefl.hs" #-}
                                                )) of
                                         { _methodsOtbls | _methodsOtbls `seq` (True) ->
                                         (case (methods_ _methodsOtbls) of
                                          { ( _methodsIoutput) | True ->
                                              (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                                      _tbls
                                                      {-# LINE 114 "src/TrfInjectRefl.hs" #-}
                                                      )) of
                                               { _constantPoolOtbls | _constantPoolOtbls `seq` (True) ->
                                               (case (constantPool_ _constantPoolOtbls) of
                                                { ( _constantPoolIoutput) | True ->
                                                    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                                            AbcFile_File minorVersion_ majorVersion_ _constantPoolIoutput _methodsIoutput _metadatasIoutput _instancesIoutput _classesIoutput _scriptsIoutput _bodiesIoutput
                                                            {-# LINE 121 "src/TrfInjectRefl.hs" #-}
                                                            )) of
                                                     { _output | _output `seq` (True) ->
                                                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                                             _output
                                                             {-# LINE 126 "src/TrfInjectRefl.hs" #-}
                                                             )) of
                                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                      (case (({-# LINE 57 "src\\TrfInjectRefl.ag" #-}
                                                              tail _lhsItbls
                                                              {-# LINE 131 "src/TrfInjectRefl.hs" #-}
                                                              )) of
                                                       { _lhsOtbls | _lhsOtbls `seq` (True) ->
                                                       ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
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
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            AbcFlag_LazyInit
            {-# LINE 147 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 152 "src/TrfInjectRefl.hs" #-}
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
              (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 174 "src/TrfInjectRefl.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       _output
                       {-# LINE 179 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_AbcFlags_Nil :: T_AbcFlags
sem_AbcFlags_Nil =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            []
            {-# LINE 187 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 192 "src/TrfInjectRefl.hs" #-}
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
type T_BodyInfo = SymbolTables ->
                  ( BodyInfo)
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
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 218 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (traits_ _traitsOtbls) of
           { ( _traitsIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 225 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _exceptionsOtbls | _exceptionsOtbls `seq` (True) ->
                (case (exceptions_ _exceptionsOtbls) of
                 { ( _exceptionsIoutput) | True ->
                     (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                             _lhsItbls
                             {-# LINE 232 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _instructionsOtbls | _instructionsOtbls `seq` (True) ->
                      (case (instructions_ _instructionsOtbls) of
                       { ( _instructionsIoutput) | True ->
                           (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                   BodyInfo_Info method_ maxStack_ localCount_ initScopeDepth_ maxScopeDepth_ _instructionsIoutput _exceptionsIoutput _traitsIoutput
                                   {-# LINE 239 "src/TrfInjectRefl.hs" #-}
                                   )) of
                            { _output | _output `seq` (True) ->
                            (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                    _output
                                    {-# LINE 244 "src/TrfInjectRefl.hs" #-}
                                    )) of
                             { _lhsOoutput | _lhsOoutput `seq` (True) ->
                             ( _lhsOoutput) }) }) }) }) }) }) }) }))
-- BodyInfos ---------------------------------------------------
-- cata
sem_BodyInfos :: BodyInfos ->
                 T_BodyInfos
sem_BodyInfos list =
    (Prelude.foldr sem_BodyInfos_Cons sem_BodyInfos_Nil (Prelude.map sem_BodyInfo list))
-- semantic domain
type T_BodyInfos = SymbolTables ->
                   ( BodyInfos)
sem_BodyInfos_Cons :: T_BodyInfo ->
                      T_BodyInfos ->
                      T_BodyInfos
sem_BodyInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 264 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 271 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 278 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 283 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_BodyInfos_Nil :: T_BodyInfos
sem_BodyInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 292 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 297 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- CaseOffsets -------------------------------------------------
-- cata
sem_CaseOffsets :: CaseOffsets ->
                   T_CaseOffsets
sem_CaseOffsets list =
    (Prelude.foldr sem_CaseOffsets_Cons sem_CaseOffsets_Nil list)
-- semantic domain
type T_CaseOffsets = SymbolTables ->
                     ( CaseOffsets)
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 317 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 324 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 329 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 338 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 343 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- ClassInfo ---------------------------------------------------
-- cata
sem_ClassInfo :: ClassInfo ->
                 T_ClassInfo
sem_ClassInfo (ClassInfo_Info _con _traits) =
    (sem_ClassInfo_Info _con (sem_Traits _traits))
-- semantic domain
type T_ClassInfo = SymbolTables ->
                   ( ClassInfo)
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 363 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (traits_ _traitsOtbls) of
           { ( _traitsIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       ClassInfo_Info con_ _traitsIoutput
                       {-# LINE 370 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 375 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = SymbolTables ->
                    ( ClassInfos)
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 395 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 402 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 409 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 414 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 423 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 428 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- DebugType ---------------------------------------------------
-- cata
sem_DebugType :: DebugType ->
                 T_DebugType
sem_DebugType (DebugType_Local) =
    (sem_DebugType_Local)
-- semantic domain
type T_DebugType = SymbolTables ->
                   ( DebugType)
sem_DebugType_Local :: T_DebugType
sem_DebugType_Local =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 DebugType_Local
                 {-# LINE 446 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 451 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Exception ---------------------------------------------------
-- cata
sem_Exception :: Exception ->
                 T_Exception
sem_Exception (Exception_Info _from _to _target _tp _name) =
    (sem_Exception_Info _from _to _target _tp _name)
-- semantic domain
type T_Exception = SymbolTables ->
                   ( Exception)
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Exception_Info from_ to_ target_ tp_ name_
                 {-# LINE 474 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 479 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = SymbolTables ->
                    ( Exceptions)
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 499 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 506 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 513 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 518 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 527 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 532 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_InstanceFlag = SymbolTables ->
                      ( InstanceFlag)
sem_InstanceFlag_ClassSealed :: T_InstanceFlag
sem_InstanceFlag_ClassSealed =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 InstanceFlag_ClassSealed
                 {-# LINE 556 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 561 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_InstanceFlag_ClassFinal :: T_InstanceFlag
sem_InstanceFlag_ClassFinal =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 InstanceFlag_ClassFinal
                 {-# LINE 570 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 575 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_InstanceFlag_ClassInterface :: T_InstanceFlag
sem_InstanceFlag_ClassInterface =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 InstanceFlag_ClassInterface
                 {-# LINE 584 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 589 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_InstanceFlag_ClassProtected :: T_InstanceFlag
sem_InstanceFlag_ClassProtected =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 InstanceFlag_ClassProtected
                 {-# LINE 598 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 603 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- InstanceFlags -----------------------------------------------
-- cata
sem_InstanceFlags :: InstanceFlags ->
                     T_InstanceFlags
sem_InstanceFlags list =
    (Prelude.foldr sem_InstanceFlags_Cons sem_InstanceFlags_Nil (Prelude.map sem_InstanceFlag list))
-- semantic domain
type T_InstanceFlags = SymbolTables ->
                       ( InstanceFlags)
sem_InstanceFlags_Cons :: T_InstanceFlag ->
                          T_InstanceFlags ->
                          T_InstanceFlags
sem_InstanceFlags_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 623 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 630 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 637 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 642 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_InstanceFlags_Nil :: T_InstanceFlags
sem_InstanceFlags_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 651 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 656 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- InstanceInfo ------------------------------------------------
-- cata
sem_InstanceInfo :: InstanceInfo ->
                    T_InstanceInfo
sem_InstanceInfo (InstanceInfo_Info _name _super _flags _protectedNs _interfaces _constructor _traits) =
    (sem_InstanceInfo_Info _name _super (sem_InstanceFlags _flags) _protectedNs (sem_Interfaces _interfaces) _constructor (sem_Traits _traits))
-- semantic domain
type T_InstanceInfo = SymbolTables ->
                      ( InstanceInfo)
sem_InstanceInfo_Info :: Word32 ->
                         Word32 ->
                         T_InstanceFlags ->
                         Word32 ->
                         T_Interfaces ->
                         Word32 ->
                         T_Traits ->
                         T_InstanceInfo
sem_InstanceInfo_Info name_ super_ flags_ protectedNs_ interfaces_ constructor_ traits_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 681 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (traits_ _traitsOtbls) of
           { ( _traitsIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 688 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _interfacesOtbls | _interfacesOtbls `seq` (True) ->
                (case (interfaces_ _interfacesOtbls) of
                 { ( _interfacesIoutput) | True ->
                     (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                             _lhsItbls
                             {-# LINE 695 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _flagsOtbls | _flagsOtbls `seq` (True) ->
                      (case (flags_ _flagsOtbls) of
                       { ( _flagsIoutput) | True ->
                           (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                   InstanceInfo_Info name_ super_ _flagsIoutput protectedNs_ _interfacesIoutput constructor_ _traitsIoutput
                                   {-# LINE 702 "src/TrfInjectRefl.hs" #-}
                                   )) of
                            { _output | _output `seq` (True) ->
                            (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                    _output
                                    {-# LINE 707 "src/TrfInjectRefl.hs" #-}
                                    )) of
                             { _lhsOoutput | _lhsOoutput `seq` (True) ->
                             ( _lhsOoutput) }) }) }) }) }) }) }) }))
-- InstanceInfos -----------------------------------------------
-- cata
sem_InstanceInfos :: InstanceInfos ->
                     T_InstanceInfos
sem_InstanceInfos list =
    (Prelude.foldr sem_InstanceInfos_Cons sem_InstanceInfos_Nil (Prelude.map sem_InstanceInfo list))
-- semantic domain
type T_InstanceInfos = SymbolTables ->
                       ( InstanceInfos)
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 727 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 734 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 741 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 746 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 755 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 760 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_Instruction = SymbolTables ->
                     ( Instruction)
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Add
                 {-# LINE 1128 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1133 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Add_i
                 {-# LINE 1142 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1147 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Add_d
                 {-# LINE 1156 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1161 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_ApplyType name_
                 {-# LINE 1171 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1176 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_AsType name_
                 {-# LINE 1186 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1191 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_AsTypeLate
                 {-# LINE 1200 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1205 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Breakpoint
                 {-# LINE 1214 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1219 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_BreakLine line_
                 {-# LINE 1229 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1234 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_BitAnd
                 {-# LINE 1243 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1248 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_BitNot
                 {-# LINE 1257 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1262 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_BitOr
                 {-# LINE 1271 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1276 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_BitXor
                 {-# LINE 1285 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1290 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Call argCount_
                 {-# LINE 1300 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1305 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallInterface name_ argCount_
                 {-# LINE 1316 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1321 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallMethod index_ argCount_
                 {-# LINE 1332 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1337 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallProp name_ argCount_
                 {-# LINE 1348 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1353 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallPropLex name_ argCount_
                 {-# LINE 1364 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1369 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallPropVoid name_ argCount_
                 {-# LINE 1380 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1385 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallStatic method_ argCount_
                 {-# LINE 1396 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1401 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallSuper name_ argCount_
                 {-# LINE 1412 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1417 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallSuperId
                 {-# LINE 1426 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1431 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CallSuperVoid name_ argCount_
                 {-# LINE 1442 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1447 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_CheckFilter
                 {-# LINE 1456 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1461 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce name_
                 {-# LINE 1471 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1476 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_a
                 {-# LINE 1485 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1490 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_b
                 {-# LINE 1499 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1504 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_d
                 {-# LINE 1513 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1518 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_i
                 {-# LINE 1527 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1532 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_o
                 {-# LINE 1541 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1546 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_s
                 {-# LINE 1555 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1560 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Coerce_u
                 {-# LINE 1569 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1574 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Concat
                 {-# LINE 1583 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1588 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Construct argCount_
                 {-# LINE 1598 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1603 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_ConstructProp name_ argCount_
                 {-# LINE 1614 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1619 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_ConstructSuper argCount_
                 {-# LINE 1629 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1634 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_b
                 {-# LINE 1643 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1648 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_i
                 {-# LINE 1657 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1662 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_d
                 {-# LINE 1671 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1676 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_o
                 {-# LINE 1685 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1690 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_u
                 {-# LINE 1699 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1704 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Convert_s
                 {-# LINE 1713 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1718 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 1731 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tpOtbls | _tpOtbls `seq` (True) ->
          (case (tp_ _tpOtbls) of
           { ( _tpIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       Instruction_Debug _tpIoutput name_ reg_ extra_
                       {-# LINE 1738 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 1743 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DebugFile name_
                 {-# LINE 1753 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1758 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DebugLine line_
                 {-# LINE 1768 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1773 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DecLocal reg_
                 {-# LINE 1783 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1788 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DecLocal_i reg_
                 {-# LINE 1798 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1803 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Decrement
                 {-# LINE 1812 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1817 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Decrement_i
                 {-# LINE 1826 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1831 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DeleteProperty name_
                 {-# LINE 1841 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1846 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DeletePropertyLate
                 {-# LINE 1855 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1860 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Divide
                 {-# LINE 1869 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1874 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Dup
                 {-# LINE 1883 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1888 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Dxns name_
                 {-# LINE 1898 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1903 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_DxnsLate
                 {-# LINE 1912 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1917 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Equals
                 {-# LINE 1926 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1931 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_EscXAttr
                 {-# LINE 1940 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1945 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_EscXElem
                 {-# LINE 1954 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1959 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_FindDef name_
                 {-# LINE 1969 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1974 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_FindPropertyGlobalStrict name_
                 {-# LINE 1984 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 1989 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_FindPropertyGlobal name_
                 {-# LINE 1999 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2004 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_FindProperty name_
                 {-# LINE 2014 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2019 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_FindPropStrict name_
                 {-# LINE 2029 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2034 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetDescendants name_
                 {-# LINE 2044 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2049 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetGlobalScope
                 {-# LINE 2058 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2063 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetGlobalSlot slot_
                 {-# LINE 2073 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2078 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLex name_
                 {-# LINE 2088 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2093 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLocal reg_
                 {-# LINE 2103 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2108 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLocal0
                 {-# LINE 2117 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2122 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLocal1
                 {-# LINE 2131 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2136 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLocal2
                 {-# LINE 2145 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2150 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetLocal3
                 {-# LINE 2159 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2164 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetOuterScope name_
                 {-# LINE 2174 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2179 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetProperty name_
                 {-# LINE 2189 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2194 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetScopeObject index_
                 {-# LINE 2204 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2209 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetSlot slot_
                 {-# LINE 2219 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2224 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GetSuper name_
                 {-# LINE 2234 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2239 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GreaterEquals
                 {-# LINE 2248 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2253 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_GreaterThan
                 {-# LINE 2262 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2267 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_HasNext
                 {-# LINE 2276 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2281 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_HasNext2 objectReg_ indexReg_
                 {-# LINE 2292 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2297 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfEq offset_
                 {-# LINE 2307 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2312 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfFalse offset_
                 {-# LINE 2322 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2327 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfGe offset_
                 {-# LINE 2337 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2342 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfGt offset_
                 {-# LINE 2352 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2357 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfLe offset_
                 {-# LINE 2367 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2372 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfLt offset_
                 {-# LINE 2382 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2387 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfNGe offset_
                 {-# LINE 2397 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2402 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfNGt offset_
                 {-# LINE 2412 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2417 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfNLe offset_
                 {-# LINE 2427 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2432 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfNLt offset_
                 {-# LINE 2442 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2447 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfNe offset_
                 {-# LINE 2457 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2462 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfStrictEq offset_
                 {-# LINE 2472 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2477 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfStrictNe offset_
                 {-# LINE 2487 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2492 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IfTrue offset_
                 {-# LINE 2502 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2507 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_In
                 {-# LINE 2516 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2521 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IncLocal reg_
                 {-# LINE 2531 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2536 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IncLocal_i reg_
                 {-# LINE 2546 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2551 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Increment
                 {-# LINE 2560 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2565 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Increment_i
                 {-# LINE 2574 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2579 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_InitProperty name_
                 {-# LINE 2589 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2594 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_InstanceOf
                 {-# LINE 2603 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2608 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IsType name_
                 {-# LINE 2618 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2623 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_IsTypeLate
                 {-# LINE 2632 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2637 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Jump offset_
                 {-# LINE 2647 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2652 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Kill reg_
                 {-# LINE 2662 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2667 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Label
                 {-# LINE 2676 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2681 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LessEquals
                 {-# LINE 2690 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2695 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LessThan
                 {-# LINE 2704 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2709 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LoadFloat32
                 {-# LINE 2718 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2723 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LoadFloat64
                 {-# LINE 2732 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2737 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LoadIndirect8
                 {-# LINE 2746 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2751 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LoadIndirect16
                 {-# LINE 2760 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2765 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_LoadIndirect32
                 {-# LINE 2774 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2779 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 2790 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _caseOffsetsOtbls | _caseOffsetsOtbls `seq` (True) ->
          (case (caseOffsets_ _caseOffsetsOtbls) of
           { ( _caseOffsetsIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       Instruction_LookupSwitch defaultOffset_ _caseOffsetsIoutput
                       {-# LINE 2797 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 2802 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Lshift
                 {-# LINE 2811 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2816 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Modulo
                 {-# LINE 2825 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2830 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Multiply
                 {-# LINE 2839 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2844 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Multiply_i
                 {-# LINE 2853 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2858 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Negate
                 {-# LINE 2867 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2872 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Negate_i
                 {-# LINE 2881 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2886 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewActivation
                 {-# LINE 2895 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2900 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewArray argCount_
                 {-# LINE 2910 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2915 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewCatch exception_
                 {-# LINE 2925 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2930 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewClass class_
                 {-# LINE 2940 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2945 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewFunction method_
                 {-# LINE 2955 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2960 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NewObject argCount_
                 {-# LINE 2970 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2975 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NextName
                 {-# LINE 2984 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 2989 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_NextValue
                 {-# LINE 2998 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3003 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Nop
                 {-# LINE 3012 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3017 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Not
                 {-# LINE 3026 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3031 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Pop
                 {-# LINE 3040 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3045 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PopScope
                 {-# LINE 3054 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3059 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushByte val_
                 {-# LINE 3069 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3074 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushDouble name_
                 {-# LINE 3084 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3089 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushFalse
                 {-# LINE 3098 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3103 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushInt name_
                 {-# LINE 3113 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3118 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushNamespace name_
                 {-# LINE 3128 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3133 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushNaN
                 {-# LINE 3142 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3147 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushNull
                 {-# LINE 3156 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3161 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushScope
                 {-# LINE 3170 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3175 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushShort val_
                 {-# LINE 3185 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3190 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushString name_
                 {-# LINE 3200 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3205 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushTrue
                 {-# LINE 3214 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3219 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushUInt name_
                 {-# LINE 3229 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3234 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushUndefined
                 {-# LINE 3243 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3248 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_PushWith
                 {-# LINE 3257 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3262 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_ReturnValue
                 {-# LINE 3271 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3276 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_ReturnVoid
                 {-# LINE 3285 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3290 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Rshift
                 {-# LINE 3299 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3304 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetLocal reg_
                 {-# LINE 3314 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3319 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetLocal0
                 {-# LINE 3328 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3333 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetLocal1
                 {-# LINE 3342 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3347 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetLocal2
                 {-# LINE 3356 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3361 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetLocal3
                 {-# LINE 3370 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3375 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetGlobalSlot slot_
                 {-# LINE 3385 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3390 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetProperty name_
                 {-# LINE 3400 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3405 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetPropertyLate
                 {-# LINE 3414 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3419 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetSlot slot_
                 {-# LINE 3429 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3434 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SetSuper name_
                 {-# LINE 3444 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3449 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SignExtend1
                 {-# LINE 3458 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3463 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SignExtend8
                 {-# LINE 3472 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3477 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_SignExtend16
                 {-# LINE 3486 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3491 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StoreFloat32
                 {-# LINE 3500 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3505 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StoreFloat64
                 {-# LINE 3514 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3519 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StoreIndirect32
                 {-# LINE 3528 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3533 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StoreIndirect16
                 {-# LINE 3542 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3547 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StoreIndirect8
                 {-# LINE 3556 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3561 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_StrictEquals
                 {-# LINE 3570 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3575 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Substract
                 {-# LINE 3584 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3589 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Substract_i
                 {-# LINE 3598 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3603 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Swap
                 {-# LINE 3612 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3617 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Throw
                 {-# LINE 3626 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3631 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Timestamp
                 {-# LINE 3640 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3645 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_TypeOf
                 {-# LINE 3654 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3659 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Urshift
                 {-# LINE 3668 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3673 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Instruction_Location index_
                 {-# LINE 3683 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3688 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = SymbolTables ->
                      ( Instructions)
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 3708 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 3715 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 3722 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 3727 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 3736 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3741 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Interfaces --------------------------------------------------
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil list)
-- semantic domain
type T_Interfaces = SymbolTables ->
                    ( Interfaces)
sem_Interfaces_Cons :: Word32 ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 3761 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 3768 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 3773 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 3782 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3787 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- MetaInfo ----------------------------------------------------
-- cata
sem_MetaInfo :: MetaInfo ->
                T_MetaInfo
sem_MetaInfo (MetaInfo_Info _name _items) =
    (sem_MetaInfo_Info _name (sem_MetaItems _items))
-- semantic domain
type T_MetaInfo = SymbolTables ->
                  ( MetaInfo)
sem_MetaInfo_Info :: Word32 ->
                     T_MetaItems ->
                     T_MetaInfo
sem_MetaInfo_Info name_ items_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 3807 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _itemsOtbls | _itemsOtbls `seq` (True) ->
          (case (items_ _itemsOtbls) of
           { ( _itemsIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       MetaInfo_Info name_ _itemsIoutput
                       {-# LINE 3814 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 3819 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- MetaInfos ---------------------------------------------------
-- cata
sem_MetaInfos :: MetaInfos ->
                 T_MetaInfos
sem_MetaInfos list =
    (Prelude.foldr sem_MetaInfos_Cons sem_MetaInfos_Nil (Prelude.map sem_MetaInfo list))
-- semantic domain
type T_MetaInfos = SymbolTables ->
                   ( MetaInfos)
sem_MetaInfos_Cons :: T_MetaInfo ->
                      T_MetaInfos ->
                      T_MetaInfos
sem_MetaInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 3839 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 3846 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 3853 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 3858 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_MetaInfos_Nil :: T_MetaInfos
sem_MetaInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 3867 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3872 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- MetaItem ----------------------------------------------------
-- cata
sem_MetaItem :: MetaItem ->
                T_MetaItem
sem_MetaItem (MetaItem_Item _key _value) =
    (sem_MetaItem_Item _key _value)
-- semantic domain
type T_MetaItem = SymbolTables ->
                  ( MetaItem)
sem_MetaItem_Item :: Word32 ->
                     Word32 ->
                     T_MetaItem
sem_MetaItem_Item key_ value_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MetaItem_Item key_ value_
                 {-# LINE 3892 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3897 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- MetaItems ---------------------------------------------------
-- cata
sem_MetaItems :: MetaItems ->
                 T_MetaItems
sem_MetaItems list =
    (Prelude.foldr sem_MetaItems_Cons sem_MetaItems_Nil (Prelude.map sem_MetaItem list))
-- semantic domain
type T_MetaItems = SymbolTables ->
                   ( MetaItems)
sem_MetaItems_Cons :: T_MetaItem ->
                      T_MetaItems ->
                      T_MetaItems
sem_MetaItems_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 3917 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 3924 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 3931 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 3936 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_MetaItems_Nil :: T_MetaItems
sem_MetaItems_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 3945 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3950 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_MethodFlag = SymbolTables ->
                    ( MethodFlag)
sem_MethodFlag_NeedArgs :: T_MethodFlag
sem_MethodFlag_NeedArgs =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_NeedArgs
                 {-# LINE 3978 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3983 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MethodFlag_NeedAct :: T_MethodFlag
sem_MethodFlag_NeedAct =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_NeedAct
                 {-# LINE 3992 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 3997 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MethodFlag_NeedRest :: T_MethodFlag
sem_MethodFlag_NeedRest =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_NeedRest
                 {-# LINE 4006 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4011 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MethodFlag_HasOptionals :: T_MethodFlag
sem_MethodFlag_HasOptionals =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_HasOptionals
                 {-# LINE 4020 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4025 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MethodFlag_SetDXNS :: T_MethodFlag
sem_MethodFlag_SetDXNS =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_SetDXNS
                 {-# LINE 4034 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4039 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MethodFlag_HasParamNames :: T_MethodFlag
sem_MethodFlag_HasParamNames =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MethodFlag_HasParamNames
                 {-# LINE 4048 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4053 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- MethodFlags -------------------------------------------------
-- cata
sem_MethodFlags :: MethodFlags ->
                   T_MethodFlags
sem_MethodFlags list =
    (Prelude.foldr sem_MethodFlags_Cons sem_MethodFlags_Nil (Prelude.map sem_MethodFlag list))
-- semantic domain
type T_MethodFlags = SymbolTables ->
                     ( MethodFlags)
sem_MethodFlags_Cons :: T_MethodFlag ->
                        T_MethodFlags ->
                        T_MethodFlags
sem_MethodFlags_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4073 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4080 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 4087 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 4092 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_MethodFlags_Nil :: T_MethodFlags
sem_MethodFlags_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4101 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4106 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- MethodInfo --------------------------------------------------
-- cata
sem_MethodInfo :: MethodInfo ->
                  T_MethodInfo
sem_MethodInfo (MethodInfo_Info _return _params _name _flags _options _names) =
    (sem_MethodInfo_Info _return (sem_ParamTypes _params) _name (sem_MethodFlags _flags) (sem_Optionals _options) (sem_ParamNames _names))
-- semantic domain
type T_MethodInfo = SymbolTables ->
                    ( MethodInfo)
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4130 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _namesOtbls | _namesOtbls `seq` (True) ->
          (case (names_ _namesOtbls) of
           { ( _namesIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4137 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _optionsOtbls | _optionsOtbls `seq` (True) ->
                (case (options_ _optionsOtbls) of
                 { ( _optionsIoutput) | True ->
                     (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                             _lhsItbls
                             {-# LINE 4144 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _flagsOtbls | _flagsOtbls `seq` (True) ->
                      (case (flags_ _flagsOtbls) of
                       { ( _flagsIoutput) | True ->
                           (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                   _lhsItbls
                                   {-# LINE 4151 "src/TrfInjectRefl.hs" #-}
                                   )) of
                            { _paramsOtbls | _paramsOtbls `seq` (True) ->
                            (case (params_ _paramsOtbls) of
                             { ( _paramsIoutput) | True ->
                                 (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                         MethodInfo_Info return_ _paramsIoutput name_ _flagsIoutput _optionsIoutput _namesIoutput
                                         {-# LINE 4158 "src/TrfInjectRefl.hs" #-}
                                         )) of
                                  { _output | _output `seq` (True) ->
                                  (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                          _output
                                          {-# LINE 4163 "src/TrfInjectRefl.hs" #-}
                                          )) of
                                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                   ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }))
-- MethodInfos -------------------------------------------------
-- cata
sem_MethodInfos :: MethodInfos ->
                   T_MethodInfos
sem_MethodInfos list =
    (Prelude.foldr sem_MethodInfos_Cons sem_MethodInfos_Nil (Prelude.map sem_MethodInfo list))
-- semantic domain
type T_MethodInfos = SymbolTables ->
                     ( MethodInfos)
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4183 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4190 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 4197 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 4202 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4211 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4216 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_MultinameInfo = SymbolTables ->
                       ( MultinameInfo)
sem_MultinameInfo_QName :: Word32 ->
                           Word32 ->
                           T_MultinameInfo
sem_MultinameInfo_QName namespace_ name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_QName namespace_ name_
                 {-# LINE 4256 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4261 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_QNameA :: Word32 ->
                            Word32 ->
                            T_MultinameInfo
sem_MultinameInfo_QNameA namespace_ name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_QNameA namespace_ name_
                 {-# LINE 4272 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4277 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_RTQName :: Word32 ->
                             T_MultinameInfo
sem_MultinameInfo_RTQName name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_RTQName name_
                 {-# LINE 4287 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4292 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_RTQNameA :: Word32 ->
                              T_MultinameInfo
sem_MultinameInfo_RTQNameA name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_RTQNameA name_
                 {-# LINE 4302 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4307 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_RTQNameL :: T_MultinameInfo
sem_MultinameInfo_RTQNameL =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_RTQNameL
                 {-# LINE 4316 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4321 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_RTQNameLA :: T_MultinameInfo
sem_MultinameInfo_RTQNameLA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_RTQNameLA
                 {-# LINE 4330 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4335 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_Multiname :: Word32 ->
                               Word32 ->
                               T_MultinameInfo
sem_MultinameInfo_Multiname name_ set_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_Multiname name_ set_
                 {-# LINE 4346 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4351 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_MultinameA :: Word32 ->
                                Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameA name_ set_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_MultinameA name_ set_
                 {-# LINE 4362 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4367 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_MultinameL :: Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameL set_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_MultinameL set_
                 {-# LINE 4377 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4382 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_MultinameLA :: Word32 ->
                                 T_MultinameInfo
sem_MultinameInfo_MultinameLA set_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameInfo_MultinameLA set_
                 {-# LINE 4392 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4397 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameInfo_Generic :: Word32 ->
                             T_ParamNames ->
                             T_MultinameInfo
sem_MultinameInfo_Generic name_ params_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4408 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _paramsOtbls | _paramsOtbls `seq` (True) ->
          (case (params_ _paramsOtbls) of
           { ( _paramsIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       MultinameInfo_Generic name_ _paramsIoutput
                       {-# LINE 4415 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 4420 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- MultinameInfos ----------------------------------------------
-- cata
sem_MultinameInfos :: MultinameInfos ->
                      T_MultinameInfos
sem_MultinameInfos list =
    (Prelude.foldr sem_MultinameInfos_Cons sem_MultinameInfos_Nil (Prelude.map sem_MultinameInfo list))
-- semantic domain
type T_MultinameInfos = SymbolTables ->
                        ( MultinameInfos)
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4440 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4447 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 4454 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 4459 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4468 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4473 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_MultinameKind = SymbolTables ->
                       ( MultinameKind)
sem_MultinameKind_QName :: T_MultinameKind
sem_MultinameKind_QName =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_QName
                 {-# LINE 4511 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4516 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_QNameA :: T_MultinameKind
sem_MultinameKind_QNameA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_QNameA
                 {-# LINE 4525 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4530 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_RTQName :: T_MultinameKind
sem_MultinameKind_RTQName =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_RTQName
                 {-# LINE 4539 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4544 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_RTQNameA :: T_MultinameKind
sem_MultinameKind_RTQNameA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_RTQNameA
                 {-# LINE 4553 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4558 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_RTQNameL :: T_MultinameKind
sem_MultinameKind_RTQNameL =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_RTQNameL
                 {-# LINE 4567 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4572 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_RTQNameLA :: T_MultinameKind
sem_MultinameKind_RTQNameLA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_RTQNameLA
                 {-# LINE 4581 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4586 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_Multiname :: T_MultinameKind
sem_MultinameKind_Multiname =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_Multiname
                 {-# LINE 4595 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4600 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_MultinameA :: T_MultinameKind
sem_MultinameKind_MultinameA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_MultinameA
                 {-# LINE 4609 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4614 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_MultinameL :: T_MultinameKind
sem_MultinameKind_MultinameL =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_MultinameL
                 {-# LINE 4623 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4628 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_MultinameLA :: T_MultinameKind
sem_MultinameKind_MultinameLA =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_MultinameLA
                 {-# LINE 4637 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4642 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_MultinameKind_Generic :: T_MultinameKind
sem_MultinameKind_Generic =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 MultinameKind_Generic
                 {-# LINE 4651 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4656 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- NamespaceInfo -----------------------------------------------
-- cata
sem_NamespaceInfo :: NamespaceInfo ->
                     T_NamespaceInfo
sem_NamespaceInfo (NamespaceInfo_Info _kind _name) =
    (sem_NamespaceInfo_Info (sem_NamespaceKind _kind) _name)
-- semantic domain
type T_NamespaceInfo = SymbolTables ->
                       ( NamespaceInfo)
sem_NamespaceInfo_Info :: T_NamespaceKind ->
                          Word32 ->
                          T_NamespaceInfo
sem_NamespaceInfo_Info kind_ name_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4676 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _kindOtbls | _kindOtbls `seq` (True) ->
          (case (kind_ _kindOtbls) of
           { ( _kindIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       NamespaceInfo_Info _kindIoutput name_
                       {-# LINE 4683 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 4688 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- NamespaceInfos ----------------------------------------------
-- cata
sem_NamespaceInfos :: NamespaceInfos ->
                      T_NamespaceInfos
sem_NamespaceInfos list =
    (Prelude.foldr sem_NamespaceInfos_Cons sem_NamespaceInfos_Nil (Prelude.map sem_NamespaceInfo list))
-- semantic domain
type T_NamespaceInfos = SymbolTables ->
                        ( NamespaceInfos)
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4708 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4715 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 4722 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 4727 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4736 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4741 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_NamespaceKind = SymbolTables ->
                       ( NamespaceKind)
sem_NamespaceKind_General :: T_NamespaceKind
sem_NamespaceKind_General =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_General
                 {-# LINE 4771 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4776 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Package :: T_NamespaceKind
sem_NamespaceKind_Package =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Package
                 {-# LINE 4785 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4790 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Internal :: T_NamespaceKind
sem_NamespaceKind_Internal =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Internal
                 {-# LINE 4799 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4804 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Protected :: T_NamespaceKind
sem_NamespaceKind_Protected =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Protected
                 {-# LINE 4813 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4818 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Explicit :: T_NamespaceKind
sem_NamespaceKind_Explicit =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Explicit
                 {-# LINE 4827 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4832 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Static :: T_NamespaceKind
sem_NamespaceKind_Static =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Static
                 {-# LINE 4841 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4846 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_NamespaceKind_Private :: T_NamespaceKind
sem_NamespaceKind_Private =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 NamespaceKind_Private
                 {-# LINE 4855 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4860 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- NamespaceNames ----------------------------------------------
-- cata
sem_NamespaceNames :: NamespaceNames ->
                      T_NamespaceNames
sem_NamespaceNames list =
    (Prelude.foldr sem_NamespaceNames_Cons sem_NamespaceNames_Nil list)
-- semantic domain
type T_NamespaceNames = SymbolTables ->
                        ( NamespaceNames)
sem_NamespaceNames_Cons :: Word32 ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4880 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 4887 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 4892 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4901 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4906 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Optional ----------------------------------------------------
-- cata
sem_Optional :: Optional ->
                T_Optional
sem_Optional (Optional_Detail _val _kind) =
    (sem_Optional_Detail _val (sem_ValueKind _kind))
-- semantic domain
type T_Optional = SymbolTables ->
                  ( Optional)
sem_Optional_Detail :: Word32 ->
                       T_ValueKind ->
                       T_Optional
sem_Optional_Detail val_ kind_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4926 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _kindOtbls | _kindOtbls `seq` (True) ->
          (case (kind_ _kindOtbls) of
           { ( _kindIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       Optional_Detail val_ _kindIoutput
                       {-# LINE 4933 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 4938 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- Optionals ---------------------------------------------------
-- cata
sem_Optionals :: Optionals ->
                 T_Optionals
sem_Optionals list =
    (Prelude.foldr sem_Optionals_Cons sem_Optionals_Nil (Prelude.map sem_Optional list))
-- semantic domain
type T_Optionals = SymbolTables ->
                   ( Optionals)
sem_Optionals_Cons :: T_Optional ->
                      T_Optionals ->
                      T_Optionals
sem_Optionals_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 4958 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 4965 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 4972 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 4977 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_Optionals_Nil :: T_Optionals
sem_Optionals_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 4986 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 4991 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- ParamNames --------------------------------------------------
-- cata
sem_ParamNames :: ParamNames ->
                  T_ParamNames
sem_ParamNames list =
    (Prelude.foldr sem_ParamNames_Cons sem_ParamNames_Nil list)
-- semantic domain
type T_ParamNames = SymbolTables ->
                    ( ParamNames)
sem_ParamNames_Cons :: Word32 ->
                       T_ParamNames ->
                       T_ParamNames
sem_ParamNames_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5011 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5018 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5023 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_ParamNames_Nil :: T_ParamNames
sem_ParamNames_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5032 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5037 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- ParamTypes --------------------------------------------------
-- cata
sem_ParamTypes :: ParamTypes ->
                  T_ParamTypes
sem_ParamTypes list =
    (Prelude.foldr sem_ParamTypes_Cons sem_ParamTypes_Nil list)
-- semantic domain
type T_ParamTypes = SymbolTables ->
                    ( ParamTypes)
sem_ParamTypes_Cons :: Word32 ->
                       T_ParamTypes ->
                       T_ParamTypes
sem_ParamTypes_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5057 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5064 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5069 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5078 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5083 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = SymbolTables ->
                     ( PoolDoubles)
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5103 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5110 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5115 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5124 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5129 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = SymbolTables ->
                  ( PoolInfo)
sem_PoolInfo_Info :: T_PoolInts ->
                     T_PoolUInts ->
                     T_PoolDoubles ->
                     T_PoolStrings ->
                     T_NamespaceInfos ->
                     T_SetInfos ->
                     T_MultinameInfos ->
                     T_PoolInfo
sem_PoolInfo_Info integers_ uintegers_ doubles_ strings_ namespaces_ namesets_ multinames_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5154 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _multinamesOtbls | _multinamesOtbls `seq` (True) ->
          (case (multinames_ _multinamesOtbls) of
           { ( _multinamesIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 5161 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _namesetsOtbls | _namesetsOtbls `seq` (True) ->
                (case (namesets_ _namesetsOtbls) of
                 { ( _namesetsIoutput) | True ->
                     (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                             _lhsItbls
                             {-# LINE 5168 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _namespacesOtbls | _namespacesOtbls `seq` (True) ->
                      (case (namespaces_ _namespacesOtbls) of
                       { ( _namespacesIoutput) | True ->
                           (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                   _lhsItbls
                                   {-# LINE 5175 "src/TrfInjectRefl.hs" #-}
                                   )) of
                            { _stringsOtbls | _stringsOtbls `seq` (True) ->
                            (case (strings_ _stringsOtbls) of
                             { ( _stringsIoutput) | True ->
                                 (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                         _lhsItbls
                                         {-# LINE 5182 "src/TrfInjectRefl.hs" #-}
                                         )) of
                                  { _doublesOtbls | _doublesOtbls `seq` (True) ->
                                  (case (doubles_ _doublesOtbls) of
                                   { ( _doublesIoutput) | True ->
                                       (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                               _lhsItbls
                                               {-# LINE 5189 "src/TrfInjectRefl.hs" #-}
                                               )) of
                                        { _uintegersOtbls | _uintegersOtbls `seq` (True) ->
                                        (case (uintegers_ _uintegersOtbls) of
                                         { ( _uintegersIoutput) | True ->
                                             (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                                                     _lhsItbls
                                                     {-# LINE 5196 "src/TrfInjectRefl.hs" #-}
                                                     )) of
                                              { _integersOtbls | _integersOtbls `seq` (True) ->
                                              (case (integers_ _integersOtbls) of
                                               { ( _integersIoutput) | True ->
                                                   (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                                           PoolInfo_Info _integersIoutput _uintegersIoutput _doublesIoutput _stringsIoutput _namespacesIoutput _namesetsIoutput _multinamesIoutput
                                                           {-# LINE 5203 "src/TrfInjectRefl.hs" #-}
                                                           )) of
                                                    { _output | _output `seq` (True) ->
                                                    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                                            _output
                                                            {-# LINE 5208 "src/TrfInjectRefl.hs" #-}
                                                            )) of
                                                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                     ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = SymbolTables ->
                  ( PoolInts)
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5228 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5235 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5240 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5249 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5254 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = SymbolTables ->
                     ( PoolStrings)
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5274 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5281 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5286 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5295 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5300 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = SymbolTables ->
                   ( PoolUInts)
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5320 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 5327 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5332 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5341 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5346 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_
            {-# LINE 5367 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5372 "src/TrfInjectRefl.hs" #-}
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
type T_ScriptInfo = SymbolTables ->
                    ( ScriptInfo)
sem_ScriptInfo_Info :: Word32 ->
                       T_Traits ->
                       T_ScriptInfo
sem_ScriptInfo_Info method_ traits_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5392 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (traits_ _traitsOtbls) of
           { ( _traitsIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       ScriptInfo_Info method_ _traitsIoutput
                       {-# LINE 5399 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5404 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- ScriptInfos -------------------------------------------------
-- cata
sem_ScriptInfos :: ScriptInfos ->
                   T_ScriptInfos
sem_ScriptInfos list =
    (Prelude.foldr sem_ScriptInfos_Cons sem_ScriptInfos_Nil (Prelude.map sem_ScriptInfo list))
-- semantic domain
type T_ScriptInfos = SymbolTables ->
                     ( ScriptInfos)
sem_ScriptInfos_Cons :: T_ScriptInfo ->
                        T_ScriptInfos ->
                        T_ScriptInfos
sem_ScriptInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5424 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 5431 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 5438 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 5443 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_ScriptInfos_Nil :: T_ScriptInfos
sem_ScriptInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5452 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5457 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- SetInfo -----------------------------------------------------
-- cata
sem_SetInfo :: SetInfo ->
               T_SetInfo
sem_SetInfo (SetInfo_Info _names) =
    (sem_SetInfo_Info (sem_NamespaceNames _names))
-- semantic domain
type T_SetInfo = SymbolTables ->
                 ( SetInfo)
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5476 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _namesOtbls | _namesOtbls `seq` (True) ->
          (case (names_ _namesOtbls) of
           { ( _namesIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       SetInfo_Info _namesIoutput
                       {-# LINE 5483 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 5488 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
-- SetInfos ----------------------------------------------------
-- cata
sem_SetInfos :: SetInfos ->
                T_SetInfos
sem_SetInfos list =
    (Prelude.foldr sem_SetInfos_Cons sem_SetInfos_Nil (Prelude.map sem_SetInfo list))
-- semantic domain
type T_SetInfos = SymbolTables ->
                  ( SetInfos)
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5508 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 5515 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 5522 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 5527 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 5536 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5541 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = ([SymbolTables]) ->
                 ( SwfFile,([SymbolTables]))
data Inh_SwfFile = Inh_SwfFile {tbls_Inh_SwfFile :: !(([SymbolTables]))}
data Syn_SwfFile = Syn_SwfFile {output_Syn_SwfFile :: !(SwfFile),tbls_Syn_SwfFile :: !(([SymbolTables]))}
wrap_SwfFile :: T_SwfFile ->
                Inh_SwfFile ->
                Syn_SwfFile
wrap_SwfFile sem (Inh_SwfFile _lhsItbls) =
    (let ( _lhsOoutput,_lhsOtbls) | True = sem _lhsItbls
     in  (Syn_SwfFile _lhsOoutput _lhsOtbls))
sem_SwfFile_File :: Bool ->
                    Word8 ->
                    Word32 ->
                    T_Rect ->
                    Word16 ->
                    Word16 ->
                    T_Tags ->
                    T_SwfFile
sem_SwfFile_File compressed_ version_ length_ size_ rate_ count_ tags_ =
    (\ _lhsItbls ->
         (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5574 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tagsOtbls | _tagsOtbls `seq` (True) ->
          (case (tags_ _tagsOtbls) of
           { ( _tagsIoutput,_tagsItbls) | True ->
               (case (size_) of
                { ( _sizeIoutput) | True ->
                    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                            SwfFile_File compressed_ version_ length_ _sizeIoutput rate_ count_ _tagsIoutput
                            {-# LINE 5583 "src/TrfInjectRefl.hs" #-}
                            )) of
                     { _output | _output `seq` (True) ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             _output
                             {-# LINE 5588 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                              _tagsItbls
                              {-# LINE 5593 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOtbls | _lhsOtbls `seq` (True) ->
                       ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }))
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
type T_Tag = ([SymbolTables]) ->
             ( Tag,([SymbolTables]))
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    (\ _lhsItbls ->
         (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 5620 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _fileOtbls | _fileOtbls `seq` (True) ->
          (case (file_ _fileOtbls) of
           { ( _fileIoutput,_fileItbls) | True ->
               (case (flags_) of
                { ( _flagsIoutput) | True ->
                    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                            Tag_Abc _flagsIoutput name_ _fileIoutput
                            {-# LINE 5629 "src/TrfInjectRefl.hs" #-}
                            )) of
                     { _output | _output `seq` (True) ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             _output
                             {-# LINE 5634 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                              _fileItbls
                              {-# LINE 5639 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOtbls | _lhsOtbls `seq` (True) ->
                       ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }))
sem_Tag_FileAttributes :: Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          T_Tag
sem_Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_
                 {-# LINE 5653 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5658 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                   _lhsItbls
                   {-# LINE 5663 "src/TrfInjectRefl.hs" #-}
                   )) of
            { _lhsOtbls | _lhsOtbls `seq` (True) ->
            ( _lhsOoutput,_lhsOtbls) }) }) }))
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    (\ _lhsItbls ->
         (case (kind_) of
          { ( _kindIoutput) | True ->
              (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                      Tag_Opaque _kindIoutput length_ body_
                      {-# LINE 5677 "src/TrfInjectRefl.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       _output
                       {-# LINE 5682 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                        _lhsItbls
                        {-# LINE 5687 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOtbls | _lhsOtbls `seq` (True) ->
                 ( _lhsOoutput,_lhsOtbls) }) }) }) }))
sem_Tag_End :: T_Tag
sem_Tag_End =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 Tag_End
                 {-# LINE 5696 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 5701 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                   _lhsItbls
                   {-# LINE 5706 "src/TrfInjectRefl.hs" #-}
                   )) of
            { _lhsOtbls | _lhsOtbls `seq` (True) ->
            ( _lhsOoutput,_lhsOtbls) }) }) }))
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
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_End
            {-# LINE 5850 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5855 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ShowFrame :: T_TagKind
sem_TagKind_ShowFrame =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_ShowFrame
            {-# LINE 5863 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5868 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape :: T_TagKind
sem_TagKind_DefineShape =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineShape
            {-# LINE 5876 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5881 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject :: T_TagKind
sem_TagKind_PlaceObject =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_PlaceObject
            {-# LINE 5889 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5894 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_RemoveObject :: T_TagKind
sem_TagKind_RemoveObject =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_RemoveObject
            {-# LINE 5902 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5907 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBits :: T_TagKind
sem_TagKind_DefineBits =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBits
            {-# LINE 5915 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5920 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButton :: T_TagKind
sem_TagKind_DefineButton =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineButton
            {-# LINE 5928 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5933 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_JPEGTables :: T_TagKind
sem_TagKind_JPEGTables =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_JPEGTables
            {-# LINE 5941 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5946 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SetBackgroundColor :: T_TagKind
sem_TagKind_SetBackgroundColor =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SetBackgroundColor
            {-# LINE 5954 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5959 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont :: T_TagKind
sem_TagKind_DefineFont =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFont
            {-# LINE 5967 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5972 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineText :: T_TagKind
sem_TagKind_DefineText =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineText
            {-# LINE 5980 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5985 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoAction :: T_TagKind
sem_TagKind_DoAction =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DoAction
            {-# LINE 5993 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 5998 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontInfo :: T_TagKind
sem_TagKind_DefineFontInfo =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFontInfo
            {-# LINE 6006 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6011 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSound :: T_TagKind
sem_TagKind_DefineSound =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineSound
            {-# LINE 6019 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6024 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_StartSound :: T_TagKind
sem_TagKind_StartSound =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_StartSound
            {-# LINE 6032 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6037 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButtonSound :: T_TagKind
sem_TagKind_DefineButtonSound =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineButtonSound
            {-# LINE 6045 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6050 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamHead :: T_TagKind
sem_TagKind_SoundStreamHead =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SoundStreamHead
            {-# LINE 6058 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6063 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamBlock :: T_TagKind
sem_TagKind_SoundStreamBlock =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SoundStreamBlock
            {-# LINE 6071 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6076 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsLossless :: T_TagKind
sem_TagKind_DefineBitsLossless =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBitsLossless
            {-# LINE 6084 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6089 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG2 :: T_TagKind
sem_TagKind_DefineBitsJPEG2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBitsJPEG2
            {-# LINE 6097 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6102 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape2 :: T_TagKind
sem_TagKind_DefineShape2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineShape2
            {-# LINE 6110 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6115 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButtonCxform :: T_TagKind
sem_TagKind_DefineButtonCxform =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineButtonCxform
            {-# LINE 6123 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6128 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Protect :: T_TagKind
sem_TagKind_Protect =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_Protect
            {-# LINE 6136 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6141 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject2 :: T_TagKind
sem_TagKind_PlaceObject2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_PlaceObject2
            {-# LINE 6149 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6154 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_RemoveObject2 :: T_TagKind
sem_TagKind_RemoveObject2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_RemoveObject2
            {-# LINE 6162 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6167 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape3 :: T_TagKind
sem_TagKind_DefineShape3 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineShape3
            {-# LINE 6175 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6180 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineText2 :: T_TagKind
sem_TagKind_DefineText2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineText2
            {-# LINE 6188 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6193 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineButton2 :: T_TagKind
sem_TagKind_DefineButton2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineButton2
            {-# LINE 6201 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6206 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG3 :: T_TagKind
sem_TagKind_DefineBitsJPEG3 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBitsJPEG3
            {-# LINE 6214 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6219 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsLossless2 :: T_TagKind
sem_TagKind_DefineBitsLossless2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBitsLossless2
            {-# LINE 6227 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6232 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineEditText :: T_TagKind
sem_TagKind_DefineEditText =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineEditText
            {-# LINE 6240 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6245 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSprite :: T_TagKind
sem_TagKind_DefineSprite =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineSprite
            {-# LINE 6253 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6258 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_FrameLabel :: T_TagKind
sem_TagKind_FrameLabel =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_FrameLabel
            {-# LINE 6266 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6271 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SoundStreamHead2 :: T_TagKind
sem_TagKind_SoundStreamHead2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SoundStreamHead2
            {-# LINE 6279 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6284 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineMorphShape :: T_TagKind
sem_TagKind_DefineMorphShape =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineMorphShape
            {-# LINE 6292 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6297 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont2 :: T_TagKind
sem_TagKind_DefineFont2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFont2
            {-# LINE 6305 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6310 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ExportAssets :: T_TagKind
sem_TagKind_ExportAssets =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_ExportAssets
            {-# LINE 6318 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6323 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ImportAssets :: T_TagKind
sem_TagKind_ImportAssets =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_ImportAssets
            {-# LINE 6331 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6336 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_EnableDebugger :: T_TagKind
sem_TagKind_EnableDebugger =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_EnableDebugger
            {-# LINE 6344 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6349 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoInitAction :: T_TagKind
sem_TagKind_DoInitAction =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DoInitAction
            {-# LINE 6357 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6362 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineVideoStream :: T_TagKind
sem_TagKind_DefineVideoStream =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineVideoStream
            {-# LINE 6370 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6375 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_VideoFrame :: T_TagKind
sem_TagKind_VideoFrame =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_VideoFrame
            {-# LINE 6383 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6388 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontInfo2 :: T_TagKind
sem_TagKind_DefineFontInfo2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFontInfo2
            {-# LINE 6396 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6401 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_EnableDebugger2 :: T_TagKind
sem_TagKind_EnableDebugger2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_EnableDebugger2
            {-# LINE 6409 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6414 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ScriptLimits :: T_TagKind
sem_TagKind_ScriptLimits =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_ScriptLimits
            {-# LINE 6422 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6427 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SetTabIndex :: T_TagKind
sem_TagKind_SetTabIndex =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SetTabIndex
            {-# LINE 6435 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6440 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_FileAttributes :: T_TagKind
sem_TagKind_FileAttributes =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_FileAttributes
            {-# LINE 6448 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6453 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_PlaceObject3 :: T_TagKind
sem_TagKind_PlaceObject3 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_PlaceObject3
            {-# LINE 6461 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6466 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_ImportAssets2 :: T_TagKind
sem_TagKind_ImportAssets2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_ImportAssets2
            {-# LINE 6474 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6479 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontAlignZones :: T_TagKind
sem_TagKind_DefineFontAlignZones =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFontAlignZones
            {-# LINE 6487 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6492 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_CSMTextSettings :: T_TagKind
sem_TagKind_CSMTextSettings =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_CSMTextSettings
            {-# LINE 6500 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6505 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont3 :: T_TagKind
sem_TagKind_DefineFont3 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFont3
            {-# LINE 6513 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6518 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_SymbolClass :: T_TagKind
sem_TagKind_SymbolClass =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_SymbolClass
            {-# LINE 6526 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6531 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Metadata :: T_TagKind
sem_TagKind_Metadata =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_Metadata
            {-# LINE 6539 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6544 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineScalingGrid :: T_TagKind
sem_TagKind_DefineScalingGrid =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineScalingGrid
            {-# LINE 6552 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6557 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DoABC :: T_TagKind
sem_TagKind_DoABC =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DoABC
            {-# LINE 6565 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6570 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineShape4 :: T_TagKind
sem_TagKind_DefineShape4 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineShape4
            {-# LINE 6578 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6583 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineMorphShape2 :: T_TagKind
sem_TagKind_DefineMorphShape2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineMorphShape2
            {-# LINE 6591 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6596 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineSceneAndFrameLabelData :: T_TagKind
sem_TagKind_DefineSceneAndFrameLabelData =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineSceneAndFrameLabelData
            {-# LINE 6604 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6609 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBinaryData :: T_TagKind
sem_TagKind_DefineBinaryData =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBinaryData
            {-# LINE 6617 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6622 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFontName :: T_TagKind
sem_TagKind_DefineFontName =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFontName
            {-# LINE 6630 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6635 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_StartSound2 :: T_TagKind
sem_TagKind_StartSound2 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_StartSound2
            {-# LINE 6643 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6648 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineBitsJPEG4 :: T_TagKind
sem_TagKind_DefineBitsJPEG4 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineBitsJPEG4
            {-# LINE 6656 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6661 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_DefineFont4 :: T_TagKind
sem_TagKind_DefineFont4 =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_DefineFont4
            {-# LINE 6669 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6674 "src/TrfInjectRefl.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TagKind_Other :: Word16 ->
                     T_TagKind
sem_TagKind_Other code_ =
    (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
            TagKind_Other code_
            {-# LINE 6683 "src/TrfInjectRefl.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
             _output
             {-# LINE 6688 "src/TrfInjectRefl.hs" #-}
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
type T_Tags = ([SymbolTables]) ->
              ( Tags,([SymbolTables]))
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 6708 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _hdOtbls | _hdOtbls `seq` (True) ->
          (case (hd_ _hdOtbls) of
           { ( _hdIoutput,_hdItbls) | True ->
               (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                       _hdItbls
                       {-# LINE 6715 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _tlOtbls | _tlOtbls `seq` (True) ->
                (case (tl_ _tlOtbls) of
                 { ( _tlIoutput,_tlItbls) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 6722 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 6727 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                               _tlItbls
                               {-# LINE 6732 "src/TrfInjectRefl.hs" #-}
                               )) of
                        { _lhsOtbls | _lhsOtbls `seq` (True) ->
                        ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }) }))
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 6741 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6746 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           (case (({-# LINE 52 "src\\TrfInjectRefl.ag" #-}
                   _lhsItbls
                   {-# LINE 6751 "src/TrfInjectRefl.hs" #-}
                   )) of
            { _lhsOtbls | _lhsOtbls `seq` (True) ->
            ( _lhsOoutput,_lhsOtbls) }) }) }))
-- Trait -------------------------------------------------------
-- cata
sem_Trait :: Trait ->
             T_Trait
sem_Trait (Trait_Trait _name _data _attrs _meta) =
    (sem_Trait_Trait _name (sem_TraitData _data) (sem_TraitAttrs _attrs) (sem_TraitMeta _meta))
-- semantic domain
type T_Trait = SymbolTables ->
               ( Trait)
sem_Trait_Trait :: Word32 ->
                   T_TraitData ->
                   T_TraitAttrs ->
                   T_TraitMeta ->
                   T_Trait
sem_Trait_Trait name_ data_ attrs_ meta_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 6773 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _metaOtbls | _metaOtbls `seq` (True) ->
          (case (meta_ _metaOtbls) of
           { ( _metaIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 6780 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _attrsOtbls | _attrsOtbls `seq` (True) ->
                (case (attrs_ _attrsOtbls) of
                 { ( _attrsIoutput) | True ->
                     (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                             _lhsItbls
                             {-# LINE 6787 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _dataOtbls | _dataOtbls `seq` (True) ->
                      (case (data_ _dataOtbls) of
                       { ( _dataIoutput) | True ->
                           (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                   Trait_Trait name_ _dataIoutput _attrsIoutput _metaIoutput
                                   {-# LINE 6794 "src/TrfInjectRefl.hs" #-}
                                   )) of
                            { _output | _output `seq` (True) ->
                            (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                                    _output
                                    {-# LINE 6799 "src/TrfInjectRefl.hs" #-}
                                    )) of
                             { _lhsOoutput | _lhsOoutput `seq` (True) ->
                             ( _lhsOoutput) }) }) }) }) }) }) }) }))
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
type T_TraitAttr = SymbolTables ->
                   ( TraitAttr)
sem_TraitAttr_Final :: T_TraitAttr
sem_TraitAttr_Final =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitAttr_Final
                 {-# LINE 6821 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6826 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitAttr_Override
                 {-# LINE 6835 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6840 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitAttr_Metadata
                 {-# LINE 6849 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6854 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- TraitAttrs --------------------------------------------------
-- cata
sem_TraitAttrs :: TraitAttrs ->
                  T_TraitAttrs
sem_TraitAttrs list =
    (Prelude.foldr sem_TraitAttrs_Cons sem_TraitAttrs_Nil (Prelude.map sem_TraitAttr list))
-- semantic domain
type T_TraitAttrs = SymbolTables ->
                    ( TraitAttrs)
sem_TraitAttrs_Cons :: T_TraitAttr ->
                       T_TraitAttrs ->
                       T_TraitAttrs
sem_TraitAttrs_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 6874 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 6881 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 6888 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 6893 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 6902 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6907 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_TraitData = SymbolTables ->
                   ( TraitData)
sem_TraitData_Slot :: Word32 ->
                      Word32 ->
                      Word32 ->
                      T_ValueKind ->
                      T_TraitData
sem_TraitData_Slot slotId_ tp_ vindex_ vkind_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 6941 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _vkindOtbls | _vkindOtbls `seq` (True) ->
          (case (vkind_ _vkindOtbls) of
           { ( _vkindIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       TraitData_Slot slotId_ tp_ vindex_ _vkindIoutput
                       {-# LINE 6948 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 6953 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_TraitData_Const :: Word32 ->
                       Word32 ->
                       Word32 ->
                       T_ValueKind ->
                       T_TraitData
sem_TraitData_Const slotId_ tp_ vindex_ vkind_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 6966 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _vkindOtbls | _vkindOtbls `seq` (True) ->
          (case (vkind_ _vkindOtbls) of
           { ( _vkindIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       TraitData_Const slotId_ tp_ vindex_ _vkindIoutput
                       {-# LINE 6973 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 6978 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitData_Method dispId_ method_
                 {-# LINE 6989 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 6994 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitData_Getter dispId_ method_
                 {-# LINE 7005 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7010 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitData_Setter dispId_ method_
                 {-# LINE 7021 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7026 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitData_Function dispId_ method_
                 {-# LINE 7037 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7042 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitData_Class slotId_ class_
                 {-# LINE 7053 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7058 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_TraitKind = SymbolTables ->
                   ( TraitKind)
sem_TraitKind_Slot :: T_TraitKind
sem_TraitKind_Slot =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Slot
                 {-# LINE 7088 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7093 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Method
                 {-# LINE 7102 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7107 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Getter
                 {-# LINE 7116 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7121 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Setter
                 {-# LINE 7130 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7135 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Class
                 {-# LINE 7144 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7149 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Function
                 {-# LINE 7158 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7163 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 TraitKind_Const
                 {-# LINE 7172 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7177 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- TraitMeta ---------------------------------------------------
-- cata
sem_TraitMeta :: TraitMeta ->
                 T_TraitMeta
sem_TraitMeta list =
    (Prelude.foldr sem_TraitMeta_Cons sem_TraitMeta_Nil list)
-- semantic domain
type T_TraitMeta = SymbolTables ->
                   ( TraitMeta)
sem_TraitMeta_Cons :: Word32 ->
                      T_TraitMeta ->
                      T_TraitMeta
sem_TraitMeta_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 7197 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                       (:) hd_ _tlIoutput
                       {-# LINE 7204 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                        _output
                        {-# LINE 7209 "src/TrfInjectRefl.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 7218 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7223 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
-- Traits ------------------------------------------------------
-- cata
sem_Traits :: Traits ->
              T_Traits
sem_Traits list =
    (Prelude.foldr sem_Traits_Cons sem_Traits_Nil (Prelude.map sem_Trait list))
-- semantic domain
type T_Traits = SymbolTables ->
                ( Traits)
sem_Traits_Cons :: T_Trait ->
                   T_Traits ->
                   T_Traits
sem_Traits_Cons hd_ tl_ =
    (\ _lhsItbls ->
         (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                 _lhsItbls
                 {-# LINE 7243 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (tl_ _tlOtbls) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 53 "src\\TrfInjectRefl.ag" #-}
                       _lhsItbls
                       {-# LINE 7250 "src/TrfInjectRefl.hs" #-}
                       )) of
                { _hdOtbls | _hdOtbls `seq` (True) ->
                (case (hd_ _hdOtbls) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                             (:) _hdIoutput _tlIoutput
                             {-# LINE 7257 "src/TrfInjectRefl.hs" #-}
                             )) of
                      { _output | _output `seq` (True) ->
                      (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                              _output
                              {-# LINE 7262 "src/TrfInjectRefl.hs" #-}
                              )) of
                       { _lhsOoutput | _lhsOoutput `seq` (True) ->
                       ( _lhsOoutput) }) }) }) }) }) }))
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 []
                 {-# LINE 7271 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7276 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
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
type T_ValueKind = SymbolTables ->
                   ( ValueKind)
sem_ValueKind_Int :: T_ValueKind
sem_ValueKind_Int =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Int
                 {-# LINE 7322 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7327 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_UInt
                 {-# LINE 7336 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7341 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Double
                 {-# LINE 7350 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7355 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Utf8
                 {-# LINE 7364 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7369 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_True
                 {-# LINE 7378 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7383 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_False
                 {-# LINE 7392 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7397 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Null
                 {-# LINE 7406 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7411 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Undefined
                 {-# LINE 7420 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7425 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Namespace
                 {-# LINE 7434 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7439 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Package
                 {-# LINE 7448 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7453 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Internal
                 {-# LINE 7462 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7467 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Protected
                 {-# LINE 7476 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7481 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Explicit
                 {-# LINE 7490 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7495 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Static
                 {-# LINE 7504 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7509 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    (\ _lhsItbls ->
         (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                 ValueKind_Private
                 {-# LINE 7518 "src/TrfInjectRefl.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 45 "src\\TrfInjectRefl.ag" #-}
                  _output
                  {-# LINE 7523 "src/TrfInjectRefl.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }))