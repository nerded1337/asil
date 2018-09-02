

-- UUAGC 0.9.52.1 (src/InstrLocFilter.ag)
module InstrLocFilter(instrLocFilter) where

{-# LINE 11 "src\\InstrLocFilter.ag" #-}

import Data.ByteString.Lazy(ByteString,pack)
import ByteCode
import Data.Word
import Data.Bits
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.Monoid
{-# LINE 16 "src/InstrLocFilter.hs" #-}
{-# LINE 23 "src\\InstrLocFilter.ag" #-}

-- | Computes the minimum length of an instruction (in bytes)
instrLocFilter :: BodyInfo -> BodyInfo
instrLocFilter body = body' where
  inh   = Inh_BodyInfo {}
  sem   = sem_BodyInfo body
  syn   = wrap_BodyInfo sem inh
  body' = output_Syn_BodyInfo syn
{-# LINE 26 "src/InstrLocFilter.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = ( )
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
    ( )
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
type T_BodyInfo = ( BodyInfo)
data Inh_BodyInfo = Inh_BodyInfo {}
data Syn_BodyInfo = Syn_BodyInfo {output_Syn_BodyInfo :: !(BodyInfo)}
wrap_BodyInfo :: T_BodyInfo ->
                 Inh_BodyInfo ->
                 Syn_BodyInfo
wrap_BodyInfo sem (Inh_BodyInfo) =
    (let ( _lhsOoutput) | True = sem
     in  (Syn_BodyInfo _lhsOoutput))
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
    (case (({-# LINE 38 "src\\InstrLocFilter.ag" #-}
            True
            {-# LINE 102 "src/InstrLocFilter.hs" #-}
            )) of
     { _instructionsOisBranch | _instructionsOisBranch `seq` (True) ->
     (case (({-# LINE 2 "src\\ByteCodeLocationInfo.ag" #-}
             0
             {-# LINE 107 "src/InstrLocFilter.hs" #-}
             )) of
      { _instructionsOlocation | _instructionsOlocation `seq` (True) ->
      (case (instructions_ _instructionsOisBranch _instructionsOlocation) of
       { ( _instructionsIisBranch,_instructionsIlocation,instructions_1) | True ->
           (case (({-# LINE 39 "src\\InstrLocFilter.ag" #-}
                   False
                   {-# LINE 114 "src/InstrLocFilter.hs" #-}
                   )) of
            { _instructionsOrevIsBranch | _instructionsOrevIsBranch `seq` (True) ->
            (case (({-# LINE 3 "src\\ByteCodeLocationInfo.ag" #-}
                    0
                    {-# LINE 119 "src/InstrLocFilter.hs" #-}
                    )) of
             { _instructionsOrevLocation | _instructionsOrevLocation `seq` (True) ->
             (case (exceptions_) of
              { ( _exceptionsIlocs,_exceptionsIoutput) | True ->
                  (case (instructions_1 _instructionsOrevIsBranch _instructionsOrevLocation) of
                   { ( _instructionsIlocs,_instructionsIrevIsBranch,_instructionsIrevLocation,instructions_2) | True ->
                       (case (({-# LINE 47 "src\\InstrLocFilter.ag" #-}
                               _instructionsIlocs `mappend` _exceptionsIlocs
                               {-# LINE 128 "src/InstrLocFilter.hs" #-}
                               )) of
                        { _instructionsOretain | _instructionsOretain `seq` (True) ->
                        (case (traits_) of
                         { ( _traitsIoutput) | True ->
                             (case (instructions_2 _instructionsOretain) of
                              { ( _instructionsIoutput) | True ->
                                  (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                          BodyInfo_Info method_ maxStack_ localCount_ initScopeDepth_ maxScopeDepth_ _instructionsIoutput _exceptionsIoutput _traitsIoutput
                                          {-# LINE 137 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _output | _output `seq` (True) ->
                                   (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                           _output
                                           {-# LINE 142 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })
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
type T_CaseOffsets = Int ->
                     ( Int,T_CaseOffsets_1)
type T_CaseOffsets_1 = Int ->
                       ( IntSet,CaseOffsets,Int)
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 180 "src/InstrLocFilter.hs" #-}
                 )) of
          { _tlOlocation | _tlOlocation `seq` (True) ->
          (case (tl_ _tlOlocation) of
           { ( _tlIlocation,tl_1) | True ->
               (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                       _tlIlocation
                       {-# LINE 187 "src/InstrLocFilter.hs" #-}
                       )) of
                { _lhsOlocation | _lhsOlocation `seq` (True) ->
                (case ((let sem_CaseOffsets_Cons_1 :: T_CaseOffsets_1
                            sem_CaseOffsets_Cons_1 =
                                (\ _lhsIrevLocation ->
                                     (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                             _lhsIrevLocation
                                             {-# LINE 195 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _tlOrevLocation | _tlOrevLocation `seq` (True) ->
                                      (case (tl_1 _tlOrevLocation) of
                                       { ( _tlIlocs,_tlIoutput,_tlIrevLocation) | True ->
                                           (case (({-# LINE 88 "src\\InstrLocFilter.ag" #-}
                                                   _tlIlocs
                                                   {-# LINE 202 "src/InstrLocFilter.hs" #-}
                                                   )) of
                                            { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                            (case (({-# LINE 30 "src\\ByteCodeLocationInfo.ag" #-}
                                                    fromS24 hd_
                                                    {-# LINE 207 "src/InstrLocFilter.hs" #-}
                                                    )) of
                                             { _relative | _relative `seq` (True) ->
                                             (case (({-# LINE 31 "src\\ByteCodeLocationInfo.ag" #-}
                                                     _lhsIlocation + _relative
                                                     {-# LINE 212 "src/InstrLocFilter.hs" #-}
                                                     )) of
                                              { _target | _target `seq` (True) ->
                                              (case (({-# LINE 88 "src\\InstrLocFilter.ag" #-}
                                                      IntSet.insert _target
                                                      {-# LINE 217 "src/InstrLocFilter.hs" #-}
                                                      )) of
                                               { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                               (case (({-# LINE 88 "src\\InstrLocFilter.ag" #-}
                                                       foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                                       {-# LINE 222 "src/InstrLocFilter.hs" #-}
                                                       )) of
                                                { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                                (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                        (:) hd_ _tlIoutput
                                                        {-# LINE 227 "src/InstrLocFilter.hs" #-}
                                                        )) of
                                                 { _output | _output `seq` (True) ->
                                                 (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                         _output
                                                         {-# LINE 232 "src/InstrLocFilter.hs" #-}
                                                         )) of
                                                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                          _tlIrevLocation
                                                          {-# LINE 237 "src/InstrLocFilter.hs" #-}
                                                          )) of
                                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                   ( _lhsOlocs,_lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }) }))
                        in  sem_CaseOffsets_Cons_1)) of
                 { ( sem_CaseOffsets_1) | True ->
                 ( _lhsOlocation,sem_CaseOffsets_1) }) }) }) }))
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 249 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case ((let sem_CaseOffsets_Nil_1 :: T_CaseOffsets_1
                      sem_CaseOffsets_Nil_1 =
                          (\ _lhsIrevLocation ->
                               (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                       mempty
                                       {-# LINE 257 "src/InstrLocFilter.hs" #-}
                                       )) of
                                { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                        []
                                        {-# LINE 262 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _output | _output `seq` (True) ->
                                 (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                         _output
                                         {-# LINE 267 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 272 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   ( _lhsOlocs,_lhsOoutput,_lhsOrevLocation) }) }) }) }))
                  in  sem_CaseOffsets_Nil_1)) of
           { ( sem_CaseOffsets_1) | True ->
           ( _lhsOlocation,sem_CaseOffsets_1) }) }))
-- ClassInfo ---------------------------------------------------
-- cata
sem_ClassInfo :: ClassInfo ->
                 T_ClassInfo
sem_ClassInfo (ClassInfo_Info _con _traits) =
    (sem_ClassInfo_Info _con (sem_Traits _traits))
-- semantic domain
type T_ClassInfo = ( )
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    ( )
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = ( )
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    ( )
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    ( )
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
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            DebugType_Local
            {-# LINE 320 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 325 "src/InstrLocFilter.hs" #-}
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
type T_Exception = ( IntSet,Exception)
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    (case (({-# LINE 77 "src\\InstrLocFilter.ag" #-}
            IntSet.fromList $ map fromIntegral $ [from_,to_,target_]
            {-# LINE 346 "src/InstrLocFilter.hs" #-}
            )) of
     { _lhsOlocs | _lhsOlocs `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             Exception_Info from_ to_ target_ tp_ name_
             {-# LINE 351 "src/InstrLocFilter.hs" #-}
             )) of
      { _output | _output `seq` (True) ->
      (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
              _output
              {-# LINE 356 "src/InstrLocFilter.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOlocs,_lhsOoutput) }) }) })
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = ( IntSet,Exceptions)
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIlocs,_tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIlocs,_hdIoutput) | True ->
              (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                      _hdIlocs `mappend` _tlIlocs
                      {-# LINE 378 "src/InstrLocFilter.hs" #-}
                      )) of
               { _lhsOlocs | _lhsOlocs `seq` (True) ->
               (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                       (:) _hdIoutput _tlIoutput
                       {-# LINE 383 "src/InstrLocFilter.hs" #-}
                       )) of
                { _output | _output `seq` (True) ->
                (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                        _output
                        {-# LINE 388 "src/InstrLocFilter.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOlocs,_lhsOoutput) }) }) }) }) })
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
            mempty
            {-# LINE 396 "src/InstrLocFilter.hs" #-}
            )) of
     { _lhsOlocs | _lhsOlocs `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             []
             {-# LINE 401 "src/InstrLocFilter.hs" #-}
             )) of
      { _output | _output `seq` (True) ->
      (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
              _output
              {-# LINE 406 "src/InstrLocFilter.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOlocs,_lhsOoutput) }) }) })
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
type T_InstanceInfo = ( )
sem_InstanceInfo_Info :: Word32 ->
                         Word32 ->
                         T_InstanceFlags ->
                         Word32 ->
                         T_Interfaces ->
                         Word32 ->
                         T_Traits ->
                         T_InstanceInfo
sem_InstanceInfo_Info name_ super_ flags_ protectedNs_ interfaces_ constructor_ traits_ =
    ( )
-- InstanceInfos -----------------------------------------------
-- cata
sem_InstanceInfos :: InstanceInfos ->
                     T_InstanceInfos
sem_InstanceInfos list =
    (Prelude.foldr sem_InstanceInfos_Cons sem_InstanceInfos_Nil (Prelude.map sem_InstanceInfo list))
-- semantic domain
type T_InstanceInfos = ( )
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    ( )
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    ( )
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
type T_Instruction = Int ->
                     ( Bool,Int,T_Instruction_1)
type T_Instruction_1 = Bool ->
                       Bool ->
                       Int ->
                       ( IntSet,Bool,Int,T_Instruction_2)
type T_Instruction_2 = IntSet ->
                       ( Instruction,Bool)
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 856 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 861 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Add_1 :: T_Instruction_1
                       sem_Instruction_Add_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 871 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 876 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 881 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Add_2 :: T_Instruction_2
                                               sem_Instruction_Add_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Add
                                                                {-# LINE 889 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 894 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 899 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Add_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Add_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 914 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 919 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Add_i_1 :: T_Instruction_1
                       sem_Instruction_Add_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 929 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 934 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 939 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Add_i_2 :: T_Instruction_2
                                               sem_Instruction_Add_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Add_i
                                                                {-# LINE 947 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 952 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 957 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Add_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Add_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 972 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 977 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Add_d_1 :: T_Instruction_1
                       sem_Instruction_Add_d_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 987 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 992 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 997 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Add_d_2 :: T_Instruction_2
                                               sem_Instruction_Add_d_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Add_d
                                                                {-# LINE 1005 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1010 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1015 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Add_d_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Add_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1031 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1036 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_ApplyType_1 :: T_Instruction_1
                       sem_Instruction_ApplyType_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1046 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1051 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1056 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_ApplyType_2 :: T_Instruction_2
                                               sem_Instruction_ApplyType_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_ApplyType name_
                                                                {-# LINE 1064 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1069 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1074 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_ApplyType_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_ApplyType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1090 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1095 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_AsType_1 :: T_Instruction_1
                       sem_Instruction_AsType_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1105 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1110 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1115 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_AsType_2 :: T_Instruction_2
                                               sem_Instruction_AsType_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_AsType name_
                                                                {-# LINE 1123 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1128 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1133 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_AsType_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_AsType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1148 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1153 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_AsTypeLate_1 :: T_Instruction_1
                       sem_Instruction_AsTypeLate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1163 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1168 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1173 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_AsTypeLate_2 :: T_Instruction_2
                                               sem_Instruction_AsTypeLate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_AsTypeLate
                                                                {-# LINE 1181 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1186 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1191 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_AsTypeLate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_AsTypeLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1206 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1211 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Breakpoint_1 :: T_Instruction_1
                       sem_Instruction_Breakpoint_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1221 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1226 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1231 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Breakpoint_2 :: T_Instruction_2
                                               sem_Instruction_Breakpoint_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Breakpoint
                                                                {-# LINE 1239 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1244 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1249 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Breakpoint_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Breakpoint_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1265 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1270 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_BreakLine_1 :: T_Instruction_1
                       sem_Instruction_BreakLine_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1280 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1285 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1290 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_BreakLine_2 :: T_Instruction_2
                                               sem_Instruction_BreakLine_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_BreakLine line_
                                                                {-# LINE 1298 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1303 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1308 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_BreakLine_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_BreakLine_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1323 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1328 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_BitAnd_1 :: T_Instruction_1
                       sem_Instruction_BitAnd_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1338 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1343 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1348 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_BitAnd_2 :: T_Instruction_2
                                               sem_Instruction_BitAnd_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_BitAnd
                                                                {-# LINE 1356 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1361 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1366 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_BitAnd_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_BitAnd_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1381 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1386 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_BitNot_1 :: T_Instruction_1
                       sem_Instruction_BitNot_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1396 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1401 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1406 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_BitNot_2 :: T_Instruction_2
                                               sem_Instruction_BitNot_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_BitNot
                                                                {-# LINE 1414 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1419 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1424 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_BitNot_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_BitNot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1439 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1444 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_BitOr_1 :: T_Instruction_1
                       sem_Instruction_BitOr_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1454 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1459 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1464 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_BitOr_2 :: T_Instruction_2
                                               sem_Instruction_BitOr_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_BitOr
                                                                {-# LINE 1472 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1477 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1482 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_BitOr_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_BitOr_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1497 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1502 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_BitXor_1 :: T_Instruction_1
                       sem_Instruction_BitXor_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1512 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1517 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1522 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_BitXor_2 :: T_Instruction_2
                                               sem_Instruction_BitXor_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_BitXor
                                                                {-# LINE 1530 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1535 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1540 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_BitXor_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_BitXor_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1556 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1561 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Call_1 :: T_Instruction_1
                       sem_Instruction_Call_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1571 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1576 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1581 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Call_2 :: T_Instruction_2
                                               sem_Instruction_Call_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Call argCount_
                                                                {-# LINE 1589 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1594 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1599 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Call_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Call_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1616 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1621 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallInterface_1 :: T_Instruction_1
                       sem_Instruction_CallInterface_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1631 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1636 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1641 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallInterface_2 :: T_Instruction_2
                                               sem_Instruction_CallInterface_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallInterface name_ argCount_
                                                                {-# LINE 1649 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1654 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1659 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallInterface_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallInterface_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1676 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1681 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallMethod_1 :: T_Instruction_1
                       sem_Instruction_CallMethod_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1691 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1696 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1701 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallMethod_2 :: T_Instruction_2
                                               sem_Instruction_CallMethod_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallMethod index_ argCount_
                                                                {-# LINE 1709 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1714 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1719 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallMethod_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallMethod_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1736 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1741 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallProp_1 :: T_Instruction_1
                       sem_Instruction_CallProp_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1751 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1756 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1761 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallProp_2 :: T_Instruction_2
                                               sem_Instruction_CallProp_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallProp name_ argCount_
                                                                {-# LINE 1769 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1774 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1779 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallProp_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallProp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1796 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1801 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallPropLex_1 :: T_Instruction_1
                       sem_Instruction_CallPropLex_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1811 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1816 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1821 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallPropLex_2 :: T_Instruction_2
                                               sem_Instruction_CallPropLex_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallPropLex name_ argCount_
                                                                {-# LINE 1829 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1834 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1839 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallPropLex_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallPropLex_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1856 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1861 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallPropVoid_1 :: T_Instruction_1
                       sem_Instruction_CallPropVoid_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1871 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1876 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1881 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallPropVoid_2 :: T_Instruction_2
                                               sem_Instruction_CallPropVoid_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallPropVoid name_ argCount_
                                                                {-# LINE 1889 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1894 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1899 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallPropVoid_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallPropVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1916 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1921 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallStatic_1 :: T_Instruction_1
                       sem_Instruction_CallStatic_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1931 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1936 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 1941 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallStatic_2 :: T_Instruction_2
                                               sem_Instruction_CallStatic_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallStatic method_ argCount_
                                                                {-# LINE 1949 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 1954 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 1959 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallStatic_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallStatic_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 1976 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 1981 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallSuper_1 :: T_Instruction_1
                       sem_Instruction_CallSuper_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 1991 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 1996 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2001 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallSuper_2 :: T_Instruction_2
                                               sem_Instruction_CallSuper_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallSuper name_ argCount_
                                                                {-# LINE 2009 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2014 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2019 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallSuper_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2034 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2039 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallSuperId_1 :: T_Instruction_1
                       sem_Instruction_CallSuperId_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2049 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2054 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2059 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallSuperId_2 :: T_Instruction_2
                                               sem_Instruction_CallSuperId_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallSuperId
                                                                {-# LINE 2067 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2072 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2077 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallSuperId_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallSuperId_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2094 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2099 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CallSuperVoid_1 :: T_Instruction_1
                       sem_Instruction_CallSuperVoid_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2109 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2114 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2119 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CallSuperVoid_2 :: T_Instruction_2
                                               sem_Instruction_CallSuperVoid_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CallSuperVoid name_ argCount_
                                                                {-# LINE 2127 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2132 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2137 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CallSuperVoid_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CallSuperVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2152 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2157 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_CheckFilter_1 :: T_Instruction_1
                       sem_Instruction_CheckFilter_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2167 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2172 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2177 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_CheckFilter_2 :: T_Instruction_2
                                               sem_Instruction_CheckFilter_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_CheckFilter
                                                                {-# LINE 2185 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2190 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2195 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_CheckFilter_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_CheckFilter_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2211 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2216 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_1 :: T_Instruction_1
                       sem_Instruction_Coerce_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2226 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2231 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2236 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce name_
                                                                {-# LINE 2244 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2249 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2254 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2269 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2274 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_a_1 :: T_Instruction_1
                       sem_Instruction_Coerce_a_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2284 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2289 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2294 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_a_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_a_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_a
                                                                {-# LINE 2302 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2307 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2312 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_a_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_a_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2327 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2332 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_b_1 :: T_Instruction_1
                       sem_Instruction_Coerce_b_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2342 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2347 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2352 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_b_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_b_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_b
                                                                {-# LINE 2360 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2365 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2370 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_b_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_b_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2385 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2390 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_d_1 :: T_Instruction_1
                       sem_Instruction_Coerce_d_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2400 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2405 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2410 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_d_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_d_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_d
                                                                {-# LINE 2418 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2423 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2428 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_d_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2443 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2448 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_i_1 :: T_Instruction_1
                       sem_Instruction_Coerce_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2458 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2463 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2468 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_i_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_i
                                                                {-# LINE 2476 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2481 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2486 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2501 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2506 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_o_1 :: T_Instruction_1
                       sem_Instruction_Coerce_o_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2516 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2521 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2526 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_o_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_o_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_o
                                                                {-# LINE 2534 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2539 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2544 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_o_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_o_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2559 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2564 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_s_1 :: T_Instruction_1
                       sem_Instruction_Coerce_s_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2574 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2579 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2584 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_s_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_s_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_s
                                                                {-# LINE 2592 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2597 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2602 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_s_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_s_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2617 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2622 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Coerce_u_1 :: T_Instruction_1
                       sem_Instruction_Coerce_u_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2632 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2637 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2642 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Coerce_u_2 :: T_Instruction_2
                                               sem_Instruction_Coerce_u_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Coerce_u
                                                                {-# LINE 2650 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2655 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2660 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Coerce_u_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Coerce_u_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2675 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2680 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Concat_1 :: T_Instruction_1
                       sem_Instruction_Concat_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2690 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2695 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2700 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Concat_2 :: T_Instruction_2
                                               sem_Instruction_Concat_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Concat
                                                                {-# LINE 2708 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2713 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2718 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Concat_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Concat_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2734 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2739 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Construct_1 :: T_Instruction_1
                       sem_Instruction_Construct_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2749 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2754 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2759 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Construct_2 :: T_Instruction_2
                                               sem_Instruction_Construct_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Construct argCount_
                                                                {-# LINE 2767 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2772 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2777 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Construct_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Construct_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2794 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2799 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_ConstructProp_1 :: T_Instruction_1
                       sem_Instruction_ConstructProp_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2809 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2814 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2819 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_ConstructProp_2 :: T_Instruction_2
                                               sem_Instruction_ConstructProp_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_ConstructProp name_ argCount_
                                                                {-# LINE 2827 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2832 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2837 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_ConstructProp_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_ConstructProp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2853 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2858 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_ConstructSuper_1 :: T_Instruction_1
                       sem_Instruction_ConstructSuper_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2868 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2873 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2878 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_ConstructSuper_2 :: T_Instruction_2
                                               sem_Instruction_ConstructSuper_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_ConstructSuper argCount_
                                                                {-# LINE 2886 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2891 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2896 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_ConstructSuper_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_ConstructSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2911 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2916 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_b_1 :: T_Instruction_1
                       sem_Instruction_Convert_b_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2926 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2931 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2936 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_b_2 :: T_Instruction_2
                                               sem_Instruction_Convert_b_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_b
                                                                {-# LINE 2944 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 2949 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 2954 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_b_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_b_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 2969 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 2974 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_i_1 :: T_Instruction_1
                       sem_Instruction_Convert_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 2984 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 2989 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 2994 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_i_2 :: T_Instruction_2
                                               sem_Instruction_Convert_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_i
                                                                {-# LINE 3002 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3007 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3012 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3027 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3032 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_d_1 :: T_Instruction_1
                       sem_Instruction_Convert_d_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3042 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3047 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3052 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_d_2 :: T_Instruction_2
                                               sem_Instruction_Convert_d_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_d
                                                                {-# LINE 3060 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3065 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3070 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_d_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3085 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3090 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_o_1 :: T_Instruction_1
                       sem_Instruction_Convert_o_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3100 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3105 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3110 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_o_2 :: T_Instruction_2
                                               sem_Instruction_Convert_o_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_o
                                                                {-# LINE 3118 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3123 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3128 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_o_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_o_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3143 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3148 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_u_1 :: T_Instruction_1
                       sem_Instruction_Convert_u_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3158 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3163 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3168 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_u_2 :: T_Instruction_2
                                               sem_Instruction_Convert_u_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_u
                                                                {-# LINE 3176 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3181 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3186 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_u_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_u_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3201 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3206 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Convert_s_1 :: T_Instruction_1
                       sem_Instruction_Convert_s_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3216 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3221 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3226 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Convert_s_2 :: T_Instruction_2
                                               sem_Instruction_Convert_s_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Convert_s
                                                                {-# LINE 3234 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3239 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3244 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Convert_s_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Convert_s_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3263 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3268 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Debug_1 :: T_Instruction_1
                       sem_Instruction_Debug_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3278 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3283 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3288 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Debug_2 :: T_Instruction_2
                                               sem_Instruction_Debug_2 =
                                                   (\ _lhsIretain ->
                                                        (case (tp_) of
                                                         { ( _tpIoutput) | True ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     Instruction_Debug _tpIoutput name_ reg_ extra_
                                                                     {-# LINE 3298 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _output | _output `seq` (True) ->
                                                              (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                      _output
                                                                      {-# LINE 3303 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                               (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                       False
                                                                       {-# LINE 3308 "src/InstrLocFilter.hs" #-}
                                                                       )) of
                                                                { _lhsOskip | _lhsOskip `seq` (True) ->
                                                                ( _lhsOoutput,_lhsOskip) }) }) }) }))
                                           in  sem_Instruction_Debug_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Debug_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3324 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3329 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DebugFile_1 :: T_Instruction_1
                       sem_Instruction_DebugFile_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3339 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3344 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3349 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DebugFile_2 :: T_Instruction_2
                                               sem_Instruction_DebugFile_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DebugFile name_
                                                                {-# LINE 3357 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3362 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3367 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DebugFile_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DebugFile_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3383 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3388 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DebugLine_1 :: T_Instruction_1
                       sem_Instruction_DebugLine_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3398 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3403 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3408 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DebugLine_2 :: T_Instruction_2
                                               sem_Instruction_DebugLine_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DebugLine line_
                                                                {-# LINE 3416 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3421 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3426 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DebugLine_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DebugLine_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3442 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3447 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DecLocal_1 :: T_Instruction_1
                       sem_Instruction_DecLocal_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3457 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3462 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3467 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DecLocal_2 :: T_Instruction_2
                                               sem_Instruction_DecLocal_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DecLocal reg_
                                                                {-# LINE 3475 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3480 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3485 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DecLocal_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DecLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3501 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3506 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DecLocal_i_1 :: T_Instruction_1
                       sem_Instruction_DecLocal_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3516 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3521 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3526 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DecLocal_i_2 :: T_Instruction_2
                                               sem_Instruction_DecLocal_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DecLocal_i reg_
                                                                {-# LINE 3534 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3539 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3544 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DecLocal_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DecLocal_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3559 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3564 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Decrement_1 :: T_Instruction_1
                       sem_Instruction_Decrement_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3574 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3579 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3584 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Decrement_2 :: T_Instruction_2
                                               sem_Instruction_Decrement_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Decrement
                                                                {-# LINE 3592 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3597 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3602 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Decrement_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Decrement_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3617 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3622 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Decrement_i_1 :: T_Instruction_1
                       sem_Instruction_Decrement_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3632 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3637 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3642 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Decrement_i_2 :: T_Instruction_2
                                               sem_Instruction_Decrement_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Decrement_i
                                                                {-# LINE 3650 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3655 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3660 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Decrement_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Decrement_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3676 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3681 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DeleteProperty_1 :: T_Instruction_1
                       sem_Instruction_DeleteProperty_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3691 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3696 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3701 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DeleteProperty_2 :: T_Instruction_2
                                               sem_Instruction_DeleteProperty_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DeleteProperty name_
                                                                {-# LINE 3709 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3714 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3719 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DeleteProperty_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DeleteProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3734 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3739 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DeletePropertyLate_1 :: T_Instruction_1
                       sem_Instruction_DeletePropertyLate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3749 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3754 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3759 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DeletePropertyLate_2 :: T_Instruction_2
                                               sem_Instruction_DeletePropertyLate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DeletePropertyLate
                                                                {-# LINE 3767 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3772 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3777 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DeletePropertyLate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DeletePropertyLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3792 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3797 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Divide_1 :: T_Instruction_1
                       sem_Instruction_Divide_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3807 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3812 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3817 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Divide_2 :: T_Instruction_2
                                               sem_Instruction_Divide_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Divide
                                                                {-# LINE 3825 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3830 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3835 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Divide_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Divide_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3850 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3855 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Dup_1 :: T_Instruction_1
                       sem_Instruction_Dup_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3865 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3870 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3875 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Dup_2 :: T_Instruction_2
                                               sem_Instruction_Dup_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Dup
                                                                {-# LINE 3883 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3888 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3893 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Dup_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Dup_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3909 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3914 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Dxns_1 :: T_Instruction_1
                       sem_Instruction_Dxns_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3924 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3929 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3934 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Dxns_2 :: T_Instruction_2
                                               sem_Instruction_Dxns_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Dxns name_
                                                                {-# LINE 3942 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 3947 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 3952 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Dxns_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Dxns_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 3967 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 3972 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_DxnsLate_1 :: T_Instruction_1
                       sem_Instruction_DxnsLate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 3982 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 3987 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 3992 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_DxnsLate_2 :: T_Instruction_2
                                               sem_Instruction_DxnsLate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_DxnsLate
                                                                {-# LINE 4000 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4005 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4010 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_DxnsLate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_DxnsLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4025 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4030 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Equals_1 :: T_Instruction_1
                       sem_Instruction_Equals_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4040 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4045 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4050 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Equals_2 :: T_Instruction_2
                                               sem_Instruction_Equals_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Equals
                                                                {-# LINE 4058 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4063 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4068 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Equals_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Equals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4083 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4088 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_EscXAttr_1 :: T_Instruction_1
                       sem_Instruction_EscXAttr_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4098 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4103 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4108 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_EscXAttr_2 :: T_Instruction_2
                                               sem_Instruction_EscXAttr_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_EscXAttr
                                                                {-# LINE 4116 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4121 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4126 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_EscXAttr_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_EscXAttr_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4141 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4146 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_EscXElem_1 :: T_Instruction_1
                       sem_Instruction_EscXElem_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4156 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4161 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4166 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_EscXElem_2 :: T_Instruction_2
                                               sem_Instruction_EscXElem_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_EscXElem
                                                                {-# LINE 4174 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4179 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4184 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_EscXElem_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_EscXElem_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4200 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4205 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_FindDef_1 :: T_Instruction_1
                       sem_Instruction_FindDef_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4215 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4220 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4225 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_FindDef_2 :: T_Instruction_2
                                               sem_Instruction_FindDef_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_FindDef name_
                                                                {-# LINE 4233 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4238 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4243 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_FindDef_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_FindDef_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4259 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4264 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_FindPropertyGlobalStrict_1 :: T_Instruction_1
                       sem_Instruction_FindPropertyGlobalStrict_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4274 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4279 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4284 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_FindPropertyGlobalStrict_2 :: T_Instruction_2
                                               sem_Instruction_FindPropertyGlobalStrict_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_FindPropertyGlobalStrict name_
                                                                {-# LINE 4292 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4297 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4302 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_FindPropertyGlobalStrict_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_FindPropertyGlobalStrict_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4318 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4323 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_FindPropertyGlobal_1 :: T_Instruction_1
                       sem_Instruction_FindPropertyGlobal_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4333 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4338 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4343 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_FindPropertyGlobal_2 :: T_Instruction_2
                                               sem_Instruction_FindPropertyGlobal_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_FindPropertyGlobal name_
                                                                {-# LINE 4351 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4356 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4361 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_FindPropertyGlobal_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_FindPropertyGlobal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4377 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4382 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_FindProperty_1 :: T_Instruction_1
                       sem_Instruction_FindProperty_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4392 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4397 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4402 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_FindProperty_2 :: T_Instruction_2
                                               sem_Instruction_FindProperty_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_FindProperty name_
                                                                {-# LINE 4410 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4415 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4420 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_FindProperty_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_FindProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4436 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4441 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_FindPropStrict_1 :: T_Instruction_1
                       sem_Instruction_FindPropStrict_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4451 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4456 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4461 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_FindPropStrict_2 :: T_Instruction_2
                                               sem_Instruction_FindPropStrict_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_FindPropStrict name_
                                                                {-# LINE 4469 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4474 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4479 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_FindPropStrict_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_FindPropStrict_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4495 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4500 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetDescendants_1 :: T_Instruction_1
                       sem_Instruction_GetDescendants_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4510 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4515 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4520 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetDescendants_2 :: T_Instruction_2
                                               sem_Instruction_GetDescendants_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetDescendants name_
                                                                {-# LINE 4528 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4533 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4538 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetDescendants_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetDescendants_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4553 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4558 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetGlobalScope_1 :: T_Instruction_1
                       sem_Instruction_GetGlobalScope_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4568 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4573 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4578 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetGlobalScope_2 :: T_Instruction_2
                                               sem_Instruction_GetGlobalScope_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetGlobalScope
                                                                {-# LINE 4586 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4591 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4596 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetGlobalScope_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetGlobalScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4612 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4617 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetGlobalSlot_1 :: T_Instruction_1
                       sem_Instruction_GetGlobalSlot_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4627 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4632 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4637 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetGlobalSlot_2 :: T_Instruction_2
                                               sem_Instruction_GetGlobalSlot_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetGlobalSlot slot_
                                                                {-# LINE 4645 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4650 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4655 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetGlobalSlot_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetGlobalSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4671 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4676 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLex_1 :: T_Instruction_1
                       sem_Instruction_GetLex_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4686 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4691 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4696 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLex_2 :: T_Instruction_2
                                               sem_Instruction_GetLex_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLex name_
                                                                {-# LINE 4704 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4709 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4714 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLex_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLex_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4730 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4735 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLocal_1 :: T_Instruction_1
                       sem_Instruction_GetLocal_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4745 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4750 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4755 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLocal_2 :: T_Instruction_2
                                               sem_Instruction_GetLocal_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLocal reg_
                                                                {-# LINE 4763 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4768 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4773 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLocal_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4788 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4793 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLocal0_1 :: T_Instruction_1
                       sem_Instruction_GetLocal0_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4803 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4808 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4813 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLocal0_2 :: T_Instruction_2
                                               sem_Instruction_GetLocal0_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLocal0
                                                                {-# LINE 4821 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4826 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4831 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLocal0_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLocal0_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4846 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4851 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLocal1_1 :: T_Instruction_1
                       sem_Instruction_GetLocal1_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4861 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4866 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4871 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLocal1_2 :: T_Instruction_2
                                               sem_Instruction_GetLocal1_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLocal1
                                                                {-# LINE 4879 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4884 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4889 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLocal1_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLocal1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4904 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4909 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLocal2_1 :: T_Instruction_1
                       sem_Instruction_GetLocal2_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4919 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4924 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4929 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLocal2_2 :: T_Instruction_2
                                               sem_Instruction_GetLocal2_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLocal2
                                                                {-# LINE 4937 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 4942 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 4947 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLocal2_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLocal2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 4962 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 4967 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetLocal3_1 :: T_Instruction_1
                       sem_Instruction_GetLocal3_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 4977 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 4982 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 4987 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetLocal3_2 :: T_Instruction_2
                                               sem_Instruction_GetLocal3_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetLocal3
                                                                {-# LINE 4995 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5000 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5005 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetLocal3_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetLocal3_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5021 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5026 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetOuterScope_1 :: T_Instruction_1
                       sem_Instruction_GetOuterScope_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5036 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5041 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5046 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetOuterScope_2 :: T_Instruction_2
                                               sem_Instruction_GetOuterScope_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetOuterScope name_
                                                                {-# LINE 5054 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5059 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5064 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetOuterScope_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetOuterScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5080 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5085 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetProperty_1 :: T_Instruction_1
                       sem_Instruction_GetProperty_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5095 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5100 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5105 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetProperty_2 :: T_Instruction_2
                                               sem_Instruction_GetProperty_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetProperty name_
                                                                {-# LINE 5113 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5118 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5123 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetProperty_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5139 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5144 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetScopeObject_1 :: T_Instruction_1
                       sem_Instruction_GetScopeObject_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5154 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5159 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5164 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetScopeObject_2 :: T_Instruction_2
                                               sem_Instruction_GetScopeObject_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetScopeObject index_
                                                                {-# LINE 5172 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5177 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5182 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetScopeObject_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetScopeObject_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5198 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5203 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetSlot_1 :: T_Instruction_1
                       sem_Instruction_GetSlot_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5213 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5218 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5223 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetSlot_2 :: T_Instruction_2
                                               sem_Instruction_GetSlot_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetSlot slot_
                                                                {-# LINE 5231 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5236 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5241 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetSlot_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5257 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5262 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GetSuper_1 :: T_Instruction_1
                       sem_Instruction_GetSuper_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5272 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5277 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5282 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GetSuper_2 :: T_Instruction_2
                                               sem_Instruction_GetSuper_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GetSuper name_
                                                                {-# LINE 5290 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5295 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5300 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GetSuper_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GetSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5315 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5320 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GreaterEquals_1 :: T_Instruction_1
                       sem_Instruction_GreaterEquals_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5330 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5335 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5340 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GreaterEquals_2 :: T_Instruction_2
                                               sem_Instruction_GreaterEquals_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GreaterEquals
                                                                {-# LINE 5348 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5353 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5358 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GreaterEquals_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GreaterEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5373 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5378 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_GreaterThan_1 :: T_Instruction_1
                       sem_Instruction_GreaterThan_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5388 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5393 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5398 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_GreaterThan_2 :: T_Instruction_2
                                               sem_Instruction_GreaterThan_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_GreaterThan
                                                                {-# LINE 5406 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5411 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5416 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_GreaterThan_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_GreaterThan_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5431 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5436 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_HasNext_1 :: T_Instruction_1
                       sem_Instruction_HasNext_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5446 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5451 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5456 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_HasNext_2 :: T_Instruction_2
                                               sem_Instruction_HasNext_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_HasNext
                                                                {-# LINE 5464 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5469 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5474 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_HasNext_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_HasNext_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 5491 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5496 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_HasNext2_1 :: T_Instruction_1
                       sem_Instruction_HasNext2_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5506 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 5511 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 5516 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_HasNext2_2 :: T_Instruction_2
                                               sem_Instruction_HasNext2_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_HasNext2 objectReg_ indexReg_
                                                                {-# LINE 5524 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 5529 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 5534 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_HasNext2_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_HasNext2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5550 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5555 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfEq_1 :: T_Instruction_1
                       sem_Instruction_IfEq_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5565 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5570 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5575 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5580 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5585 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5590 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5595 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfEq_2 :: T_Instruction_2
                                                   sem_Instruction_IfEq_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfEq offset_
                                                                    {-# LINE 5603 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 5608 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 5613 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfEq_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfEq_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5629 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5634 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfFalse_1 :: T_Instruction_1
                       sem_Instruction_IfFalse_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5644 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5649 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5654 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5659 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5664 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5669 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5674 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfFalse_2 :: T_Instruction_2
                                                   sem_Instruction_IfFalse_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfFalse offset_
                                                                    {-# LINE 5682 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 5687 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 5692 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfFalse_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfFalse_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5708 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5713 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfGe_1 :: T_Instruction_1
                       sem_Instruction_IfGe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5723 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5728 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5733 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5738 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5743 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5748 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5753 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfGe_2 :: T_Instruction_2
                                                   sem_Instruction_IfGe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfGe offset_
                                                                    {-# LINE 5761 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 5766 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 5771 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfGe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfGe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5787 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5792 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfGt_1 :: T_Instruction_1
                       sem_Instruction_IfGt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5802 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5807 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5812 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5817 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5822 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5827 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5832 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfGt_2 :: T_Instruction_2
                                                   sem_Instruction_IfGt_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfGt offset_
                                                                    {-# LINE 5840 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 5845 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 5850 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfGt_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfGt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5866 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5871 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfLe_1 :: T_Instruction_1
                       sem_Instruction_IfLe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5881 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5886 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5891 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5896 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5901 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5906 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5911 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfLe_2 :: T_Instruction_2
                                                   sem_Instruction_IfLe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfLe offset_
                                                                    {-# LINE 5919 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 5924 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 5929 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfLe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfLe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 5945 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 5950 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfLt_1 :: T_Instruction_1
                       sem_Instruction_IfLt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 5960 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 5965 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 5970 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 5975 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 5980 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 5985 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 5990 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfLt_2 :: T_Instruction_2
                                                   sem_Instruction_IfLt_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfLt offset_
                                                                    {-# LINE 5998 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6003 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6008 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfLt_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfLt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6024 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6029 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfNGe_1 :: T_Instruction_1
                       sem_Instruction_IfNGe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6039 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6044 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6049 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6054 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6059 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6064 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6069 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfNGe_2 :: T_Instruction_2
                                                   sem_Instruction_IfNGe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfNGe offset_
                                                                    {-# LINE 6077 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6082 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6087 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfNGe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNGe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6103 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6108 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfNGt_1 :: T_Instruction_1
                       sem_Instruction_IfNGt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6118 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6123 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6128 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6133 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6138 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6143 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6148 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfNGt_2 :: T_Instruction_2
                                                   sem_Instruction_IfNGt_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfNGt offset_
                                                                    {-# LINE 6156 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6161 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6166 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfNGt_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNGt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6182 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6187 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfNLe_1 :: T_Instruction_1
                       sem_Instruction_IfNLe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6197 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6202 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6207 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6212 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6217 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6222 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6227 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfNLe_2 :: T_Instruction_2
                                                   sem_Instruction_IfNLe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfNLe offset_
                                                                    {-# LINE 6235 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6240 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6245 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfNLe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNLe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6261 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6266 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfNLt_1 :: T_Instruction_1
                       sem_Instruction_IfNLt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6276 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6281 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6286 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6291 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6296 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6301 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6306 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfNLt_2 :: T_Instruction_2
                                                   sem_Instruction_IfNLt_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfNLt offset_
                                                                    {-# LINE 6314 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6319 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6324 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfNLt_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNLt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6340 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6345 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfNe_1 :: T_Instruction_1
                       sem_Instruction_IfNe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6355 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6360 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6365 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6370 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6375 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6380 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6385 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfNe_2 :: T_Instruction_2
                                                   sem_Instruction_IfNe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfNe offset_
                                                                    {-# LINE 6393 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6398 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6403 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfNe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6419 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6424 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfStrictEq_1 :: T_Instruction_1
                       sem_Instruction_IfStrictEq_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6434 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6439 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6444 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6449 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6454 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6459 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6464 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfStrictEq_2 :: T_Instruction_2
                                                   sem_Instruction_IfStrictEq_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfStrictEq offset_
                                                                    {-# LINE 6472 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6477 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6482 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfStrictEq_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfStrictEq_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6498 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6503 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfStrictNe_1 :: T_Instruction_1
                       sem_Instruction_IfStrictNe_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6513 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6518 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6523 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6528 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6533 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6538 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6543 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfStrictNe_2 :: T_Instruction_2
                                                   sem_Instruction_IfStrictNe_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfStrictNe offset_
                                                                    {-# LINE 6551 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6556 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6561 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfStrictNe_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfStrictNe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 6577 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6582 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IfTrue_1 :: T_Instruction_1
                       sem_Instruction_IfTrue_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6592 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 6597 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 6602 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 6607 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 6612 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 6617 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6622 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_IfTrue_2 :: T_Instruction_2
                                                   sem_Instruction_IfTrue_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_IfTrue offset_
                                                                    {-# LINE 6630 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 6635 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 6640 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_IfTrue_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfTrue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6655 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6660 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_In_1 :: T_Instruction_1
                       sem_Instruction_In_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6670 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6675 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6680 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_In_2 :: T_Instruction_2
                                               sem_Instruction_In_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_In
                                                                {-# LINE 6688 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6693 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6698 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_In_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_In_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6714 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6719 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IncLocal_1 :: T_Instruction_1
                       sem_Instruction_IncLocal_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6729 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6734 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6739 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_IncLocal_2 :: T_Instruction_2
                                               sem_Instruction_IncLocal_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_IncLocal reg_
                                                                {-# LINE 6747 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6752 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6757 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_IncLocal_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_IncLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6773 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6778 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IncLocal_i_1 :: T_Instruction_1
                       sem_Instruction_IncLocal_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6788 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6793 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6798 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_IncLocal_i_2 :: T_Instruction_2
                                               sem_Instruction_IncLocal_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_IncLocal_i reg_
                                                                {-# LINE 6806 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6811 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6816 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_IncLocal_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_IncLocal_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6831 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6836 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Increment_1 :: T_Instruction_1
                       sem_Instruction_Increment_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6846 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6851 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6856 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Increment_2 :: T_Instruction_2
                                               sem_Instruction_Increment_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Increment
                                                                {-# LINE 6864 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6869 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6874 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Increment_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Increment_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6889 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6894 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Increment_i_1 :: T_Instruction_1
                       sem_Instruction_Increment_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6904 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6909 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6914 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Increment_i_2 :: T_Instruction_2
                                               sem_Instruction_Increment_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Increment_i
                                                                {-# LINE 6922 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6927 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6932 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Increment_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Increment_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 6948 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 6953 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_InitProperty_1 :: T_Instruction_1
                       sem_Instruction_InitProperty_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 6963 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 6968 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 6973 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_InitProperty_2 :: T_Instruction_2
                                               sem_Instruction_InitProperty_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_InitProperty name_
                                                                {-# LINE 6981 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 6986 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 6991 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_InitProperty_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_InitProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7006 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7011 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_InstanceOf_1 :: T_Instruction_1
                       sem_Instruction_InstanceOf_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7021 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7026 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7031 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_InstanceOf_2 :: T_Instruction_2
                                               sem_Instruction_InstanceOf_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_InstanceOf
                                                                {-# LINE 7039 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7044 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7049 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_InstanceOf_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_InstanceOf_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7065 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7070 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IsType_1 :: T_Instruction_1
                       sem_Instruction_IsType_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7080 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7085 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7090 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_IsType_2 :: T_Instruction_2
                                               sem_Instruction_IsType_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_IsType name_
                                                                {-# LINE 7098 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7103 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7108 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_IsType_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_IsType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7123 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7128 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_IsTypeLate_1 :: T_Instruction_1
                       sem_Instruction_IsTypeLate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7138 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7143 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7148 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_IsTypeLate_2 :: T_Instruction_2
                                               sem_Instruction_IsTypeLate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_IsTypeLate
                                                                {-# LINE 7156 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7161 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7166 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_IsTypeLate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_IsTypeLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 7182 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7187 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Jump_1 :: T_Instruction_1
                       sem_Instruction_Jump_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7197 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                         fromS24 offset_
                                         {-# LINE 7202 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _relative | _relative `seq` (True) ->
                                  (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation + _relative
                                          {-# LINE 7207 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _target | _target `seq` (True) ->
                                   (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                           IntSet.insert _target
                                           {-# LINE 7212 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                    (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                            foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                            {-# LINE 7217 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                     (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                             True
                                             {-# LINE 7222 "src/InstrLocFilter.hs" #-}
                                             )) of
                                      { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 7227 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       (case ((let sem_Instruction_Jump_2 :: T_Instruction_2
                                                   sem_Instruction_Jump_2 =
                                                       (\ _lhsIretain ->
                                                            (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                    Instruction_Jump offset_
                                                                    {-# LINE 7235 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _output | _output `seq` (True) ->
                                                             (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                     _output
                                                                     {-# LINE 7240 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                              (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                      False
                                                                      {-# LINE 7245 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOskip | _lhsOskip `seq` (True) ->
                                                               ( _lhsOoutput,_lhsOskip) }) }) }))
                                               in  sem_Instruction_Jump_2)) of
                                        { ( sem_Instruction_2) | True ->
                                        ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_Jump_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7261 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7266 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Kill_1 :: T_Instruction_1
                       sem_Instruction_Kill_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7276 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7281 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7286 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Kill_2 :: T_Instruction_2
                                               sem_Instruction_Kill_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Kill reg_
                                                                {-# LINE 7294 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7299 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7304 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Kill_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Kill_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7319 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7324 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Label_1 :: T_Instruction_1
                       sem_Instruction_Label_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7334 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7339 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7344 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Label_2 :: T_Instruction_2
                                               sem_Instruction_Label_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Label
                                                                {-# LINE 7352 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7357 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7362 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Label_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Label_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7377 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7382 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LessEquals_1 :: T_Instruction_1
                       sem_Instruction_LessEquals_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7392 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7397 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7402 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LessEquals_2 :: T_Instruction_2
                                               sem_Instruction_LessEquals_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LessEquals
                                                                {-# LINE 7410 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7415 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7420 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LessEquals_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LessEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7435 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7440 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LessThan_1 :: T_Instruction_1
                       sem_Instruction_LessThan_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7450 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7455 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7460 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LessThan_2 :: T_Instruction_2
                                               sem_Instruction_LessThan_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LessThan
                                                                {-# LINE 7468 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7473 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7478 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LessThan_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LessThan_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7493 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7498 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LoadFloat32_1 :: T_Instruction_1
                       sem_Instruction_LoadFloat32_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7508 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7513 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7518 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LoadFloat32_2 :: T_Instruction_2
                                               sem_Instruction_LoadFloat32_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LoadFloat32
                                                                {-# LINE 7526 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7531 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7536 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LoadFloat32_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LoadFloat32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7551 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7556 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LoadFloat64_1 :: T_Instruction_1
                       sem_Instruction_LoadFloat64_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7566 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7571 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7576 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LoadFloat64_2 :: T_Instruction_2
                                               sem_Instruction_LoadFloat64_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LoadFloat64
                                                                {-# LINE 7584 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7589 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7594 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LoadFloat64_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LoadFloat64_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7609 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7614 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect8_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect8_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7624 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7629 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7634 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LoadIndirect8_2 :: T_Instruction_2
                                               sem_Instruction_LoadIndirect8_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LoadIndirect8
                                                                {-# LINE 7642 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7647 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7652 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LoadIndirect8_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LoadIndirect8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7667 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7672 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect16_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect16_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7682 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7687 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7692 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LoadIndirect16_2 :: T_Instruction_2
                                               sem_Instruction_LoadIndirect16_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LoadIndirect16
                                                                {-# LINE 7700 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7705 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7710 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LoadIndirect16_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LoadIndirect16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7725 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7730 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect32_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect32_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7740 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7745 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7750 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_LoadIndirect32_2 :: T_Instruction_2
                                               sem_Instruction_LoadIndirect32_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_LoadIndirect32
                                                                {-# LINE 7758 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7763 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7768 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_LoadIndirect32_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_LoadIndirect32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 65 "src\\InstrLocFilter.ag" #-}
                 True
                 {-# LINE 7785 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7790 "src/InstrLocFilter.hs" #-}
                  )) of
           { _caseOffsetsOlocation | _caseOffsetsOlocation `seq` (True) ->
           (case (caseOffsets_ _caseOffsetsOlocation) of
            { ( _caseOffsetsIlocation,caseOffsets_1) | True ->
                (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                        _caseOffsetsIlocation
                        {-# LINE 7797 "src/InstrLocFilter.hs" #-}
                        )) of
                 { _lhsOlocation | _lhsOlocation `seq` (True) ->
                 (case ((let sem_Instruction_LookupSwitch_1 :: T_Instruction_1
                             sem_Instruction_LookupSwitch_1 =
                                 (\ _lhsIisBranch
                                    _lhsIrevIsBranch
                                    _lhsIrevLocation ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 7807 "src/InstrLocFilter.hs" #-}
                                              )) of
                                       { _caseOffsetsOrevLocation | _caseOffsetsOrevLocation `seq` (True) ->
                                       (case (caseOffsets_1 _caseOffsetsOrevLocation) of
                                        { ( _caseOffsetsIlocs,_caseOffsetsIoutput,_caseOffsetsIrevLocation) | True ->
                                            (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                                    _caseOffsetsIlocs
                                                    {-# LINE 7814 "src/InstrLocFilter.hs" #-}
                                                    )) of
                                             { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                             (case (({-# LINE 23 "src\\ByteCodeLocationInfo.ag" #-}
                                                     fromS24 defaultOffset_
                                                     {-# LINE 7819 "src/InstrLocFilter.hs" #-}
                                                     )) of
                                              { _relative | _relative `seq` (True) ->
                                              (case (({-# LINE 24 "src\\ByteCodeLocationInfo.ag" #-}
                                                      _lhsIlocation + _relative
                                                      {-# LINE 7824 "src/InstrLocFilter.hs" #-}
                                                      )) of
                                               { _target | _target `seq` (True) ->
                                               (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                                       IntSet.insert _target
                                                       {-# LINE 7829 "src/InstrLocFilter.hs" #-}
                                                       )) of
                                                { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                                (case (({-# LINE 85 "src\\InstrLocFilter.ag" #-}
                                                        foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                                        {-# LINE 7834 "src/InstrLocFilter.hs" #-}
                                                        )) of
                                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                                 (case (({-# LINE 66 "src\\InstrLocFilter.ag" #-}
                                                         True
                                                         {-# LINE 7839 "src/InstrLocFilter.hs" #-}
                                                         )) of
                                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                          _caseOffsetsIrevLocation
                                                          {-# LINE 7844 "src/InstrLocFilter.hs" #-}
                                                          )) of
                                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                   (case ((let sem_Instruction_LookupSwitch_2 :: T_Instruction_2
                                                               sem_Instruction_LookupSwitch_2 =
                                                                   (\ _lhsIretain ->
                                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                                Instruction_LookupSwitch defaultOffset_ _caseOffsetsIoutput
                                                                                {-# LINE 7852 "src/InstrLocFilter.hs" #-}
                                                                                )) of
                                                                         { _output | _output `seq` (True) ->
                                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                                 _output
                                                                                 {-# LINE 7857 "src/InstrLocFilter.hs" #-}
                                                                                 )) of
                                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                                  False
                                                                                  {-# LINE 7862 "src/InstrLocFilter.hs" #-}
                                                                                  )) of
                                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                                           in  sem_Instruction_LookupSwitch_2)) of
                                                    { ( sem_Instruction_2) | True ->
                                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }) }) }) }) }))
                         in  sem_Instruction_LookupSwitch_1)) of
                  { ( sem_Instruction_1) | True ->
                  ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }) }) }))
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7877 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7882 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Lshift_1 :: T_Instruction_1
                       sem_Instruction_Lshift_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7892 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7897 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7902 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Lshift_2 :: T_Instruction_2
                                               sem_Instruction_Lshift_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Lshift
                                                                {-# LINE 7910 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7915 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7920 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Lshift_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Lshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7935 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7940 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Modulo_1 :: T_Instruction_1
                       sem_Instruction_Modulo_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 7950 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 7955 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 7960 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Modulo_2 :: T_Instruction_2
                                               sem_Instruction_Modulo_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Modulo
                                                                {-# LINE 7968 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 7973 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 7978 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Modulo_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Modulo_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 7993 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 7998 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Multiply_1 :: T_Instruction_1
                       sem_Instruction_Multiply_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8008 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8013 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8018 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Multiply_2 :: T_Instruction_2
                                               sem_Instruction_Multiply_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Multiply
                                                                {-# LINE 8026 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8031 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8036 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Multiply_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Multiply_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8051 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8056 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Multiply_i_1 :: T_Instruction_1
                       sem_Instruction_Multiply_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8066 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8071 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8076 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Multiply_i_2 :: T_Instruction_2
                                               sem_Instruction_Multiply_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Multiply_i
                                                                {-# LINE 8084 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8089 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8094 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Multiply_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Multiply_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8109 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8114 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Negate_1 :: T_Instruction_1
                       sem_Instruction_Negate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8124 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8129 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8134 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Negate_2 :: T_Instruction_2
                                               sem_Instruction_Negate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Negate
                                                                {-# LINE 8142 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8147 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8152 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Negate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Negate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8167 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8172 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Negate_i_1 :: T_Instruction_1
                       sem_Instruction_Negate_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8182 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8187 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8192 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Negate_i_2 :: T_Instruction_2
                                               sem_Instruction_Negate_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Negate_i
                                                                {-# LINE 8200 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8205 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8210 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Negate_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Negate_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8225 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8230 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewActivation_1 :: T_Instruction_1
                       sem_Instruction_NewActivation_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8240 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8245 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8250 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewActivation_2 :: T_Instruction_2
                                               sem_Instruction_NewActivation_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewActivation
                                                                {-# LINE 8258 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8263 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8268 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewActivation_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewActivation_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8284 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8289 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewArray_1 :: T_Instruction_1
                       sem_Instruction_NewArray_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8299 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8304 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8309 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewArray_2 :: T_Instruction_2
                                               sem_Instruction_NewArray_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewArray argCount_
                                                                {-# LINE 8317 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8322 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8327 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewArray_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewArray_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8343 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8348 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewCatch_1 :: T_Instruction_1
                       sem_Instruction_NewCatch_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8358 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8363 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8368 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewCatch_2 :: T_Instruction_2
                                               sem_Instruction_NewCatch_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewCatch exception_
                                                                {-# LINE 8376 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8381 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8386 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewCatch_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewCatch_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8402 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8407 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewClass_1 :: T_Instruction_1
                       sem_Instruction_NewClass_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8417 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8422 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8427 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewClass_2 :: T_Instruction_2
                                               sem_Instruction_NewClass_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewClass class_
                                                                {-# LINE 8435 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8440 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8445 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewClass_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewClass_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8461 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8466 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewFunction_1 :: T_Instruction_1
                       sem_Instruction_NewFunction_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8476 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8481 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8486 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewFunction_2 :: T_Instruction_2
                                               sem_Instruction_NewFunction_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewFunction method_
                                                                {-# LINE 8494 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8499 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8504 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewFunction_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewFunction_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8520 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8525 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NewObject_1 :: T_Instruction_1
                       sem_Instruction_NewObject_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8535 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8540 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8545 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NewObject_2 :: T_Instruction_2
                                               sem_Instruction_NewObject_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NewObject argCount_
                                                                {-# LINE 8553 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8558 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8563 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NewObject_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NewObject_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8578 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8583 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NextName_1 :: T_Instruction_1
                       sem_Instruction_NextName_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8593 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8598 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8603 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NextName_2 :: T_Instruction_2
                                               sem_Instruction_NextName_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NextName
                                                                {-# LINE 8611 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8616 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8621 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NextName_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NextName_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8636 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8641 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_NextValue_1 :: T_Instruction_1
                       sem_Instruction_NextValue_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8651 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8656 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8661 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_NextValue_2 :: T_Instruction_2
                                               sem_Instruction_NextValue_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_NextValue
                                                                {-# LINE 8669 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8674 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8679 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_NextValue_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_NextValue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8694 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8699 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Nop_1 :: T_Instruction_1
                       sem_Instruction_Nop_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8709 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8714 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8719 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Nop_2 :: T_Instruction_2
                                               sem_Instruction_Nop_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Nop
                                                                {-# LINE 8727 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8732 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8737 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Nop_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Nop_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8752 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8757 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Not_1 :: T_Instruction_1
                       sem_Instruction_Not_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8767 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8772 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8777 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Not_2 :: T_Instruction_2
                                               sem_Instruction_Not_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Not
                                                                {-# LINE 8785 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8790 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8795 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Not_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Not_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8810 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8815 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Pop_1 :: T_Instruction_1
                       sem_Instruction_Pop_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8825 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8830 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8835 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Pop_2 :: T_Instruction_2
                                               sem_Instruction_Pop_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Pop
                                                                {-# LINE 8843 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8848 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8853 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Pop_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Pop_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8868 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8873 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PopScope_1 :: T_Instruction_1
                       sem_Instruction_PopScope_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8883 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8888 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8893 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PopScope_2 :: T_Instruction_2
                                               sem_Instruction_PopScope_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PopScope
                                                                {-# LINE 8901 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8906 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8911 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PopScope_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PopScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8927 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8932 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushByte_1 :: T_Instruction_1
                       sem_Instruction_PushByte_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 8942 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 8947 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 8952 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushByte_2 :: T_Instruction_2
                                               sem_Instruction_PushByte_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushByte val_
                                                                {-# LINE 8960 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 8965 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 8970 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushByte_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushByte_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 8986 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 8991 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushDouble_1 :: T_Instruction_1
                       sem_Instruction_PushDouble_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9001 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9006 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9011 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushDouble_2 :: T_Instruction_2
                                               sem_Instruction_PushDouble_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushDouble name_
                                                                {-# LINE 9019 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9024 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9029 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushDouble_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushDouble_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9044 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9049 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushFalse_1 :: T_Instruction_1
                       sem_Instruction_PushFalse_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9059 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9064 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9069 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushFalse_2 :: T_Instruction_2
                                               sem_Instruction_PushFalse_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushFalse
                                                                {-# LINE 9077 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9082 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9087 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushFalse_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushFalse_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9103 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9108 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushInt_1 :: T_Instruction_1
                       sem_Instruction_PushInt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9118 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9123 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9128 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushInt_2 :: T_Instruction_2
                                               sem_Instruction_PushInt_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushInt name_
                                                                {-# LINE 9136 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9141 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9146 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushInt_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushInt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9162 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9167 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushNamespace_1 :: T_Instruction_1
                       sem_Instruction_PushNamespace_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9177 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9182 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9187 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushNamespace_2 :: T_Instruction_2
                                               sem_Instruction_PushNamespace_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushNamespace name_
                                                                {-# LINE 9195 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9200 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9205 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushNamespace_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushNamespace_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9220 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9225 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushNaN_1 :: T_Instruction_1
                       sem_Instruction_PushNaN_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9235 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9240 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9245 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushNaN_2 :: T_Instruction_2
                                               sem_Instruction_PushNaN_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushNaN
                                                                {-# LINE 9253 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9258 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9263 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushNaN_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushNaN_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9278 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9283 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushNull_1 :: T_Instruction_1
                       sem_Instruction_PushNull_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9293 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9298 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9303 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushNull_2 :: T_Instruction_2
                                               sem_Instruction_PushNull_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushNull
                                                                {-# LINE 9311 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9316 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9321 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushNull_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushNull_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9336 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9341 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushScope_1 :: T_Instruction_1
                       sem_Instruction_PushScope_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9351 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9356 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9361 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushScope_2 :: T_Instruction_2
                                               sem_Instruction_PushScope_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushScope
                                                                {-# LINE 9369 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9374 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9379 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushScope_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9395 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9400 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushShort_1 :: T_Instruction_1
                       sem_Instruction_PushShort_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9410 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9415 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9420 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushShort_2 :: T_Instruction_2
                                               sem_Instruction_PushShort_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushShort val_
                                                                {-# LINE 9428 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9433 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9438 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushShort_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushShort_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9454 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9459 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushString_1 :: T_Instruction_1
                       sem_Instruction_PushString_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9469 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9474 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9479 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushString_2 :: T_Instruction_2
                                               sem_Instruction_PushString_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushString name_
                                                                {-# LINE 9487 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9492 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9497 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushString_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushString_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9512 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9517 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushTrue_1 :: T_Instruction_1
                       sem_Instruction_PushTrue_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9527 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9532 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9537 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushTrue_2 :: T_Instruction_2
                                               sem_Instruction_PushTrue_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushTrue
                                                                {-# LINE 9545 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9550 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9555 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushTrue_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushTrue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9571 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9576 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushUInt_1 :: T_Instruction_1
                       sem_Instruction_PushUInt_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9586 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9591 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9596 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushUInt_2 :: T_Instruction_2
                                               sem_Instruction_PushUInt_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushUInt name_
                                                                {-# LINE 9604 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9609 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9614 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushUInt_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushUInt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9629 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9634 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushUndefined_1 :: T_Instruction_1
                       sem_Instruction_PushUndefined_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9644 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9649 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9654 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushUndefined_2 :: T_Instruction_2
                                               sem_Instruction_PushUndefined_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushUndefined
                                                                {-# LINE 9662 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9667 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9672 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushUndefined_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushUndefined_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9687 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9692 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_PushWith_1 :: T_Instruction_1
                       sem_Instruction_PushWith_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9702 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9707 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9712 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_PushWith_2 :: T_Instruction_2
                                               sem_Instruction_PushWith_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_PushWith
                                                                {-# LINE 9720 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9725 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9730 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_PushWith_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_PushWith_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9745 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9750 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_ReturnValue_1 :: T_Instruction_1
                       sem_Instruction_ReturnValue_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9760 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9765 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9770 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_ReturnValue_2 :: T_Instruction_2
                                               sem_Instruction_ReturnValue_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_ReturnValue
                                                                {-# LINE 9778 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9783 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9788 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_ReturnValue_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_ReturnValue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9803 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9808 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_ReturnVoid_1 :: T_Instruction_1
                       sem_Instruction_ReturnVoid_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9818 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9823 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9828 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_ReturnVoid_2 :: T_Instruction_2
                                               sem_Instruction_ReturnVoid_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_ReturnVoid
                                                                {-# LINE 9836 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9841 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9846 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_ReturnVoid_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_ReturnVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9861 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9866 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Rshift_1 :: T_Instruction_1
                       sem_Instruction_Rshift_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9876 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9881 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9886 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Rshift_2 :: T_Instruction_2
                                               sem_Instruction_Rshift_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Rshift
                                                                {-# LINE 9894 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9899 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9904 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Rshift_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Rshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9920 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9925 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetLocal_1 :: T_Instruction_1
                       sem_Instruction_SetLocal_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9935 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9940 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 9945 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetLocal_2 :: T_Instruction_2
                                               sem_Instruction_SetLocal_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetLocal reg_
                                                                {-# LINE 9953 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 9958 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 9963 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetLocal_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 9978 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 9983 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetLocal0_1 :: T_Instruction_1
                       sem_Instruction_SetLocal0_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 9993 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 9998 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10003 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetLocal0_2 :: T_Instruction_2
                                               sem_Instruction_SetLocal0_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetLocal0
                                                                {-# LINE 10011 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10016 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10021 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetLocal0_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetLocal0_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10036 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10041 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetLocal1_1 :: T_Instruction_1
                       sem_Instruction_SetLocal1_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10051 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10056 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10061 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetLocal1_2 :: T_Instruction_2
                                               sem_Instruction_SetLocal1_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetLocal1
                                                                {-# LINE 10069 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10074 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10079 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetLocal1_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetLocal1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10094 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10099 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetLocal2_1 :: T_Instruction_1
                       sem_Instruction_SetLocal2_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10109 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10114 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10119 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetLocal2_2 :: T_Instruction_2
                                               sem_Instruction_SetLocal2_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetLocal2
                                                                {-# LINE 10127 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10132 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10137 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetLocal2_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetLocal2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10152 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10157 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetLocal3_1 :: T_Instruction_1
                       sem_Instruction_SetLocal3_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10167 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10172 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10177 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetLocal3_2 :: T_Instruction_2
                                               sem_Instruction_SetLocal3_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetLocal3
                                                                {-# LINE 10185 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10190 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10195 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetLocal3_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetLocal3_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10211 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10216 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetGlobalSlot_1 :: T_Instruction_1
                       sem_Instruction_SetGlobalSlot_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10226 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10231 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10236 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetGlobalSlot_2 :: T_Instruction_2
                                               sem_Instruction_SetGlobalSlot_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetGlobalSlot slot_
                                                                {-# LINE 10244 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10249 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10254 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetGlobalSlot_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetGlobalSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10270 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10275 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetProperty_1 :: T_Instruction_1
                       sem_Instruction_SetProperty_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10285 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10290 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10295 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetProperty_2 :: T_Instruction_2
                                               sem_Instruction_SetProperty_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetProperty name_
                                                                {-# LINE 10303 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10308 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10313 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetProperty_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10328 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10333 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetPropertyLate_1 :: T_Instruction_1
                       sem_Instruction_SetPropertyLate_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10343 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10348 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10353 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetPropertyLate_2 :: T_Instruction_2
                                               sem_Instruction_SetPropertyLate_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetPropertyLate
                                                                {-# LINE 10361 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10366 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10371 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetPropertyLate_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetPropertyLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10387 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10392 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetSlot_1 :: T_Instruction_1
                       sem_Instruction_SetSlot_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10402 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10407 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10412 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetSlot_2 :: T_Instruction_2
                                               sem_Instruction_SetSlot_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetSlot slot_
                                                                {-# LINE 10420 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10425 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10430 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetSlot_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10446 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10451 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SetSuper_1 :: T_Instruction_1
                       sem_Instruction_SetSuper_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10461 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10466 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10471 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SetSuper_2 :: T_Instruction_2
                                               sem_Instruction_SetSuper_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SetSuper name_
                                                                {-# LINE 10479 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10484 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10489 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SetSuper_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SetSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10504 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10509 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SignExtend1_1 :: T_Instruction_1
                       sem_Instruction_SignExtend1_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10519 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10524 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10529 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SignExtend1_2 :: T_Instruction_2
                                               sem_Instruction_SignExtend1_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SignExtend1
                                                                {-# LINE 10537 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10542 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10547 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SignExtend1_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SignExtend1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10562 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10567 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SignExtend8_1 :: T_Instruction_1
                       sem_Instruction_SignExtend8_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10577 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10582 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10587 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SignExtend8_2 :: T_Instruction_2
                                               sem_Instruction_SignExtend8_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SignExtend8
                                                                {-# LINE 10595 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10600 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10605 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SignExtend8_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SignExtend8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10620 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10625 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_SignExtend16_1 :: T_Instruction_1
                       sem_Instruction_SignExtend16_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10635 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10640 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10645 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_SignExtend16_2 :: T_Instruction_2
                                               sem_Instruction_SignExtend16_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_SignExtend16
                                                                {-# LINE 10653 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10658 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10663 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_SignExtend16_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_SignExtend16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10678 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10683 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StoreFloat32_1 :: T_Instruction_1
                       sem_Instruction_StoreFloat32_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10693 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10698 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10703 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StoreFloat32_2 :: T_Instruction_2
                                               sem_Instruction_StoreFloat32_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StoreFloat32
                                                                {-# LINE 10711 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10716 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10721 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StoreFloat32_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StoreFloat32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10736 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10741 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StoreFloat64_1 :: T_Instruction_1
                       sem_Instruction_StoreFloat64_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10751 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10756 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10761 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StoreFloat64_2 :: T_Instruction_2
                                               sem_Instruction_StoreFloat64_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StoreFloat64
                                                                {-# LINE 10769 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10774 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10779 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StoreFloat64_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StoreFloat64_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10794 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10799 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect32_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect32_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10809 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10814 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10819 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StoreIndirect32_2 :: T_Instruction_2
                                               sem_Instruction_StoreIndirect32_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StoreIndirect32
                                                                {-# LINE 10827 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10832 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10837 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StoreIndirect32_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StoreIndirect32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10852 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10857 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect16_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect16_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10867 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10872 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10877 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StoreIndirect16_2 :: T_Instruction_2
                                               sem_Instruction_StoreIndirect16_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StoreIndirect16
                                                                {-# LINE 10885 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10890 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10895 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StoreIndirect16_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StoreIndirect16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10910 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10915 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect8_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect8_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10925 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10930 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10935 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StoreIndirect8_2 :: T_Instruction_2
                                               sem_Instruction_StoreIndirect8_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StoreIndirect8
                                                                {-# LINE 10943 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 10948 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 10953 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StoreIndirect8_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StoreIndirect8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 10968 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 10973 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_StrictEquals_1 :: T_Instruction_1
                       sem_Instruction_StrictEquals_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 10983 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 10988 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 10993 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_StrictEquals_2 :: T_Instruction_2
                                               sem_Instruction_StrictEquals_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_StrictEquals
                                                                {-# LINE 11001 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11006 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11011 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_StrictEquals_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_StrictEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11026 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11031 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Substract_1 :: T_Instruction_1
                       sem_Instruction_Substract_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11041 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11046 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11051 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Substract_2 :: T_Instruction_2
                                               sem_Instruction_Substract_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Substract
                                                                {-# LINE 11059 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11064 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11069 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Substract_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Substract_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11084 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11089 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Substract_i_1 :: T_Instruction_1
                       sem_Instruction_Substract_i_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11099 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11104 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11109 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Substract_i_2 :: T_Instruction_2
                                               sem_Instruction_Substract_i_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Substract_i
                                                                {-# LINE 11117 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11122 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11127 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Substract_i_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Substract_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11142 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11147 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Swap_1 :: T_Instruction_1
                       sem_Instruction_Swap_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11157 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11162 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11167 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Swap_2 :: T_Instruction_2
                                               sem_Instruction_Swap_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Swap
                                                                {-# LINE 11175 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11180 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11185 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Swap_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Swap_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11200 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11205 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Throw_1 :: T_Instruction_1
                       sem_Instruction_Throw_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11215 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11220 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11225 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Throw_2 :: T_Instruction_2
                                               sem_Instruction_Throw_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Throw
                                                                {-# LINE 11233 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11238 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11243 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Throw_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Throw_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11258 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11263 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Timestamp_1 :: T_Instruction_1
                       sem_Instruction_Timestamp_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11273 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11278 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11283 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Timestamp_2 :: T_Instruction_2
                                               sem_Instruction_Timestamp_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Timestamp
                                                                {-# LINE 11291 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11296 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11301 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Timestamp_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Timestamp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11316 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11321 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_TypeOf_1 :: T_Instruction_1
                       sem_Instruction_TypeOf_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11331 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11336 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11341 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_TypeOf_2 :: T_Instruction_2
                                               sem_Instruction_TypeOf_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_TypeOf
                                                                {-# LINE 11349 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11354 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11359 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_TypeOf_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_TypeOf_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11374 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11379 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Urshift_1 :: T_Instruction_1
                       sem_Instruction_Urshift_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11389 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                         False
                                         {-# LINE 11394 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11399 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instruction_Urshift_2 :: T_Instruction_2
                                               sem_Instruction_Urshift_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                Instruction_Urshift
                                                                {-# LINE 11407 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11412 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          (case (({-# LINE 52 "src\\InstrLocFilter.ag" #-}
                                                                  False
                                                                  {-# LINE 11417 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _lhsOskip | _lhsOskip `seq` (True) ->
                                                           ( _lhsOoutput,_lhsOskip) }) }) }))
                                           in  sem_Instruction_Urshift_2)) of
                                    { ( sem_Instruction_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }))
                   in  sem_Instruction_Urshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 62 "src\\InstrLocFilter.ag" #-}
                 False
                 {-# LINE 11433 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 7 "src\\ByteCodeLocationInfo.ag" #-}
                  index_
                  {-# LINE 11438 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instruction_Location_1 :: T_Instruction_1
                       sem_Instruction_Location_1 =
                           (\ _lhsIisBranch
                              _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 81 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11448 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _locs_augmented_syn | _locs_augmented_syn `seq` (True) ->
                                 (case (({-# LINE 81 "src\\InstrLocFilter.ag" #-}
                                         if _lhsIisBranch || _lhsIrevIsBranch then (IntSet.insert index_) else id
                                         {-# LINE 11453 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _locs_augmented_f1 | _locs_augmented_f1 `seq` (True) ->
                                  (case (({-# LINE 81 "src\\InstrLocFilter.ag" #-}
                                          foldr ($) _locs_augmented_syn [_locs_augmented_f1]
                                          {-# LINE 11458 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                   (case (({-# LINE 63 "src\\InstrLocFilter.ag" #-}
                                           False
                                           {-# LINE 11463 "src/InstrLocFilter.hs" #-}
                                           )) of
                                    { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                    (case (({-# LINE 11 "src\\ByteCodeLocationInfo.ag" #-}
                                            index_
                                            {-# LINE 11468 "src/InstrLocFilter.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     (case ((let sem_Instruction_Location_2 :: T_Instruction_2
                                                 sem_Instruction_Location_2 =
                                                     (\ _lhsIretain ->
                                                          (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                  Instruction_Location index_
                                                                  {-# LINE 11476 "src/InstrLocFilter.hs" #-}
                                                                  )) of
                                                           { _output | _output `seq` (True) ->
                                                           (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                   _output
                                                                   {-# LINE 11481 "src/InstrLocFilter.hs" #-}
                                                                   )) of
                                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                            (case (({-# LINE 55 "src\\InstrLocFilter.ag" #-}
                                                                    not (IntSet.member index_ _lhsIretain)
                                                                    {-# LINE 11486 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _lhsOskip | _lhsOskip `seq` (True) ->
                                                             ( _lhsOoutput,_lhsOskip) }) }) }))
                                             in  sem_Instruction_Location_2)) of
                                      { ( sem_Instruction_2) | True ->
                                      ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instruction_2) }) }) }) }) }) }))
                   in  sem_Instruction_Location_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instruction_1) }) }) }))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = Bool ->
                      Int ->
                      ( Bool,Int,T_Instructions_1)
type T_Instructions_1 = Bool ->
                        Int ->
                        ( IntSet,Bool,Int,T_Instructions_2)
type T_Instructions_2 = IntSet ->
                        ( Instructions)
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIisBranch
       _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11519 "src/InstrLocFilter.hs" #-}
                 )) of
          { _hdOlocation | _hdOlocation `seq` (True) ->
          (case (hd_ _hdOlocation) of
           { ( _hdIisBranch,_hdIlocation,hd_1) | True ->
               (case (({-# LINE 59 "src\\InstrLocFilter.ag" #-}
                       _hdIisBranch
                       {-# LINE 11526 "src/InstrLocFilter.hs" #-}
                       )) of
                { _tlOisBranch | _tlOisBranch `seq` (True) ->
                (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                        _hdIlocation
                        {-# LINE 11531 "src/InstrLocFilter.hs" #-}
                        )) of
                 { _tlOlocation | _tlOlocation `seq` (True) ->
                 (case (tl_ _tlOisBranch _tlOlocation) of
                  { ( _tlIisBranch,_tlIlocation,tl_1) | True ->
                      (case (({-# LINE 59 "src\\InstrLocFilter.ag" #-}
                              _tlIisBranch
                              {-# LINE 11538 "src/InstrLocFilter.hs" #-}
                              )) of
                       { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
                       (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                               _tlIlocation
                               {-# LINE 11543 "src/InstrLocFilter.hs" #-}
                               )) of
                        { _lhsOlocation | _lhsOlocation `seq` (True) ->
                        (case ((let sem_Instructions_Cons_1 :: T_Instructions_1
                                    sem_Instructions_Cons_1 =
                                        (\ _lhsIrevIsBranch
                                           _lhsIrevLocation ->
                                             (case (({-# LINE 59 "src\\InstrLocFilter.ag" #-}
                                                     _lhsIisBranch
                                                     {-# LINE 11552 "src/InstrLocFilter.hs" #-}
                                                     )) of
                                              { _hdOisBranch | _hdOisBranch `seq` (True) ->
                                              (case (({-# LINE 69 "src\\InstrLocFilter.ag" #-}
                                                      _lhsIrevIsBranch
                                                      {-# LINE 11557 "src/InstrLocFilter.hs" #-}
                                                      )) of
                                               { _tlOrevIsBranch | _tlOrevIsBranch `seq` (True) ->
                                               (case (({-# LINE 13 "src\\ByteCodeLocationInfo.ag" #-}
                                                       _lhsIrevLocation
                                                       {-# LINE 11562 "src/InstrLocFilter.hs" #-}
                                                       )) of
                                                { _tlOrevLocation | _tlOrevLocation `seq` (True) ->
                                                (case (tl_1 _tlOrevIsBranch _tlOrevLocation) of
                                                 { ( _tlIlocs,_tlIrevIsBranch,_tlIrevLocation,tl_2) | True ->
                                                     (case (({-# LINE 70 "src\\InstrLocFilter.ag" #-}
                                                             _tlIrevIsBranch
                                                             {-# LINE 11569 "src/InstrLocFilter.hs" #-}
                                                             )) of
                                                      { _hdOrevIsBranch | _hdOrevIsBranch `seq` (True) ->
                                                      (case (({-# LINE 14 "src\\ByteCodeLocationInfo.ag" #-}
                                                              _tlIrevLocation
                                                              {-# LINE 11574 "src/InstrLocFilter.hs" #-}
                                                              )) of
                                                       { _hdOrevLocation | _hdOrevLocation `seq` (True) ->
                                                       (case (hd_1 _hdOisBranch _hdOrevIsBranch _hdOrevLocation) of
                                                        { ( _hdIlocs,_hdIrevIsBranch,_hdIrevLocation,hd_2) | True ->
                                                            (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                                                    _hdIlocs `mappend` _tlIlocs
                                                                    {-# LINE 11581 "src/InstrLocFilter.hs" #-}
                                                                    )) of
                                                             { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                                             (case (({-# LINE 71 "src\\InstrLocFilter.ag" #-}
                                                                     _hdIrevIsBranch
                                                                     {-# LINE 11586 "src/InstrLocFilter.hs" #-}
                                                                     )) of
                                                              { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                                              (case (({-# LINE 15 "src\\ByteCodeLocationInfo.ag" #-}
                                                                      _hdIrevLocation
                                                                      {-# LINE 11591 "src/InstrLocFilter.hs" #-}
                                                                      )) of
                                                               { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                               (case ((let sem_Instructions_Cons_2 :: T_Instructions_2
                                                                           sem_Instructions_Cons_2 =
                                                                               (\ _lhsIretain ->
                                                                                    (case (({-# LINE 45 "src\\InstrLocFilter.ag" #-}
                                                                                            _lhsIretain
                                                                                            {-# LINE 11599 "src/InstrLocFilter.hs" #-}
                                                                                            )) of
                                                                                     { _tlOretain | _tlOretain `seq` (True) ->
                                                                                     (case (({-# LINE 45 "src\\InstrLocFilter.ag" #-}
                                                                                             _lhsIretain
                                                                                             {-# LINE 11604 "src/InstrLocFilter.hs" #-}
                                                                                             )) of
                                                                                      { _hdOretain | _hdOretain `seq` (True) ->
                                                                                      (case (tl_2 _tlOretain) of
                                                                                       { ( _tlIoutput) | True ->
                                                                                           (case (hd_2 _hdOretain) of
                                                                                            { ( _hdIoutput,_hdIskip) | True ->
                                                                                                (case (({-# LINE 42 "src\\InstrLocFilter.ag" #-}
                                                                                                        if _hdIskip then _tlIoutput else _hdIoutput : _tlIoutput
                                                                                                        {-# LINE 11613 "src/InstrLocFilter.hs" #-}
                                                                                                        )) of
                                                                                                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                                                                 ( _lhsOoutput) }) }) }) }) }))
                                                                       in  sem_Instructions_Cons_2)) of
                                                                { ( sem_Instructions_2) | True ->
                                                                ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instructions_2) }) }) }) }) }) }) }) }) }) }) }))
                                in  sem_Instructions_Cons_1)) of
                         { ( sem_Instructions_1) | True ->
                         ( _lhsOisBranch,_lhsOlocation,sem_Instructions_1) }) }) }) }) }) }) }) }))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIisBranch
       _lhsIlocation ->
         (case (({-# LINE 59 "src\\InstrLocFilter.ag" #-}
                 _lhsIisBranch
                 {-# LINE 11629 "src/InstrLocFilter.hs" #-}
                 )) of
          { _lhsOisBranch | _lhsOisBranch `seq` (True) ->
          (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                  _lhsIlocation
                  {-# LINE 11634 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOlocation | _lhsOlocation `seq` (True) ->
           (case ((let sem_Instructions_Nil_1 :: T_Instructions_1
                       sem_Instructions_Nil_1 =
                           (\ _lhsIrevIsBranch
                              _lhsIrevLocation ->
                                (case (({-# LINE 74 "src\\InstrLocFilter.ag" #-}
                                        mempty
                                        {-# LINE 11643 "src/InstrLocFilter.hs" #-}
                                        )) of
                                 { _lhsOlocs | _lhsOlocs `seq` (True) ->
                                 (case (({-# LINE 59 "src\\InstrLocFilter.ag" #-}
                                         _lhsIrevIsBranch
                                         {-# LINE 11648 "src/InstrLocFilter.hs" #-}
                                         )) of
                                  { _lhsOrevIsBranch | _lhsOrevIsBranch `seq` (True) ->
                                  (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                          _lhsIrevLocation
                                          {-# LINE 11653 "src/InstrLocFilter.hs" #-}
                                          )) of
                                   { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                   (case ((let sem_Instructions_Nil_2 :: T_Instructions_2
                                               sem_Instructions_Nil_2 =
                                                   (\ _lhsIretain ->
                                                        (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                []
                                                                {-# LINE 11661 "src/InstrLocFilter.hs" #-}
                                                                )) of
                                                         { _output | _output `seq` (True) ->
                                                         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                                                                 _output
                                                                 {-# LINE 11666 "src/InstrLocFilter.hs" #-}
                                                                 )) of
                                                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                          ( _lhsOoutput) }) }))
                                           in  sem_Instructions_Nil_2)) of
                                    { ( sem_Instructions_2) | True ->
                                    ( _lhsOlocs,_lhsOrevIsBranch,_lhsOrevLocation,sem_Instructions_2) }) }) }) }))
                   in  sem_Instructions_Nil_1)) of
            { ( sem_Instructions_1) | True ->
            ( _lhsOisBranch,_lhsOlocation,sem_Instructions_1) }) }) }))
-- Interfaces --------------------------------------------------
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil list)
-- semantic domain
type T_Interfaces = ( )
sem_Interfaces_Cons :: Word32 ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons hd_ tl_ =
    ( )
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    ( )
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
type T_MethodInfo = ( )
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    ( )
-- MethodInfos -------------------------------------------------
-- cata
sem_MethodInfos :: MethodInfos ->
                   T_MethodInfos
sem_MethodInfos list =
    (Prelude.foldr sem_MethodInfos_Cons sem_MethodInfos_Nil (Prelude.map sem_MethodInfo list))
-- semantic domain
type T_MethodInfos = ( )
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    ( )
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    ( )
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
type T_MultinameInfo = ( )
sem_MultinameInfo_QName :: Word32 ->
                           Word32 ->
                           T_MultinameInfo
sem_MultinameInfo_QName namespace_ name_ =
    ( )
sem_MultinameInfo_QNameA :: Word32 ->
                            Word32 ->
                            T_MultinameInfo
sem_MultinameInfo_QNameA namespace_ name_ =
    ( )
sem_MultinameInfo_RTQName :: Word32 ->
                             T_MultinameInfo
sem_MultinameInfo_RTQName name_ =
    ( )
sem_MultinameInfo_RTQNameA :: Word32 ->
                              T_MultinameInfo
sem_MultinameInfo_RTQNameA name_ =
    ( )
sem_MultinameInfo_RTQNameL :: T_MultinameInfo
sem_MultinameInfo_RTQNameL =
    ( )
sem_MultinameInfo_RTQNameLA :: T_MultinameInfo
sem_MultinameInfo_RTQNameLA =
    ( )
sem_MultinameInfo_Multiname :: Word32 ->
                               Word32 ->
                               T_MultinameInfo
sem_MultinameInfo_Multiname name_ set_ =
    ( )
sem_MultinameInfo_MultinameA :: Word32 ->
                                Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameA name_ set_ =
    ( )
sem_MultinameInfo_MultinameL :: Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameL set_ =
    ( )
sem_MultinameInfo_MultinameLA :: Word32 ->
                                 T_MultinameInfo
sem_MultinameInfo_MultinameLA set_ =
    ( )
sem_MultinameInfo_Generic :: Word32 ->
                             T_ParamNames ->
                             T_MultinameInfo
sem_MultinameInfo_Generic name_ params_ =
    ( )
-- MultinameInfos ----------------------------------------------
-- cata
sem_MultinameInfos :: MultinameInfos ->
                      T_MultinameInfos
sem_MultinameInfos list =
    (Prelude.foldr sem_MultinameInfos_Cons sem_MultinameInfos_Nil (Prelude.map sem_MultinameInfo list))
-- semantic domain
type T_MultinameInfos = ( )
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    ( )
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    ( )
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
type T_NamespaceInfo = ( )
sem_NamespaceInfo_Info :: T_NamespaceKind ->
                          Word32 ->
                          T_NamespaceInfo
sem_NamespaceInfo_Info kind_ name_ =
    ( )
-- NamespaceInfos ----------------------------------------------
-- cata
sem_NamespaceInfos :: NamespaceInfos ->
                      T_NamespaceInfos
sem_NamespaceInfos list =
    (Prelude.foldr sem_NamespaceInfos_Cons sem_NamespaceInfos_Nil (Prelude.map sem_NamespaceInfo list))
-- semantic domain
type T_NamespaceInfos = ( )
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    ( )
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    ( )
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
type T_NamespaceNames = ( )
sem_NamespaceNames_Cons :: Word32 ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    ( )
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    ( )
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
type T_ParamNames = ( )
sem_ParamNames_Cons :: Word32 ->
                       T_ParamNames ->
                       T_ParamNames
sem_ParamNames_Cons hd_ tl_ =
    ( )
sem_ParamNames_Nil :: T_ParamNames
sem_ParamNames_Nil =
    ( )
-- ParamTypes --------------------------------------------------
-- cata
sem_ParamTypes :: ParamTypes ->
                  T_ParamTypes
sem_ParamTypes list =
    (Prelude.foldr sem_ParamTypes_Cons sem_ParamTypes_Nil list)
-- semantic domain
type T_ParamTypes = ( )
sem_ParamTypes_Cons :: Word32 ->
                       T_ParamTypes ->
                       T_ParamTypes
sem_ParamTypes_Cons hd_ tl_ =
    ( )
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    ( )
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = ( )
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    ( )
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    ( )
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = ( )
sem_PoolInfo_Info :: T_PoolInts ->
                     T_PoolUInts ->
                     T_PoolDoubles ->
                     T_PoolStrings ->
                     T_NamespaceInfos ->
                     T_SetInfos ->
                     T_MultinameInfos ->
                     T_PoolInfo
sem_PoolInfo_Info integers_ uintegers_ doubles_ strings_ namespaces_ namesets_ multinames_ =
    ( )
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = ( )
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    ( )
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    ( )
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = ( )
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    ( )
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    ( )
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = ( )
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    ( )
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    ( )
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
type T_SetInfo = ( )
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    ( )
-- SetInfos ----------------------------------------------------
-- cata
sem_SetInfos :: SetInfos ->
                T_SetInfos
sem_SetInfos list =
    (Prelude.foldr sem_SetInfos_Cons sem_SetInfos_Nil (Prelude.map sem_SetInfo list))
-- semantic domain
type T_SetInfos = ( )
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    ( )
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    ( )
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = ( )
sem_SwfFile_File :: Bool ->
                    Word8 ->
                    Word32 ->
                    T_Rect ->
                    Word16 ->
                    Word16 ->
                    T_Tags ->
                    T_SwfFile
sem_SwfFile_File compressed_ version_ length_ size_ rate_ count_ tags_ =
    ( )
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
type T_Tag = ( )
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    ( )
sem_Tag_FileAttributes :: Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          T_Tag
sem_Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_ =
    ( )
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    ( )
sem_Tag_End :: T_Tag
sem_Tag_End =
    ( )
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
type T_Tags = ( )
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    ( )
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    ( )
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
                   (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                           Trait_Trait name_ _dataIoutput _attrsIoutput _metaIoutput
                           {-# LINE 12714 "src/InstrLocFilter.hs" #-}
                           )) of
                    { _output | _output `seq` (True) ->
                    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                            _output
                            {-# LINE 12719 "src/InstrLocFilter.hs" #-}
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
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitAttr_Final
            {-# LINE 12739 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12744 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitAttr_Override
            {-# LINE 12752 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12757 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitAttr_Metadata
            {-# LINE 12765 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12770 "src/InstrLocFilter.hs" #-}
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
              (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 12792 "src/InstrLocFilter.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                       _output
                       {-# LINE 12797 "src/InstrLocFilter.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            []
            {-# LINE 12805 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12810 "src/InstrLocFilter.hs" #-}
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
         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                 TraitData_Slot slotId_ tp_ vindex_ _vkindIoutput
                 {-# LINE 12844 "src/InstrLocFilter.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                  _output
                  {-# LINE 12849 "src/InstrLocFilter.hs" #-}
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
         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                 TraitData_Const slotId_ tp_ vindex_ _vkindIoutput
                 {-# LINE 12863 "src/InstrLocFilter.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                  _output
                  {-# LINE 12868 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitData_Method dispId_ method_
            {-# LINE 12878 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12883 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitData_Getter dispId_ method_
            {-# LINE 12893 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12898 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitData_Setter dispId_ method_
            {-# LINE 12908 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12913 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitData_Function dispId_ method_
            {-# LINE 12923 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12928 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitData_Class slotId_ class_
            {-# LINE 12938 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12943 "src/InstrLocFilter.hs" #-}
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
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Slot
            {-# LINE 12971 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12976 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Method
            {-# LINE 12984 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 12989 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Getter
            {-# LINE 12997 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13002 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Setter
            {-# LINE 13010 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13015 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Class
            {-# LINE 13023 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13028 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Function
            {-# LINE 13036 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13041 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            TraitKind_Const
            {-# LINE 13049 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13054 "src/InstrLocFilter.hs" #-}
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
         (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                 (:) hd_ _tlIoutput
                 {-# LINE 13074 "src/InstrLocFilter.hs" #-}
                 )) of
          { _output | _output `seq` (True) ->
          (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                  _output
                  {-# LINE 13079 "src/InstrLocFilter.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOoutput) }) }) })
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            []
            {-# LINE 13087 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13092 "src/InstrLocFilter.hs" #-}
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
              (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                      (:) _hdIoutput _tlIoutput
                      {-# LINE 13114 "src/InstrLocFilter.hs" #-}
                      )) of
               { _output | _output `seq` (True) ->
               (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
                       _output
                       {-# LINE 13119 "src/InstrLocFilter.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }) })
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            []
            {-# LINE 13127 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13132 "src/InstrLocFilter.hs" #-}
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
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Int
            {-# LINE 13176 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13181 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_UInt
            {-# LINE 13189 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13194 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Double
            {-# LINE 13202 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13207 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Utf8
            {-# LINE 13215 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13220 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_True
            {-# LINE 13228 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13233 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_False
            {-# LINE 13241 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13246 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Null
            {-# LINE 13254 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13259 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Undefined
            {-# LINE 13267 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13272 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Namespace
            {-# LINE 13280 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13285 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Package
            {-# LINE 13293 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13298 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Internal
            {-# LINE 13306 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13311 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Protected
            {-# LINE 13319 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13324 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Explicit
            {-# LINE 13332 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13337 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Static
            {-# LINE 13345 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13350 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
            ValueKind_Private
            {-# LINE 13358 "src/InstrLocFilter.hs" #-}
            )) of
     { _output | _output `seq` (True) ->
     (case (({-# LINE 35 "src\\InstrLocFilter.ag" #-}
             _output
             {-# LINE 13363 "src/InstrLocFilter.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOoutput) }) })