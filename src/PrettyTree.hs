

-- UUAGC 0.9.52.1 (src/PrettyTree.ag)
module PrettyTree(ppSwf,ppAbc) where

{-# LINE 8 "src\\PrettyTree.ag" #-}

import Codec.Binary.UTF8.String
import Data.ByteString.Lazy(ByteString,unpack)
import Data.Word
import Data.Monoid
import ByteCode
import PrettyUtil hiding ((<>))
import Options
import SymView
import ProgInfo
{-# LINE 18 "src/PrettyTree.hs" #-}
{-# LINE 23 "src\\PrettyTree.ag" #-}

ppSwf :: Options -> [SymbolTables] -> SwfFile -> Doc
ppSwf opts tbls m = out where
  inh = Inh_SwfFile { opts_Inh_SwfFile = opts, tbls_Inh_SwfFile = tbls }
  sem = sem_SwfFile m
  syn = wrap_SwfFile sem inh
  out = output_Syn_SwfFile syn

ppAbc :: Options -> [SymbolTables] -> AbcFile -> Doc
ppAbc opts tbls m = out where
  inh = Inh_AbcFile { opts_Inh_AbcFile = opts, tbls_Inh_AbcFile = tbls }
  sem = sem_AbcFile m
  syn = wrap_AbcFile sem inh
  out = output_Syn_AbcFile syn
{-# LINE 34 "src/PrettyTree.hs" #-}

{-# LINE 58 "src\\PrettyTree.ag" #-}

bool :: Bool -> Doc
bool True  = text "yes"
bool False = text "no"

num :: (Num a, Show a) => a -> Doc
num x = text (show x)

str :: ByteString -> Doc
str = text . decode . unpack

props :: [(String, Doc)] -> Doc
props xs = hsep $ punctuate (text ",") [ (text k <> text ":") <+> v | (k,v) <- xs ]
{-# LINE 50 "src/PrettyTree.hs" #-}

{-# LINE 181 "src\\PrettyTree.ag" #-}

data PPInfo = Short | Verbose
{-# LINE 55 "src/PrettyTree.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = Options ->
                 ([SymbolTables]) ->
                 ( Doc,([SymbolTables]))
data Inh_AbcFile = Inh_AbcFile {opts_Inh_AbcFile :: !(Options),tbls_Inh_AbcFile :: !(([SymbolTables]))}
data Syn_AbcFile = Syn_AbcFile {output_Syn_AbcFile :: !(Doc),tbls_Syn_AbcFile :: !(([SymbolTables]))}
wrap_AbcFile :: T_AbcFile ->
                Inh_AbcFile ->
                Syn_AbcFile
wrap_AbcFile sem (Inh_AbcFile _lhsIopts _lhsItbls) =
    (let ( _lhsOoutput,_lhsOtbls) | True = sem _lhsIopts _lhsItbls
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
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 55 "src\\PrettyTree.ag" #-}
                 head _lhsItbls
                 {-# LINE 89 "src/PrettyTree.hs" #-}
                 )) of
          { _tbls | _tbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _tbls
                  {-# LINE 94 "src/PrettyTree.hs" #-}
                  )) of
           { _bodiesOtbls | _bodiesOtbls `seq` (True) ->
           (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                   _tbls
                   {-# LINE 99 "src/PrettyTree.hs" #-}
                   )) of
            { _classesOtbls | _classesOtbls `seq` (True) ->
            (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                    _tbls
                    {-# LINE 104 "src/PrettyTree.hs" #-}
                    )) of
             { _instancesOtbls | _instancesOtbls `seq` (True) ->
             (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                     _tbls
                     {-# LINE 109 "src/PrettyTree.hs" #-}
                     )) of
              { _constantPoolOtbls | _constantPoolOtbls `seq` (True) ->
              (case (({-# LINE 281 "src\\PrettyTree.ag" #-}
                      0
                      {-# LINE 114 "src/PrettyTree.hs" #-}
                      )) of
               { _classesOindex | _classesOindex `seq` (True) ->
               (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                       _lhsIopts
                       {-# LINE 119 "src/PrettyTree.hs" #-}
                       )) of
                { _bodiesOopts | _bodiesOopts `seq` (True) ->
                (case (bodies_ _bodiesOopts _bodiesOtbls) of
                 { ( _bodiesIlength,_bodiesIoutput) | True ->
                     (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                             _lhsIopts
                             {-# LINE 126 "src/PrettyTree.hs" #-}
                             )) of
                      { _classesOopts | _classesOopts `seq` (True) ->
                      (case (classes_ _classesOindex _classesOopts _classesOtbls) of
                       { ( _classesIlength,_classesIoutput) | True ->
                           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                   _lhsIopts
                                   {-# LINE 133 "src/PrettyTree.hs" #-}
                                   )) of
                            { _instancesOopts | _instancesOopts `seq` (True) ->
                            (case (({-# LINE 280 "src\\PrettyTree.ag" #-}
                                    0
                                    {-# LINE 138 "src/PrettyTree.hs" #-}
                                    )) of
                             { _instancesOindex | _instancesOindex `seq` (True) ->
                             (case (instances_ _instancesOindex _instancesOopts _instancesOtbls) of
                              { ( _instancesIoutput) | True ->
                                  (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                          _lhsIopts
                                          {-# LINE 145 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _constantPoolOopts | _constantPoolOopts `seq` (True) ->
                                   (case (constantPool_ _constantPoolOopts _constantPoolOtbls) of
                                    { ( _constantPoolIoutput) | True ->
                                        (case (({-# LINE 99 "src\\PrettyTree.ag" #-}
                                                text "ABC" <+> space <+> props
                                                  [ ("major version", num majorVersion_)
                                                  , ("minor version", num minorVersion_)
                                                  , ("classes", num _classesIlength)
                                                  , ("methods", num _bodiesIlength)
                                                  ]
                                                $+$ _constantPoolIoutput
                                                $+$ _classesIoutput
                                                $+$ _instancesIoutput
                                                $+$ _bodiesIoutput
                                                {-# LINE 161 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                         (case (({-# LINE 56 "src\\PrettyTree.ag" #-}
                                                 tail _lhsItbls
                                                 {-# LINE 166 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lhsOtbls | _lhsOtbls `seq` (True) ->
                                          ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- AbcFlag -----------------------------------------------------
-- cata
sem_AbcFlag :: AbcFlag ->
               T_AbcFlag
sem_AbcFlag (AbcFlag_LazyInit) =
    (sem_AbcFlag_LazyInit)
-- semantic domain
type T_AbcFlag = ( Bool)
sem_AbcFlag_LazyInit :: T_AbcFlag
sem_AbcFlag_LazyInit =
    (case (({-# LINE 95 "src\\PrettyTree.ag" #-}
            True
            {-# LINE 182 "src/PrettyTree.hs" #-}
            )) of
     { _lhsOdoLazyInit | _lhsOdoLazyInit `seq` (True) ->
     ( _lhsOdoLazyInit) })
-- AbcFlags ----------------------------------------------------
-- cata
sem_AbcFlags :: AbcFlags ->
                T_AbcFlags
sem_AbcFlags list =
    (Prelude.foldr sem_AbcFlags_Cons sem_AbcFlags_Nil (Prelude.map sem_AbcFlag list))
-- semantic domain
type T_AbcFlags = ( Bool)
sem_AbcFlags_Cons :: T_AbcFlag ->
                     T_AbcFlags ->
                     T_AbcFlags
sem_AbcFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIdoLazyInit) | True ->
         (case (hd_) of
          { ( _hdIdoLazyInit) | True ->
              (case (({-# LINE 94 "src\\PrettyTree.ag" #-}
                      _hdIdoLazyInit || _tlIdoLazyInit
                      {-# LINE 204 "src/PrettyTree.hs" #-}
                      )) of
               { _lhsOdoLazyInit | _lhsOdoLazyInit `seq` (True) ->
               ( _lhsOdoLazyInit) }) }) })
sem_AbcFlags_Nil :: T_AbcFlags
sem_AbcFlags_Nil =
    (case (({-# LINE 94 "src\\PrettyTree.ag" #-}
            False
            {-# LINE 212 "src/PrettyTree.hs" #-}
            )) of
     { _lhsOdoLazyInit | _lhsOdoLazyInit `seq` (True) ->
     ( _lhsOdoLazyInit) })
-- BodyInfo ----------------------------------------------------
-- cata
sem_BodyInfo :: BodyInfo ->
                T_BodyInfo
sem_BodyInfo (BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth _instructions _exceptions _traits) =
    (sem_BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth (sem_Instructions _instructions) (sem_Exceptions _exceptions) (sem_Traits _traits))
-- semantic domain
type T_BodyInfo = Options ->
                  SymbolTables ->
                  ( Int,Doc)
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
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 113 "src\\PrettyTree.ag" #-}
                 1
                 {-# LINE 240 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlength | _lhsOlength `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 245 "src/PrettyTree.hs" #-}
                  )) of
           { _exceptionsOtbls | _exceptionsOtbls `seq` (True) ->
           (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                   _lhsItbls
                   {-# LINE 250 "src/PrettyTree.hs" #-}
                   )) of
            { _instructionsOtbls | _instructionsOtbls `seq` (True) ->
            (case (({-# LINE 455 "src\\PrettyTree.ag" #-}
                    0
                    {-# LINE 255 "src/PrettyTree.hs" #-}
                    )) of
             { _exceptionsOindex | _exceptionsOindex `seq` (True) ->
             (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                     _lhsIopts
                     {-# LINE 260 "src/PrettyTree.hs" #-}
                     )) of
              { _exceptionsOopts | _exceptionsOopts `seq` (True) ->
              (case (exceptions_ _exceptionsOindex _exceptionsOopts _exceptionsOtbls) of
               { ( _exceptionsIgathExcptEnv,_exceptionsIoutput) | True ->
                   (case (({-# LINE 466 "src\\PrettyTree.ag" #-}
                           _exceptionsIgathExcptEnv
                           {-# LINE 267 "src/PrettyTree.hs" #-}
                           )) of
                    { _instructionsOexcptEnv | _instructionsOexcptEnv `seq` (True) ->
                    (case (({-# LINE 436 "src\\PrettyTree.ag" #-}
                            Nothing
                            {-# LINE 272 "src/PrettyTree.hs" #-}
                            )) of
                     { _instructionsOmbLoc | _instructionsOmbLoc `seq` (True) ->
                     (case (({-# LINE 405 "src\\PrettyTree.ag" #-}
                             methodView _lhsItbls (Ref method_)
                             {-# LINE 277 "src/PrettyTree.hs" #-}
                             )) of
                      { m_val_ | m_val_ `seq` (True) ->
                      (case ((sem_MethodV m_val_)) of
                       { m_inst_ | m_inst_ `seq` (True) ->
                       (case (({-# LINE 2 "src\\ByteCodeLocationInfo.ag" #-}
                               0
                               {-# LINE 284 "src/PrettyTree.hs" #-}
                               )) of
                        { _instructionsOlocation | _instructionsOlocation `seq` (True) ->
                        (case (instructions_ _instructionsOlocation _instructionsOmbLoc) of
                         { ( _instructionsIlocation,_instructionsImbLoc,instructions_1) | True ->
                             (case (({-# LINE 3 "src\\ByteCodeLocationInfo.ag" #-}
                                     0
                                     {-# LINE 291 "src/PrettyTree.hs" #-}
                                     )) of
                              { _instructionsOrevLocation | _instructionsOrevLocation `seq` (True) ->
                              (case (({-# LINE 406 "src\\PrettyTree.ag" #-}
                                      Short
                                      {-# LINE 296 "src/PrettyTree.hs" #-}
                                      )) of
                               { _mOinfo | _mOinfo `seq` (True) ->
                               (case (m_inst_ _mOinfo) of
                                { ( _mIoutput) | True ->
                                    (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                            _lhsIopts
                                            {-# LINE 303 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _instructionsOopts | _instructionsOopts `seq` (True) ->
                                     (case (instructions_1 _instructionsOexcptEnv _instructionsOopts _instructionsOrevLocation _instructionsOtbls) of
                                      { ( _instructionsIoutput,_instructionsIrevLocation) | True ->
                                          (case (({-# LINE 408 "src\\PrettyTree.ag" #-}
                                                  text "METHOD" <+> _mIoutput <+> space <+> props
                                                    [ ("max stack",   num maxStack_)
                                                    , ("locals",      num localCount_)
                                                    , ("init scopes", num initScopeDepth_)
                                                    , ("max scopes",  num maxScopeDepth_)
                                                    ]
                                                  $+$ nest 2 _instructionsIoutput
                                                  $+$ nest 2 _exceptionsIoutput
                                                  {-# LINE 317 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                           ( _lhsOlength,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- BodyInfos ---------------------------------------------------
-- cata
sem_BodyInfos :: BodyInfos ->
                 T_BodyInfos
sem_BodyInfos list =
    (Prelude.foldr sem_BodyInfos_Cons sem_BodyInfos_Nil (Prelude.map sem_BodyInfo list))
-- semantic domain
type T_BodyInfos = Options ->
                   SymbolTables ->
                   ( Int,Doc)
sem_BodyInfos_Cons :: T_BodyInfo ->
                      T_BodyInfos ->
                      T_BodyInfos
sem_BodyInfos_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 339 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 344 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIlength,_tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 351 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 356 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIlength,_hdIoutput) | True ->
                       (case (({-# LINE 110 "src\\PrettyTree.ag" #-}
                               _hdIlength + _tlIlength
                               {-# LINE 363 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOlength | _lhsOlength `seq` (True) ->
                        (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                _hdIoutput $+$ _tlIoutput
                                {-# LINE 368 "src/PrettyTree.hs" #-}
                                )) of
                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                         ( _lhsOlength,_lhsOoutput) }) }) }) }) }) }) }) }))
sem_BodyInfos_Nil :: T_BodyInfos
sem_BodyInfos_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 110 "src\\PrettyTree.ag" #-}
                 0
                 {-# LINE 378 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlength | _lhsOlength `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  empty
                  {-# LINE 383 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOlength,_lhsOoutput) }) }))
-- CaseOffsets -------------------------------------------------
-- cata
sem_CaseOffsets :: CaseOffsets ->
                   T_CaseOffsets
sem_CaseOffsets list =
    (Prelude.foldr sem_CaseOffsets_Cons sem_CaseOffsets_Nil list)
-- semantic domain
type T_CaseOffsets = Int ->
                     ( Int,T_CaseOffsets_1)
type T_CaseOffsets_1 = Options ->
                       Int ->
                       SymbolTables ->
                       ( Doc,Int)
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 407 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOlocation | _tlOlocation `seq` (True) ->
          (case (tl_ _tlOlocation) of
           { ( _tlIlocation,tl_1) | True ->
               (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                       _tlIlocation
                       {-# LINE 414 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOlocation | _lhsOlocation `seq` (True) ->
                (case ((let sem_CaseOffsets_Cons_1 :: T_CaseOffsets_1
                            sem_CaseOffsets_Cons_1 =
                                (\ _lhsIopts
                                   _lhsIrevLocation
                                   _lhsItbls ->
                                     (case (({-# LINE 30 "src\\ByteCodeLocationInfo.ag" #-}
                                             fromS24 hd_
                                             {-# LINE 424 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _relative | _relative `seq` (True) ->
                                      (case (({-# LINE 31 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIlocation + _relative
                                              {-# LINE 429 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _target | _target `seq` (True) ->
                                       (case (({-# LINE 691 "src\\PrettyTree.ag" #-}
                                               text "CASE" <+> num _target     <+> parens (num _relative    )
                                               {-# LINE 434 "src/PrettyTree.hs" #-}
                                               )) of
                                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                        (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                _lhsIrevLocation
                                                {-# LINE 439 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _tlOrevLocation | _tlOrevLocation `seq` (True) ->
                                         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                 _lhsItbls
                                                 {-# LINE 444 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _tlOtbls | _tlOtbls `seq` (True) ->
                                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                  _lhsIopts
                                                  {-# LINE 449 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _tlOopts | _tlOopts `seq` (True) ->
                                           (case (tl_1 _tlOopts _tlOrevLocation _tlOtbls) of
                                            { ( _tlIoutput,_tlIrevLocation) | True ->
                                                (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                        _tlIrevLocation
                                                        {-# LINE 456 "src/PrettyTree.hs" #-}
                                                        )) of
                                                 { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                 ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }))
                        in  sem_CaseOffsets_Cons_1)) of
                 { ( sem_CaseOffsets_1) | True ->
                 ( _lhsOlocation,sem_CaseOffsets_1) }) }) }) }))
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 468 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case ((let sem_CaseOffsets_Nil_1 :: T_CaseOffsets_1
                      sem_CaseOffsets_Nil_1 =
                          (\ _lhsIopts
                             _lhsIrevLocation
                             _lhsItbls ->
                               (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                       empty
                                       {-# LINE 478 "src/PrettyTree.hs" #-}
                                       )) of
                                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                        _lhsIrevLocation
                                        {-# LINE 483 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                 ( _lhsOoutput,_lhsOrevLocation) }) }))
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
type T_ClassInfo = Word32 ->
                   Options ->
                   SymbolTables ->
                   ( Int,Doc)
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 112 "src\\PrettyTree.ag" #-}
                 1
                 {-# LINE 510 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlength | _lhsOlength `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 515 "src/PrettyTree.hs" #-}
                  )) of
           { _traitsOtbls | _traitsOtbls `seq` (True) ->
           (case (({-# LINE 391 "src\\PrettyTree.ag" #-}
                   methodView _lhsItbls (Ref con_)
                   {-# LINE 520 "src/PrettyTree.hs" #-}
                   )) of
            { m_val_ | m_val_ `seq` (True) ->
            (case ((sem_MethodV m_val_)) of
             { m_inst_ | m_inst_ `seq` (True) ->
             (case (({-# LINE 383 "src\\PrettyTree.ag" #-}
                     Ref _lhsIindex
                     {-# LINE 527 "src/PrettyTree.hs" #-}
                     )) of
              { _key | _key `seq` (True) ->
              (case (({-# LINE 384 "src\\PrettyTree.ag" #-}
                      lookupClass _key     _lhsItbls
                      {-# LINE 532 "src/PrettyTree.hs" #-}
                      )) of
               { _descr | _descr `seq` (True) ->
               (case (({-# LINE 387 "src\\PrettyTree.ag" #-}
                       nameView _lhsItbls (clName _descr    )
                       {-# LINE 537 "src/PrettyTree.hs" #-}
                       )) of
                { nm_val_ | nm_val_ `seq` (True) ->
                (case ((sem_NmV nm_val_)) of
                 { nm_inst_ | nm_inst_ `seq` (True) ->
                 (case (({-# LINE 388 "src\\PrettyTree.ag" #-}
                         Short
                         {-# LINE 544 "src/PrettyTree.hs" #-}
                         )) of
                  { _nmOinfo | _nmOinfo `seq` (True) ->
                  (case (({-# LINE 392 "src\\PrettyTree.ag" #-}
                          Short
                          {-# LINE 549 "src/PrettyTree.hs" #-}
                          )) of
                   { _mOinfo | _mOinfo `seq` (True) ->
                   (case (m_inst_ _mOinfo) of
                    { ( _mIoutput) | True ->
                        (case (nm_inst_ _nmOinfo) of
                         { ( _nmIisEmpty,_nmIoutput) | True ->
                             (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                     _lhsIopts
                                     {-# LINE 558 "src/PrettyTree.hs" #-}
                                     )) of
                              { _traitsOopts | _traitsOopts `seq` (True) ->
                              (case (traits_ _traitsOopts _traitsOtbls) of
                               { ( _traitsIoutput) | True ->
                                   (case (({-# LINE 394 "src\\PrettyTree.ag" #-}
                                           text "CLASS" <+> _nmIoutput
                                           $+$ nest 2 (text "CON" <+> _mIoutput)
                                           $+$ nest 2 _traitsIoutput
                                           {-# LINE 567 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    ( _lhsOlength,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = Word32 ->
                    Options ->
                    SymbolTables ->
                    ( Int,Doc)
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 591 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 596 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (({-# LINE 283 "src\\PrettyTree.ag" #-}
                   1 + _lhsIindex
                   {-# LINE 601 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOindex | _tlOindex `seq` (True) ->
            (case (tl_ _tlOindex _tlOopts _tlOtbls) of
             { ( _tlIlength,_tlIoutput) | True ->
                 (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                         _lhsItbls
                         {-# LINE 608 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOtbls | _hdOtbls `seq` (True) ->
                  (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                          _lhsIopts
                          {-# LINE 613 "src/PrettyTree.hs" #-}
                          )) of
                   { _hdOopts | _hdOopts `seq` (True) ->
                   (case (({-# LINE 277 "src\\PrettyTree.ag" #-}
                           _lhsIindex
                           {-# LINE 618 "src/PrettyTree.hs" #-}
                           )) of
                    { _hdOindex | _hdOindex `seq` (True) ->
                    (case (hd_ _hdOindex _hdOopts _hdOtbls) of
                     { ( _hdIlength,_hdIoutput) | True ->
                         (case (({-# LINE 110 "src\\PrettyTree.ag" #-}
                                 _hdIlength + _tlIlength
                                 {-# LINE 625 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOlength | _lhsOlength `seq` (True) ->
                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                  _hdIoutput $+$ _tlIoutput
                                  {-# LINE 630 "src/PrettyTree.hs" #-}
                                  )) of
                           { _lhsOoutput | _lhsOoutput `seq` (True) ->
                           ( _lhsOlength,_lhsOoutput) }) }) }) }) }) }) }) }) }) }))
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 110 "src\\PrettyTree.ag" #-}
                 0
                 {-# LINE 641 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlength | _lhsOlength `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  empty
                  {-# LINE 646 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOlength,_lhsOoutput) }) }))
-- ClassV ------------------------------------------------------
-- cata
sem_ClassV :: ClassV ->
              T_ClassV
sem_ClassV (ClassV_Class _ref _nm _mbSuper _itfs _dynTraits _staTraits) =
    (sem_ClassV_Class _ref (sem_NmV _nm) (sem_MbSuperV _mbSuper) (sem_ItfsV _itfs) (sem_TraitsV _dynTraits) (sem_TraitsV _staTraits))
-- semantic domain
type T_ClassV = PPInfo ->
                ( Doc)
sem_ClassV_Class :: ClassRef ->
                    T_NmV ->
                    T_MbSuperV ->
                    T_ItfsV ->
                    T_TraitsV ->
                    T_TraitsV ->
                    T_ClassV
sem_ClassV_Class ref_ nm_ mbSuper_ itfs_ dynTraits_ staTraits_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 670 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (nm_ _nmOinfo) of
           { ( _nmIisEmpty,_nmIoutput) | True ->
               (case (({-# LINE 269 "src\\PrettyTree.ag" #-}
                       _nmIoutput
                       {-# LINE 677 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- ClassesV ----------------------------------------------------
-- cata
sem_ClassesV :: ClassesV ->
                T_ClassesV
sem_ClassesV list =
    (Prelude.foldr sem_ClassesV_Cons sem_ClassesV_Nil (Prelude.map sem_ClassV list))
-- semantic domain
type T_ClassesV = PPInfo ->
                  ( Doc)
sem_ClassesV_Cons :: T_ClassV ->
                     T_ClassesV ->
                     T_ClassesV
sem_ClassesV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 697 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 702 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 711 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_ClassesV_Nil :: T_ClassesV
sem_ClassesV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 720 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- DebugType ---------------------------------------------------
-- cata
sem_DebugType :: DebugType ->
                 T_DebugType
sem_DebugType (DebugType_Local) =
    (sem_DebugType_Local)
-- semantic domain
type T_DebugType = Options ->
                   SymbolTables ->
                   ( Doc)
sem_DebugType_Local :: T_DebugType
sem_DebugType_Local =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 740 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- Exception ---------------------------------------------------
-- cata
sem_Exception :: Exception ->
                 T_Exception
sem_Exception (Exception_Info _from _to _target _tp _name) =
    (sem_Exception_Info _from _to _target _tp _name)
-- semantic domain
type T_Exception = Word32 ->
                   Options ->
                   SymbolTables ->
                   ( ExceptionDescrs,Doc)
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 458 "src\\PrettyTree.ag" #-}
                 singleEnv _lhsIindex ExceptionDescr
                   { expFrom    = from_
                   , expTo      = to_
                   , expTarget  = target_
                   , expTp      = Ref tp_
                   , expName    = Ref name_
                   }
                 {-# LINE 773 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOgathExcptEnv | _lhsOgathExcptEnv `seq` (True) ->
          (case (({-# LINE 419 "src\\PrettyTree.ag" #-}
                  nameView _lhsItbls (Ref tp_)
                  {-# LINE 778 "src/PrettyTree.hs" #-}
                  )) of
           { nm_val_ | nm_val_ `seq` (True) ->
           (case ((sem_NmV nm_val_)) of
            { nm_inst_ | nm_inst_ `seq` (True) ->
            (case (({-# LINE 420 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 785 "src/PrettyTree.hs" #-}
                    )) of
             { _nmOinfo | _nmOinfo `seq` (True) ->
             (case (nm_inst_ _nmOinfo) of
              { ( _nmIisEmpty,_nmIoutput) | True ->
                  (case (({-# LINE 422 "src\\PrettyTree.ag" #-}
                          text "EXCEPTION" <+> _nmIoutput <+> space <+> props
                            [ ("from",   num from_)
                            , ("to",     num to_)
                            , ("target", num target_)
                            ]
                          {-# LINE 796 "src/PrettyTree.hs" #-}
                          )) of
                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                   ( _lhsOgathExcptEnv,_lhsOoutput) }) }) }) }) }) }))
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = Word32 ->
                    Options ->
                    SymbolTables ->
                    ( ExceptionDescrs,Doc)
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 454 "src\\PrettyTree.ag" #-}
                 _lhsIindex
                 {-# LINE 820 "src/PrettyTree.hs" #-}
                 )) of
          { _hdOindex | _hdOindex `seq` (True) ->
          (case (({-# LINE 456 "src\\PrettyTree.ag" #-}
                  1 + _lhsIindex
                  {-# LINE 825 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOindex | _tlOindex `seq` (True) ->
           (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                   _lhsItbls
                   {-# LINE 830 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOtbls | _tlOtbls `seq` (True) ->
            (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                    _lhsIopts
                    {-# LINE 835 "src/PrettyTree.hs" #-}
                    )) of
             { _tlOopts | _tlOopts `seq` (True) ->
             (case (tl_ _tlOindex _tlOopts _tlOtbls) of
              { ( _tlIgathExcptEnv,_tlIoutput) | True ->
                  (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                          _lhsItbls
                          {-# LINE 842 "src/PrettyTree.hs" #-}
                          )) of
                   { _hdOtbls | _hdOtbls `seq` (True) ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 847 "src/PrettyTree.hs" #-}
                           )) of
                    { _hdOopts | _hdOopts `seq` (True) ->
                    (case (hd_ _hdOindex _hdOopts _hdOtbls) of
                     { ( _hdIgathExcptEnv,_hdIoutput) | True ->
                         (case (({-# LINE 454 "src\\PrettyTree.ag" #-}
                                 _hdIgathExcptEnv `mappend` _tlIgathExcptEnv
                                 {-# LINE 854 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOgathExcptEnv | _lhsOgathExcptEnv `seq` (True) ->
                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                  _hdIoutput $+$ _tlIoutput
                                  {-# LINE 859 "src/PrettyTree.hs" #-}
                                  )) of
                           { _lhsOoutput | _lhsOoutput `seq` (True) ->
                           ( _lhsOgathExcptEnv,_lhsOoutput) }) }) }) }) }) }) }) }) }) }))
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 454 "src\\PrettyTree.ag" #-}
                 mempty
                 {-# LINE 870 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOgathExcptEnv | _lhsOgathExcptEnv `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  empty
                  {-# LINE 875 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOgathExcptEnv,_lhsOoutput) }) }))
-- FileV -------------------------------------------------------
-- cata
sem_FileV :: FileV ->
             T_FileV
sem_FileV (FileV_File _tables) =
    (sem_FileV_File (sem_TablesV _tables))
-- semantic domain
type T_FileV = PPInfo ->
               ( Doc)
sem_FileV_File :: T_TablesV ->
                  T_FileV
sem_FileV_File tables_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 894 "src/PrettyTree.hs" #-}
                 )) of
          { _tablesOinfo | _tablesOinfo `seq` (True) ->
          (case (tables_ _tablesOinfo) of
           { ( _tablesIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _tablesIoutput
                       {-# LINE 901 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
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
type T_InstanceFlag = Options ->
                      SymbolTables ->
                      ( Doc)
sem_InstanceFlag_ClassSealed :: T_InstanceFlag
sem_InstanceFlag_ClassSealed =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 313 "src\\PrettyTree.ag" #-}
                 text "FLAG sealed"
                 {-# LINE 927 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_InstanceFlag_ClassFinal :: T_InstanceFlag
sem_InstanceFlag_ClassFinal =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 314 "src\\PrettyTree.ag" #-}
                 text "FLAG final"
                 {-# LINE 937 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_InstanceFlag_ClassInterface :: T_InstanceFlag
sem_InstanceFlag_ClassInterface =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 315 "src\\PrettyTree.ag" #-}
                 text "FLAG is interface"
                 {-# LINE 947 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_InstanceFlag_ClassProtected :: T_InstanceFlag
sem_InstanceFlag_ClassProtected =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 316 "src\\PrettyTree.ag" #-}
                 text "FLAG has protected namespace"
                 {-# LINE 957 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- InstanceFlags -----------------------------------------------
-- cata
sem_InstanceFlags :: InstanceFlags ->
                     T_InstanceFlags
sem_InstanceFlags list =
    (Prelude.foldr sem_InstanceFlags_Cons sem_InstanceFlags_Nil (Prelude.map sem_InstanceFlag list))
-- semantic domain
type T_InstanceFlags = Options ->
                       SymbolTables ->
                       ( Doc)
sem_InstanceFlags_Cons :: T_InstanceFlag ->
                          T_InstanceFlags ->
                          T_InstanceFlags
sem_InstanceFlags_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 979 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 984 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 991 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 996 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 1003 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_InstanceFlags_Nil :: T_InstanceFlags
sem_InstanceFlags_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 1013 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- InstanceInfo ------------------------------------------------
-- cata
sem_InstanceInfo :: InstanceInfo ->
                    T_InstanceInfo
sem_InstanceInfo (InstanceInfo_Info _name _super _flags _protectedNs _interfaces _constructor _traits) =
    (sem_InstanceInfo_Info _name _super (sem_InstanceFlags _flags) _protectedNs (sem_Interfaces _interfaces) _constructor (sem_Traits _traits))
-- semantic domain
type T_InstanceInfo = Word32 ->
                      Options ->
                      SymbolTables ->
                      ( Doc)
sem_InstanceInfo_Info :: Word32 ->
                         Word32 ->
                         T_InstanceFlags ->
                         Word32 ->
                         T_Interfaces ->
                         Word32 ->
                         T_Traits ->
                         T_InstanceInfo
sem_InstanceInfo_Info name_ super_ flags_ protectedNs_ interfaces_ constructor_ traits_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 1042 "src/PrettyTree.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (({-# LINE 300 "src\\PrettyTree.ag" #-}
                  namespaceView _lhsItbls (Ref protectedNs_)
                  {-# LINE 1047 "src/PrettyTree.hs" #-}
                  )) of
           { ns_val_ | ns_val_ `seq` (True) ->
           (case ((sem_NsV ns_val_)) of
            { ns_inst_ | ns_inst_ `seq` (True) ->
            (case (({-# LINE 301 "src\\PrettyTree.ag" #-}
                    Verbose
                    {-# LINE 1054 "src/PrettyTree.hs" #-}
                    )) of
             { _nsOinfo | _nsOinfo `seq` (True) ->
             (case (({-# LINE 296 "src\\PrettyTree.ag" #-}
                     methodView _lhsItbls (Ref constructor_)
                     {-# LINE 1059 "src/PrettyTree.hs" #-}
                     )) of
              { con_val_ | con_val_ `seq` (True) ->
              (case ((sem_MethodV con_val_)) of
               { con_inst_ | con_inst_ `seq` (True) ->
               (case (({-# LINE 292 "src\\PrettyTree.ag" #-}
                       nameView _lhsItbls (Ref super_)
                       {-# LINE 1066 "src/PrettyTree.hs" #-}
                       )) of
                { s_val_ | s_val_ `seq` (True) ->
                (case ((sem_NmV s_val_)) of
                 { s_inst_ | s_inst_ `seq` (True) ->
                 (case (({-# LINE 293 "src\\PrettyTree.ag" #-}
                         Short
                         {-# LINE 1073 "src/PrettyTree.hs" #-}
                         )) of
                  { _sOinfo | _sOinfo `seq` (True) ->
                  (case (({-# LINE 288 "src\\PrettyTree.ag" #-}
                          nameView _lhsItbls (Ref name_)
                          {-# LINE 1078 "src/PrettyTree.hs" #-}
                          )) of
                   { nm_val_ | nm_val_ `seq` (True) ->
                   (case ((sem_NmV nm_val_)) of
                    { nm_inst_ | nm_inst_ `seq` (True) ->
                    (case (({-# LINE 289 "src\\PrettyTree.ag" #-}
                            Short
                            {-# LINE 1085 "src/PrettyTree.hs" #-}
                            )) of
                     { _nmOinfo | _nmOinfo `seq` (True) ->
                     (case (ns_inst_ _nsOinfo) of
                      { ( _nsIisEmpty,_nsIoutput) | True ->
                          (case (({-# LINE 297 "src\\PrettyTree.ag" #-}
                                  Short
                                  {-# LINE 1092 "src/PrettyTree.hs" #-}
                                  )) of
                           { _conOinfo | _conOinfo `seq` (True) ->
                           (case (con_inst_ _conOinfo) of
                            { ( _conIoutput) | True ->
                                (case (s_inst_ _sOinfo) of
                                 { ( _sIisEmpty,_sIoutput) | True ->
                                     (case (nm_inst_ _nmOinfo) of
                                      { ( _nmIisEmpty,_nmIoutput) | True ->
                                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                  _lhsIopts
                                                  {-# LINE 1103 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _traitsOopts | _traitsOopts `seq` (True) ->
                                           (case (traits_ _traitsOopts _traitsOtbls) of
                                            { ( _traitsIoutput) | True ->
                                                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                        _lhsItbls
                                                        {-# LINE 1110 "src/PrettyTree.hs" #-}
                                                        )) of
                                                 { _interfacesOtbls | _interfacesOtbls `seq` (True) ->
                                                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                         _lhsIopts
                                                         {-# LINE 1115 "src/PrettyTree.hs" #-}
                                                         )) of
                                                  { _interfacesOopts | _interfacesOopts `seq` (True) ->
                                                  (case (interfaces_ _interfacesOopts _interfacesOtbls) of
                                                   { ( _interfacesIoutput) | True ->
                                                       (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                               _lhsItbls
                                                               {-# LINE 1122 "src/PrettyTree.hs" #-}
                                                               )) of
                                                        { _flagsOtbls | _flagsOtbls `seq` (True) ->
                                                        (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                                _lhsIopts
                                                                {-# LINE 1127 "src/PrettyTree.hs" #-}
                                                                )) of
                                                         { _flagsOopts | _flagsOopts `seq` (True) ->
                                                         (case (flags_ _flagsOopts _flagsOtbls) of
                                                          { ( _flagsIoutput) | True ->
                                                              (case (({-# LINE 303 "src\\PrettyTree.ag" #-}
                                                                      text "INSTANCE" <+> _nmIoutput
                                                                      $+$ nest 2 (   text "SUPER" <+> _sIoutput
                                                                                 $+$ _interfacesIoutput
                                                                                 $+$ _flagsIoutput
                                                                                 $+$ text "CON" <+> _conIoutput
                                                                                 $+$ text "NS" <+> _nsIoutput
                                                                                 $+$ _traitsIoutput
                                                                                 )
                                                                      {-# LINE 1141 "src/PrettyTree.hs" #-}
                                                                      )) of
                                                               { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                               ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- InstanceInfos -----------------------------------------------
-- cata
sem_InstanceInfos :: InstanceInfos ->
                     T_InstanceInfos
sem_InstanceInfos list =
    (Prelude.foldr sem_InstanceInfos_Cons sem_InstanceInfos_Nil (Prelude.map sem_InstanceInfo list))
-- semantic domain
type T_InstanceInfos = Word32 ->
                       Options ->
                       SymbolTables ->
                       ( Doc)
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 1165 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 1170 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOtbls | _hdOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 1175 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (({-# LINE 284 "src\\PrettyTree.ag" #-}
                    1 + _lhsIindex
                    {-# LINE 1180 "src/PrettyTree.hs" #-}
                    )) of
             { _tlOindex | _tlOindex `seq` (True) ->
             (case (tl_ _tlOindex _tlOopts _tlOtbls) of
              { ( _tlIoutput) | True ->
                  (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                          _lhsIopts
                          {-# LINE 1187 "src/PrettyTree.hs" #-}
                          )) of
                   { _hdOopts | _hdOopts `seq` (True) ->
                   (case (({-# LINE 277 "src\\PrettyTree.ag" #-}
                           _lhsIindex
                           {-# LINE 1192 "src/PrettyTree.hs" #-}
                           )) of
                    { _hdOindex | _hdOindex `seq` (True) ->
                    (case (hd_ _hdOindex _hdOopts _hdOtbls) of
                     { ( _hdIoutput) | True ->
                         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                 _hdIoutput $+$ _tlIoutput
                                 {-# LINE 1199 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 1210 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
                     ( Int,(Maybe Int),T_Instruction_1)
type T_Instruction_1 = ExceptionDescrs ->
                       (Maybe Int) ->
                       Options ->
                       Int ->
                       SymbolTables ->
                       ( Doc,Int)
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1584 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1589 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Add_1 :: T_Instruction_1
                       sem_Instruction_Add_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 510 "src\\PrettyTree.ag" #-}
                                        text "Add"
                                        {-# LINE 1601 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 1610 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 1615 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 1620 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 1625 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Add_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1637 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1642 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Add_i_1 :: T_Instruction_1
                       sem_Instruction_Add_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 511 "src\\PrettyTree.ag" #-}
                                        text "Add_i"
                                        {-# LINE 1654 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 1663 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 1668 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 1673 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 1678 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Add_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1690 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1695 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Add_d_1 :: T_Instruction_1
                       sem_Instruction_Add_d_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 512 "src\\PrettyTree.ag" #-}
                                        text "Add_d"
                                        {-# LINE 1707 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 1716 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 1721 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 1726 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 1731 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Add_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1744 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1749 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_ApplyType_1 :: T_Instruction_1
                       sem_Instruction_ApplyType_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 1761 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 1768 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 513 "src\\PrettyTree.ag" #-}
                                                text "Apply type"  <+> _nmIoutput
                                                {-# LINE 1775 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 1784 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 1789 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 1794 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 1799 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_ApplyType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1812 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1817 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_AsType_1 :: T_Instruction_1
                       sem_Instruction_AsType_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 1829 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 1836 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 514 "src\\PrettyTree.ag" #-}
                                                text "As type"     <+> _nmIoutput
                                                {-# LINE 1843 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 1852 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 1857 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 1862 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 1867 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_AsType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1879 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1884 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_AsTypeLate_1 :: T_Instruction_1
                       sem_Instruction_AsTypeLate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 515 "src\\PrettyTree.ag" #-}
                                        text "As type (late)"
                                        {-# LINE 1896 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 1905 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 1910 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 1915 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 1920 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_AsTypeLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1932 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1937 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Breakpoint_1 :: T_Instruction_1
                       sem_Instruction_Breakpoint_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 516 "src\\PrettyTree.ag" #-}
                                        text "Breakpoint"
                                        {-# LINE 1949 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 1958 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 1963 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 1968 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 1973 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Breakpoint_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 1986 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 1991 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_BreakLine_1 :: T_Instruction_1
                       sem_Instruction_BreakLine_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 517 "src\\PrettyTree.ag" #-}
                                        text "Break line" <+> num line_
                                        {-# LINE 2003 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2012 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2017 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2022 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2027 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_BreakLine_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2039 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2044 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_BitAnd_1 :: T_Instruction_1
                       sem_Instruction_BitAnd_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 518 "src\\PrettyTree.ag" #-}
                                        text "BitAnd"
                                        {-# LINE 2056 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2065 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2070 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2075 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2080 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_BitAnd_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2092 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2097 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_BitNot_1 :: T_Instruction_1
                       sem_Instruction_BitNot_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 519 "src\\PrettyTree.ag" #-}
                                        text "BitNot"
                                        {-# LINE 2109 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2118 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2123 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2128 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2133 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_BitNot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2145 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2150 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_BitOr_1 :: T_Instruction_1
                       sem_Instruction_BitOr_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 520 "src\\PrettyTree.ag" #-}
                                        text "BitOr"
                                        {-# LINE 2162 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2171 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2176 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2181 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2186 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_BitOr_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2198 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2203 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_BitXor_1 :: T_Instruction_1
                       sem_Instruction_BitXor_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 521 "src\\PrettyTree.ag" #-}
                                        text "BitXor"
                                        {-# LINE 2215 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2224 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2229 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2234 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2239 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_BitXor_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2252 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2257 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Call_1 :: T_Instruction_1
                       sem_Instruction_Call_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 522 "src\\PrettyTree.ag" #-}
                                        text "Call" <+> props [("args", num argCount_)]
                                        {-# LINE 2269 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2278 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2283 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2288 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2293 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Call_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2307 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2312 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallInterface_1 :: T_Instruction_1
                       sem_Instruction_CallInterface_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2324 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2331 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 523 "src\\PrettyTree.ag" #-}
                                                text "Call interface" <+> _nmIoutput <+> props [("args", num argCount_)]
                                                {-# LINE 2338 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2347 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2352 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2357 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2362 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallInterface_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2376 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2381 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallMethod_1 :: T_Instruction_1
                       sem_Instruction_CallMethod_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 524 "src\\PrettyTree.ag" #-}
                                        text "Call method" <+> props [("index", num index_), ("args", num argCount_)]
                                        {-# LINE 2393 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2402 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2407 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2412 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2417 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_CallMethod_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2431 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2436 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallProp_1 :: T_Instruction_1
                       sem_Instruction_CallProp_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2448 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2455 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 525 "src\\PrettyTree.ag" #-}
                                                text "Call prop" <+> _nmIoutput <+> props [("args", num argCount_)]
                                                {-# LINE 2462 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2471 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2476 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2481 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2486 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallProp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2500 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2505 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallPropLex_1 :: T_Instruction_1
                       sem_Instruction_CallPropLex_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2517 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2524 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 526 "src\\PrettyTree.ag" #-}
                                                text "Call prop (lex)" <+> _nmIoutput <+> props [("args", num argCount_)]
                                                {-# LINE 2531 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2540 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2545 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2550 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2555 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallPropLex_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2569 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2574 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallPropVoid_1 :: T_Instruction_1
                       sem_Instruction_CallPropVoid_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2586 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2593 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 527 "src\\PrettyTree.ag" #-}
                                                text "Call prop (void)" <+> _nmIoutput <+> props [("args", num argCount_)]
                                                {-# LINE 2600 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2609 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2614 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2619 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2624 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallPropVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2638 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2643 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallStatic_1 :: T_Instruction_1
                       sem_Instruction_CallStatic_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 488 "src\\PrettyTree.ag" #-}
                                        methodView _lhsItbls (Ref method_)
                                        {-# LINE 2655 "src/PrettyTree.hs" #-}
                                        )) of
                                 { m_val_ | m_val_ `seq` (True) ->
                                 (case ((sem_MethodV m_val_)) of
                                  { m_inst_ | m_inst_ `seq` (True) ->
                                  (case (({-# LINE 489 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2662 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _mOinfo | _mOinfo `seq` (True) ->
                                   (case (m_inst_ _mOinfo) of
                                    { ( _mIoutput) | True ->
                                        (case (({-# LINE 528 "src\\PrettyTree.ag" #-}
                                                text "Call static" <+> props [("method", _mIoutput), ("args", num argCount_)]
                                                {-# LINE 2669 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2678 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2683 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2688 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2693 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallStatic_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2707 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2712 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallSuper_1 :: T_Instruction_1
                       sem_Instruction_CallSuper_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2724 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2731 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 529 "src\\PrettyTree.ag" #-}
                                                text "Call super" <+> _nmIoutput <+> props [("args",num  argCount_)]
                                                {-# LINE 2738 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2747 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2752 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2757 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2762 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2774 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2779 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallSuperId_1 :: T_Instruction_1
                       sem_Instruction_CallSuperId_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 530 "src\\PrettyTree.ag" #-}
                                        text "Call super (id)"
                                        {-# LINE 2791 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2800 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2805 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2810 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2815 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_CallSuperId_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2829 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2834 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CallSuperVoid_1 :: T_Instruction_1
                       sem_Instruction_CallSuperVoid_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2846 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2853 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 531 "src\\PrettyTree.ag" #-}
                                                text "Call super (void)" <+> _nmIoutput <+> props [("args",num  argCount_)]
                                                {-# LINE 2860 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2869 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2874 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 2879 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 2884 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_CallSuperVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2896 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2901 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_CheckFilter_1 :: T_Instruction_1
                       sem_Instruction_CheckFilter_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 532 "src\\PrettyTree.ag" #-}
                                        text "CheckFilter"
                                        {-# LINE 2913 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 2922 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 2927 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 2932 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 2937 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_CheckFilter_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 2950 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 2955 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_1 :: T_Instruction_1
                       sem_Instruction_Coerce_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 2967 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 2974 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 533 "src\\PrettyTree.ag" #-}
                                                text "Coerce" <+> _nmIoutput
                                                {-# LINE 2981 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 2990 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 2995 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 3000 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 3005 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_Coerce_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3017 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3022 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_a_1 :: T_Instruction_1
                       sem_Instruction_Coerce_a_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 534 "src\\PrettyTree.ag" #-}
                                        text "Coerce_a (any type)"
                                        {-# LINE 3034 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3043 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3048 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3053 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3058 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_a_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3070 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3075 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_b_1 :: T_Instruction_1
                       sem_Instruction_Coerce_b_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 535 "src\\PrettyTree.ag" #-}
                                        text "Coerce_b"
                                        {-# LINE 3087 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3096 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3101 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3106 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3111 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_b_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3123 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3128 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_d_1 :: T_Instruction_1
                       sem_Instruction_Coerce_d_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 536 "src\\PrettyTree.ag" #-}
                                        text "Coerce_d (double)"
                                        {-# LINE 3140 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3149 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3154 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3159 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3164 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3176 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3181 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_i_1 :: T_Instruction_1
                       sem_Instruction_Coerce_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 537 "src\\PrettyTree.ag" #-}
                                        text "Coerce_i (signed integer)"
                                        {-# LINE 3193 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3202 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3207 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3212 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3217 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3229 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3234 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_o_1 :: T_Instruction_1
                       sem_Instruction_Coerce_o_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 538 "src\\PrettyTree.ag" #-}
                                        text "Coerce_o"
                                        {-# LINE 3246 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3255 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3260 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3265 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3270 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_o_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3282 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3287 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_s_1 :: T_Instruction_1
                       sem_Instruction_Coerce_s_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 539 "src\\PrettyTree.ag" #-}
                                        text "Coerce_s (string)"
                                        {-# LINE 3299 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3308 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3313 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3318 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3323 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_s_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3335 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3340 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Coerce_u_1 :: T_Instruction_1
                       sem_Instruction_Coerce_u_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 540 "src\\PrettyTree.ag" #-}
                                        text "Coerce_u (unsigned integer)"
                                        {-# LINE 3352 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3361 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3366 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3371 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3376 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Coerce_u_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3388 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3393 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Concat_1 :: T_Instruction_1
                       sem_Instruction_Concat_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 541 "src\\PrettyTree.ag" #-}
                                        text "Concat"
                                        {-# LINE 3405 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3414 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3419 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3424 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3429 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Concat_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3442 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3447 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Construct_1 :: T_Instruction_1
                       sem_Instruction_Construct_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 542 "src\\PrettyTree.ag" #-}
                                        text "Construct" <+> props [("args", num argCount_)]
                                        {-# LINE 3459 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3468 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3473 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3478 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3483 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Construct_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3497 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3502 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_ConstructProp_1 :: T_Instruction_1
                       sem_Instruction_ConstructProp_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 3514 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 3521 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 543 "src\\PrettyTree.ag" #-}
                                                text "Construct prop" <+> _nmIoutput <+> props [("args", num argCount_)]
                                                {-# LINE 3528 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 3537 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 3542 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 3547 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 3552 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_ConstructProp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3565 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3570 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_ConstructSuper_1 :: T_Instruction_1
                       sem_Instruction_ConstructSuper_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 544 "src\\PrettyTree.ag" #-}
                                        text "Construct super" <+> props [("args", num argCount_)]
                                        {-# LINE 3582 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3591 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3596 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3601 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3606 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_ConstructSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3618 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3623 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_b_1 :: T_Instruction_1
                       sem_Instruction_Convert_b_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 545 "src\\PrettyTree.ag" #-}
                                        text "Convert_b"
                                        {-# LINE 3635 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3644 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3649 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3654 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3659 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_b_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3671 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3676 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_i_1 :: T_Instruction_1
                       sem_Instruction_Convert_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 546 "src\\PrettyTree.ag" #-}
                                        text "Convert_i (signed integer)"
                                        {-# LINE 3688 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3697 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3702 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3707 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3712 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3724 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3729 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_d_1 :: T_Instruction_1
                       sem_Instruction_Convert_d_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 547 "src\\PrettyTree.ag" #-}
                                        text "Convert_d (double)"
                                        {-# LINE 3741 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3750 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3755 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3760 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3765 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_d_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3777 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3782 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_o_1 :: T_Instruction_1
                       sem_Instruction_Convert_o_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 548 "src\\PrettyTree.ag" #-}
                                        text "Convert_o"
                                        {-# LINE 3794 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3803 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3808 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3813 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3818 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_o_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3830 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3835 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_u_1 :: T_Instruction_1
                       sem_Instruction_Convert_u_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 549 "src\\PrettyTree.ag" #-}
                                        text "Convert_u (unsigned integer)"
                                        {-# LINE 3847 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3856 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3861 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3866 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3871 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_u_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3883 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3888 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Convert_s_1 :: T_Instruction_1
                       sem_Instruction_Convert_s_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 550 "src\\PrettyTree.ag" #-}
                                        text "Convert_s (string)"
                                        {-# LINE 3900 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 3909 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 3914 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 3919 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 3924 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Convert_s_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 3940 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 3945 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Debug_1 :: T_Instruction_1
                       sem_Instruction_Debug_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 480 "src\\PrettyTree.ag" #-}
                                        stringView _lhsItbls (Ref name_)
                                        {-# LINE 3957 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_StrV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 481 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 3964 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 551 "src\\PrettyTree.ag" #-}
                                                text "Debug" <+> _nmIoutput <+> props [("reg", num reg_)]
                                                {-# LINE 3971 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 3980 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 3985 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 3990 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 3995 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_Debug_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4008 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4013 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DebugFile_1 :: T_Instruction_1
                       sem_Instruction_DebugFile_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 480 "src\\PrettyTree.ag" #-}
                                        stringView _lhsItbls (Ref name_)
                                        {-# LINE 4025 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_StrV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 481 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 4032 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 552 "src\\PrettyTree.ag" #-}
                                                text "Debug file" <+> _nmIoutput
                                                {-# LINE 4039 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 4048 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 4053 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 4058 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 4063 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_DebugFile_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4076 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4081 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DebugLine_1 :: T_Instruction_1
                       sem_Instruction_DebugLine_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 553 "src\\PrettyTree.ag" #-}
                                        text "Debug line" <+> num line_
                                        {-# LINE 4093 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4102 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4107 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4112 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4117 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_DebugLine_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4130 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4135 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DecLocal_1 :: T_Instruction_1
                       sem_Instruction_DecLocal_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 554 "src\\PrettyTree.ag" #-}
                                        text "DecLocal (number)" <+> num reg_
                                        {-# LINE 4147 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4156 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4161 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4166 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4171 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_DecLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4184 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4189 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DecLocal_i_1 :: T_Instruction_1
                       sem_Instruction_DecLocal_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 555 "src\\PrettyTree.ag" #-}
                                        text "DecLocal_i (integer)" <+> num reg_
                                        {-# LINE 4201 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4210 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4215 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4220 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4225 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_DecLocal_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4237 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4242 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Decrement_1 :: T_Instruction_1
                       sem_Instruction_Decrement_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 556 "src\\PrettyTree.ag" #-}
                                        text "Decrement (number)"
                                        {-# LINE 4254 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4263 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4268 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4273 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4278 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Decrement_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4290 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4295 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Decrement_i_1 :: T_Instruction_1
                       sem_Instruction_Decrement_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 557 "src\\PrettyTree.ag" #-}
                                        text "Decrement_i (integer)"
                                        {-# LINE 4307 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4316 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4321 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4326 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4331 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Decrement_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4344 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4349 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DeleteProperty_1 :: T_Instruction_1
                       sem_Instruction_DeleteProperty_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 4361 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 4368 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 558 "src\\PrettyTree.ag" #-}
                                                text "Delete property" <+> _nmIoutput
                                                {-# LINE 4375 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 4384 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 4389 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 4394 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 4399 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_DeleteProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4411 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4416 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DeletePropertyLate_1 :: T_Instruction_1
                       sem_Instruction_DeletePropertyLate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 559 "src\\PrettyTree.ag" #-}
                                        text "Delete property (late)"
                                        {-# LINE 4428 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4437 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4442 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4447 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4452 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_DeletePropertyLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4464 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4469 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Divide_1 :: T_Instruction_1
                       sem_Instruction_Divide_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 560 "src\\PrettyTree.ag" #-}
                                        text "Divide"
                                        {-# LINE 4481 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4490 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4495 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4500 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4505 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Divide_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4517 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4522 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Dup_1 :: T_Instruction_1
                       sem_Instruction_Dup_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 561 "src\\PrettyTree.ag" #-}
                                        text "Dup"
                                        {-# LINE 4534 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4543 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4548 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4553 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4558 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Dup_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4571 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4576 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Dxns_1 :: T_Instruction_1
                       sem_Instruction_Dxns_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 480 "src\\PrettyTree.ag" #-}
                                        stringView _lhsItbls (Ref name_)
                                        {-# LINE 4588 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_StrV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 481 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 4595 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 562 "src\\PrettyTree.ag" #-}
                                                text "Default namespace" <+> _nmIoutput
                                                {-# LINE 4602 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 4611 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 4616 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 4621 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 4626 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_Dxns_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4638 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4643 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_DxnsLate_1 :: T_Instruction_1
                       sem_Instruction_DxnsLate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 563 "src\\PrettyTree.ag" #-}
                                        text "Default namespace (late)"
                                        {-# LINE 4655 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4664 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4669 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4674 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4679 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_DxnsLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4691 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4696 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Equals_1 :: T_Instruction_1
                       sem_Instruction_Equals_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 564 "src\\PrettyTree.ag" #-}
                                        text "Equals"
                                        {-# LINE 4708 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4717 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4722 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4727 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4732 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Equals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4744 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4749 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_EscXAttr_1 :: T_Instruction_1
                       sem_Instruction_EscXAttr_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 565 "src\\PrettyTree.ag" #-}
                                        text "Escape Xml Attr"
                                        {-# LINE 4761 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4770 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4775 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4780 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4785 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_EscXAttr_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4797 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4802 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_EscXElem_1 :: T_Instruction_1
                       sem_Instruction_EscXElem_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 566 "src\\PrettyTree.ag" #-}
                                        text "Escape Xml Elem"
                                        {-# LINE 4814 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 4823 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 4828 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 4833 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 4838 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_EscXElem_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4851 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4856 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_FindDef_1 :: T_Instruction_1
                       sem_Instruction_FindDef_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 4868 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 4875 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 567 "src\\PrettyTree.ag" #-}
                                                text "Find Definition" <+> _nmIoutput
                                                {-# LINE 4882 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 4891 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 4896 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 4901 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 4906 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_FindDef_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4919 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4924 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_FindPropertyGlobalStrict_1 :: T_Instruction_1
                       sem_Instruction_FindPropertyGlobalStrict_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 4936 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 4943 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 568 "src\\PrettyTree.ag" #-}
                                                text "Find property global (strict)" <+> _nmIoutput
                                                {-# LINE 4950 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 4959 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 4964 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 4969 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 4974 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_FindPropertyGlobalStrict_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 4987 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 4992 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_FindPropertyGlobal_1 :: T_Instruction_1
                       sem_Instruction_FindPropertyGlobal_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5004 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5011 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 569 "src\\PrettyTree.ag" #-}
                                                text "Find property global" <+> _nmIoutput
                                                {-# LINE 5018 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5027 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5032 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5037 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5042 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_FindPropertyGlobal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5055 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5060 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_FindProperty_1 :: T_Instruction_1
                       sem_Instruction_FindProperty_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5072 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5079 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 570 "src\\PrettyTree.ag" #-}
                                                text "Find property" <+> _nmIoutput
                                                {-# LINE 5086 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5095 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5100 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5105 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5110 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_FindProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5123 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5128 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_FindPropStrict_1 :: T_Instruction_1
                       sem_Instruction_FindPropStrict_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5140 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5147 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 571 "src\\PrettyTree.ag" #-}
                                                text "Find property strict" <+> _nmIoutput
                                                {-# LINE 5154 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5163 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5168 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5173 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5178 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_FindPropStrict_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5191 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5196 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetDescendants_1 :: T_Instruction_1
                       sem_Instruction_GetDescendants_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5208 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5215 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 572 "src\\PrettyTree.ag" #-}
                                                text "Get descendants" <+> _nmIoutput
                                                {-# LINE 5222 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5231 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5236 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5241 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5246 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_GetDescendants_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5258 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5263 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetGlobalScope_1 :: T_Instruction_1
                       sem_Instruction_GetGlobalScope_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 573 "src\\PrettyTree.ag" #-}
                                        text "Get global scope"
                                        {-# LINE 5275 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5284 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5289 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5294 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5299 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetGlobalScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5312 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5317 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetGlobalSlot_1 :: T_Instruction_1
                       sem_Instruction_GetGlobalSlot_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 574 "src\\PrettyTree.ag" #-}
                                        text "Set global slot" <+> num slot_
                                        {-# LINE 5329 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5338 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5343 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5348 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5353 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetGlobalSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5366 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5371 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLex_1 :: T_Instruction_1
                       sem_Instruction_GetLex_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5383 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5390 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 575 "src\\PrettyTree.ag" #-}
                                                text "Get lex (find+get prop)" <+> _nmIoutput
                                                {-# LINE 5397 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5406 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5411 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5416 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5421 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_GetLex_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5434 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5439 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLocal_1 :: T_Instruction_1
                       sem_Instruction_GetLocal_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 576 "src\\PrettyTree.ag" #-}
                                        text "Get local" <+> num reg_
                                        {-# LINE 5451 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5460 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5465 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5470 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5475 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5487 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5492 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLocal0_1 :: T_Instruction_1
                       sem_Instruction_GetLocal0_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 577 "src\\PrettyTree.ag" #-}
                                        text "Get local0"
                                        {-# LINE 5504 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5513 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5518 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5523 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5528 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetLocal0_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5540 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5545 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLocal1_1 :: T_Instruction_1
                       sem_Instruction_GetLocal1_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 578 "src\\PrettyTree.ag" #-}
                                        text "Get local1"
                                        {-# LINE 5557 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5566 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5571 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5576 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5581 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetLocal1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5593 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5598 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLocal2_1 :: T_Instruction_1
                       sem_Instruction_GetLocal2_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 579 "src\\PrettyTree.ag" #-}
                                        text "Get local2"
                                        {-# LINE 5610 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5619 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5624 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5629 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5634 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetLocal2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5646 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5651 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetLocal3_1 :: T_Instruction_1
                       sem_Instruction_GetLocal3_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 580 "src\\PrettyTree.ag" #-}
                                        text "Get local3"
                                        {-# LINE 5663 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5672 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5677 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5682 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5687 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetLocal3_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5700 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5705 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetOuterScope_1 :: T_Instruction_1
                       sem_Instruction_GetOuterScope_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5717 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5724 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 581 "src\\PrettyTree.ag" #-}
                                                text "Get outer scope" <+> _nmIoutput
                                                {-# LINE 5731 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5740 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5745 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5750 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5755 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_GetOuterScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5768 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5773 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetProperty_1 :: T_Instruction_1
                       sem_Instruction_GetProperty_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5785 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5792 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 582 "src\\PrettyTree.ag" #-}
                                                text "Get property" <+> _nmIoutput
                                                {-# LINE 5799 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5808 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5813 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5818 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5823 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_GetProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5836 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5841 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetScopeObject_1 :: T_Instruction_1
                       sem_Instruction_GetScopeObject_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 583 "src\\PrettyTree.ag" #-}
                                        text "Get scope object" <+> props [("index", num index_)]
                                        {-# LINE 5853 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5862 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5867 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5872 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5877 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetScopeObject_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5890 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5895 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetSlot_1 :: T_Instruction_1
                       sem_Instruction_GetSlot_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 584 "src\\PrettyTree.ag" #-}
                                        text "Get slot" <+> num slot_
                                        {-# LINE 5907 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 5916 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 5921 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 5926 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 5931 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GetSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 5944 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 5949 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GetSuper_1 :: T_Instruction_1
                       sem_Instruction_GetSuper_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 5961 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 5968 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 585 "src\\PrettyTree.ag" #-}
                                                text "Get super" <+> _nmIoutput
                                                {-# LINE 5975 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 5984 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 5989 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 5994 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 5999 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_GetSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6011 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6016 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GreaterEquals_1 :: T_Instruction_1
                       sem_Instruction_GreaterEquals_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 586 "src\\PrettyTree.ag" #-}
                                        text "Greater or equals"
                                        {-# LINE 6028 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 6037 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 6042 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 6047 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 6052 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GreaterEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6064 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6069 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_GreaterThan_1 :: T_Instruction_1
                       sem_Instruction_GreaterThan_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 587 "src\\PrettyTree.ag" #-}
                                        text "Greater than"
                                        {-# LINE 6081 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 6090 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 6095 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 6100 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 6105 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_GreaterThan_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6117 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6122 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_HasNext_1 :: T_Instruction_1
                       sem_Instruction_HasNext_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 588 "src\\PrettyTree.ag" #-}
                                        text "Has next"
                                        {-# LINE 6134 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 6143 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 6148 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 6153 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 6158 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_HasNext_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6172 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6177 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_HasNext2_1 :: T_Instruction_1
                       sem_Instruction_HasNext2_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 589 "src\\PrettyTree.ag" #-}
                                        text "Has next 2" <+> props [("object register", num objectReg_),("index register", num indexReg_)]
                                        {-# LINE 6189 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 6198 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 6203 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 6208 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 6213 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_HasNext2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6226 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6231 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfEq_1 :: T_Instruction_1
                       sem_Instruction_IfEq_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6243 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6248 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 590 "src\\PrettyTree.ag" #-}
                                          text "If equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6253 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6262 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6267 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6272 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6277 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfEq_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6290 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6295 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfFalse_1 :: T_Instruction_1
                       sem_Instruction_IfFalse_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6307 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6312 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 591 "src\\PrettyTree.ag" #-}
                                          text "If false" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6317 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6326 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6331 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6336 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6341 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfFalse_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6354 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6359 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfGe_1 :: T_Instruction_1
                       sem_Instruction_IfGe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6371 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6376 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 592 "src\\PrettyTree.ag" #-}
                                          text "If greater or equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6381 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6390 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6395 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6400 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6405 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfGe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6418 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6423 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfGt_1 :: T_Instruction_1
                       sem_Instruction_IfGt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6435 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6440 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 593 "src\\PrettyTree.ag" #-}
                                          text "If greater" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6445 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6454 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6459 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6464 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6469 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfGt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6482 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6487 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfLe_1 :: T_Instruction_1
                       sem_Instruction_IfLe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6499 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6504 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 594 "src\\PrettyTree.ag" #-}
                                          text "If less or equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6509 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6518 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6523 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6528 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6533 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfLe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6546 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6551 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfLt_1 :: T_Instruction_1
                       sem_Instruction_IfLt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6563 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6568 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 595 "src\\PrettyTree.ag" #-}
                                          text "If less than" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6573 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6582 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6587 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6592 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6597 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfLt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6610 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6615 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfNGe_1 :: T_Instruction_1
                       sem_Instruction_IfNGe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6627 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6632 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 596 "src\\PrettyTree.ag" #-}
                                          text "If not greater or equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6637 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6646 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6651 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6656 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6661 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNGe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6674 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6679 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfNGt_1 :: T_Instruction_1
                       sem_Instruction_IfNGt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6691 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6696 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 597 "src\\PrettyTree.ag" #-}
                                          text "If not greater than" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6701 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6710 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6715 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6720 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6725 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNGt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6738 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6743 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfNLe_1 :: T_Instruction_1
                       sem_Instruction_IfNLe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6755 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6760 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 598 "src\\PrettyTree.ag" #-}
                                          text "If not less or equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6765 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6774 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6779 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6784 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6789 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNLe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6802 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6807 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfNLt_1 :: T_Instruction_1
                       sem_Instruction_IfNLt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6819 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6824 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 599 "src\\PrettyTree.ag" #-}
                                          text "If not less than" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6829 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6838 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6843 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6848 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6853 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNLt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6866 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6871 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfNe_1 :: T_Instruction_1
                       sem_Instruction_IfNe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6883 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6888 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 600 "src\\PrettyTree.ag" #-}
                                          text "If not equal" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6893 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6902 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6907 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6912 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6917 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfNe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6930 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6935 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfStrictEq_1 :: T_Instruction_1
                       sem_Instruction_IfStrictEq_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 6947 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 6952 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 601 "src\\PrettyTree.ag" #-}
                                          text "If equal (strict)" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 6957 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 6966 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 6971 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 6976 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 6981 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfStrictEq_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 6994 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 6999 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfStrictNe_1 :: T_Instruction_1
                       sem_Instruction_IfStrictNe_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 7011 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 7016 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 602 "src\\PrettyTree.ag" #-}
                                          text "If not equal (strict)" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 7021 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 7030 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 7035 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 7040 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 7045 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfStrictNe_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7058 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7063 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IfTrue_1 :: T_Instruction_1
                       sem_Instruction_IfTrue_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 7075 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 7080 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 603 "src\\PrettyTree.ag" #-}
                                          text "If true" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 7085 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 7094 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 7099 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 7104 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 7109 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_IfTrue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7121 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7126 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_In_1 :: T_Instruction_1
                       sem_Instruction_In_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 604 "src\\PrettyTree.ag" #-}
                                        text "In (tests existence of property in object)"
                                        {-# LINE 7138 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7147 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7152 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7157 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7162 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_In_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7175 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7180 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IncLocal_1 :: T_Instruction_1
                       sem_Instruction_IncLocal_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 605 "src\\PrettyTree.ag" #-}
                                        text "Increment local (number)" <+> num reg_
                                        {-# LINE 7192 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7201 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7206 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7211 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7216 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_IncLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7229 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7234 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IncLocal_i_1 :: T_Instruction_1
                       sem_Instruction_IncLocal_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 606 "src\\PrettyTree.ag" #-}
                                        text "Increment local (integer)" <+> num reg_
                                        {-# LINE 7246 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7255 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7260 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7265 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7270 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_IncLocal_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7282 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7287 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Increment_1 :: T_Instruction_1
                       sem_Instruction_Increment_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 607 "src\\PrettyTree.ag" #-}
                                        text "Increment (number)"
                                        {-# LINE 7299 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7308 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7313 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7318 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7323 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Increment_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7335 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7340 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Increment_i_1 :: T_Instruction_1
                       sem_Instruction_Increment_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 608 "src\\PrettyTree.ag" #-}
                                        text "Increment (integer)"
                                        {-# LINE 7352 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7361 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7366 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7371 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7376 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Increment_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7389 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7394 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_InitProperty_1 :: T_Instruction_1
                       sem_Instruction_InitProperty_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 7406 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 7413 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 609 "src\\PrettyTree.ag" #-}
                                                text "Init property" <+> _nmIoutput
                                                {-# LINE 7420 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 7429 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 7434 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 7439 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 7444 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_InitProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7456 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7461 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_InstanceOf_1 :: T_Instruction_1
                       sem_Instruction_InstanceOf_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 610 "src\\PrettyTree.ag" #-}
                                        text "Instance of"
                                        {-# LINE 7473 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7482 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7487 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7492 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7497 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_InstanceOf_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7510 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7515 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IsType_1 :: T_Instruction_1
                       sem_Instruction_IsType_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 7527 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 7534 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 611 "src\\PrettyTree.ag" #-}
                                                text "Is type" <+> _nmIoutput
                                                {-# LINE 7541 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 7550 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 7555 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 7560 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 7565 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_IsType_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7577 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7582 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_IsTypeLate_1 :: T_Instruction_1
                       sem_Instruction_IsTypeLate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 612 "src\\PrettyTree.ag" #-}
                                        text "Is type (late)"
                                        {-# LINE 7594 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7603 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7608 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7613 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7618 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_IsTypeLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7631 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7636 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Jump_1 :: T_Instruction_1
                       sem_Instruction_Jump_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 26 "src\\ByteCodeLocationInfo.ag" #-}
                                        fromS24 offset_
                                        {-# LINE 7648 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _relative | _relative `seq` (True) ->
                                 (case (({-# LINE 27 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation + _relative
                                         {-# LINE 7653 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _target | _target `seq` (True) ->
                                  (case (({-# LINE 613 "src\\PrettyTree.ag" #-}
                                          text "Jump" <+> num _target     <+> parens (num _relative    )
                                          {-# LINE 7658 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _descr | _descr `seq` (True) ->
                                   (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                           case _lhsImbLoc of
                                             Nothing -> text (replicate 8 ' ')
                                             Just i  -> let str = show i
                                                            sps = replicate (max 0 (6 - length str))  ' '
                                                        in  text str <> text sps <> text ": "
                                           {-# LINE 7667 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lbl | _lbl `seq` (True) ->
                                    (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                            _lbl     <> text "INSTR"
                                            {-# LINE 7672 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _prefix | _prefix `seq` (True) ->
                                     (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                             _prefix     <+> _descr
                                             {-# LINE 7677 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                      (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                              _lhsIrevLocation
                                              {-# LINE 7682 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                       ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }))
                   in  sem_Instruction_Jump_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7695 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7700 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Kill_1 :: T_Instruction_1
                       sem_Instruction_Kill_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 614 "src\\PrettyTree.ag" #-}
                                        text "Kill register" <+> num reg_
                                        {-# LINE 7712 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7721 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7726 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7731 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7736 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Kill_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7748 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7753 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Label_1 :: T_Instruction_1
                       sem_Instruction_Label_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 615 "src\\PrettyTree.ag" #-}
                                        text "Label (nop)"
                                        {-# LINE 7765 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7774 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7779 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7784 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7789 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Label_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7801 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7806 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LessEquals_1 :: T_Instruction_1
                       sem_Instruction_LessEquals_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 616 "src\\PrettyTree.ag" #-}
                                        text "Less or equals"
                                        {-# LINE 7818 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7827 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7832 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7837 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7842 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LessEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7854 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7859 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LessThan_1 :: T_Instruction_1
                       sem_Instruction_LessThan_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 617 "src\\PrettyTree.ag" #-}
                                        text "Less than"
                                        {-# LINE 7871 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7880 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7885 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7890 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7895 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LessThan_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7907 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7912 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LoadFloat32_1 :: T_Instruction_1
                       sem_Instruction_LoadFloat32_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 618 "src\\PrettyTree.ag" #-}
                                        text "Load float 32"
                                        {-# LINE 7924 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7933 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7938 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7943 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 7948 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LoadFloat32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 7960 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 7965 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LoadFloat64_1 :: T_Instruction_1
                       sem_Instruction_LoadFloat64_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 619 "src\\PrettyTree.ag" #-}
                                        text "Load float 64"
                                        {-# LINE 7977 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 7986 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 7991 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 7996 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8001 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LoadFloat64_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8013 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8018 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect8_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect8_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 620 "src\\PrettyTree.ag" #-}
                                        text "Load indirect 8"
                                        {-# LINE 8030 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8039 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8044 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8049 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8054 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LoadIndirect8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8066 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8071 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect16_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect16_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 621 "src\\PrettyTree.ag" #-}
                                        text "Load indirect 16"
                                        {-# LINE 8083 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8092 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8097 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8102 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8107 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LoadIndirect16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8119 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8124 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_LoadIndirect32_1 :: T_Instruction_1
                       sem_Instruction_LoadIndirect32_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 622 "src\\PrettyTree.ag" #-}
                                        text "Load indirect 32"
                                        {-# LINE 8136 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8145 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8150 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8155 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8160 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_LoadIndirect32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8174 "src/PrettyTree.hs" #-}
                 )) of
          { _caseOffsetsOlocation | _caseOffsetsOlocation `seq` (True) ->
          (case (caseOffsets_ _caseOffsetsOlocation) of
           { ( _caseOffsetsIlocation,caseOffsets_1) | True ->
               (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                       _caseOffsetsIlocation
                       {-# LINE 8181 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOlocation | _lhsOlocation `seq` (True) ->
                (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                        Nothing
                        {-# LINE 8186 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
                 (case ((let sem_Instruction_LookupSwitch_1 :: T_Instruction_1
                             sem_Instruction_LookupSwitch_1 =
                                 (\ _lhsIexcptEnv
                                    _lhsImbLoc
                                    _lhsIopts
                                    _lhsIrevLocation
                                    _lhsItbls ->
                                      (case (({-# LINE 23 "src\\ByteCodeLocationInfo.ag" #-}
                                              fromS24 defaultOffset_
                                              {-# LINE 8198 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _relative | _relative `seq` (True) ->
                                       (case (({-# LINE 24 "src\\ByteCodeLocationInfo.ag" #-}
                                               _lhsIlocation + _relative
                                               {-# LINE 8203 "src/PrettyTree.hs" #-}
                                               )) of
                                        { _target | _target `seq` (True) ->
                                        (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                _lhsItbls
                                                {-# LINE 8208 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _caseOffsetsOtbls | _caseOffsetsOtbls `seq` (True) ->
                                         (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                 _lhsIrevLocation
                                                 {-# LINE 8213 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _caseOffsetsOrevLocation | _caseOffsetsOrevLocation `seq` (True) ->
                                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                  _lhsIopts
                                                  {-# LINE 8218 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _caseOffsetsOopts | _caseOffsetsOopts `seq` (True) ->
                                           (case (caseOffsets_1 _caseOffsetsOopts _caseOffsetsOrevLocation _caseOffsetsOtbls) of
                                            { ( _caseOffsetsIoutput,_caseOffsetsIrevLocation) | True ->
                                                (case (({-# LINE 623 "src\\PrettyTree.ag" #-}
                                                        text "Switch" <+> num _target     <+> parens (num _relative    )
                                                        $+$ _caseOffsetsIoutput
                                                        {-# LINE 8226 "src/PrettyTree.hs" #-}
                                                        )) of
                                                 { _descr | _descr `seq` (True) ->
                                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                         case _lhsImbLoc of
                                                           Nothing -> text (replicate 8 ' ')
                                                           Just i  -> let str = show i
                                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                                      in  text str <> text sps <> text ": "
                                                         {-# LINE 8235 "src/PrettyTree.hs" #-}
                                                         )) of
                                                  { _lbl | _lbl `seq` (True) ->
                                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                          _lbl     <> text "INSTR"
                                                          {-# LINE 8240 "src/PrettyTree.hs" #-}
                                                          )) of
                                                   { _prefix | _prefix `seq` (True) ->
                                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                           _prefix     <+> _descr
                                                           {-# LINE 8245 "src/PrettyTree.hs" #-}
                                                           )) of
                                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                            _caseOffsetsIrevLocation
                                                            {-# LINE 8250 "src/PrettyTree.hs" #-}
                                                            )) of
                                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }) }) }))
                         in  sem_Instruction_LookupSwitch_1)) of
                  { ( sem_Instruction_1) | True ->
                  ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }) }) }))
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8262 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8267 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Lshift_1 :: T_Instruction_1
                       sem_Instruction_Lshift_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 625 "src\\PrettyTree.ag" #-}
                                        text "Left shift"
                                        {-# LINE 8279 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8288 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8293 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8298 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8303 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Lshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8315 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8320 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Modulo_1 :: T_Instruction_1
                       sem_Instruction_Modulo_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 626 "src\\PrettyTree.ag" #-}
                                        text "Modulo"
                                        {-# LINE 8332 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8341 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8346 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8351 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8356 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Modulo_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8368 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8373 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Multiply_1 :: T_Instruction_1
                       sem_Instruction_Multiply_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 627 "src\\PrettyTree.ag" #-}
                                        text "Multiply (number)"
                                        {-# LINE 8385 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8394 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8399 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8404 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8409 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Multiply_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8421 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8426 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Multiply_i_1 :: T_Instruction_1
                       sem_Instruction_Multiply_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 628 "src\\PrettyTree.ag" #-}
                                        text "Multiply (integer)"
                                        {-# LINE 8438 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8447 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8452 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8457 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8462 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Multiply_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8474 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8479 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Negate_1 :: T_Instruction_1
                       sem_Instruction_Negate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 629 "src\\PrettyTree.ag" #-}
                                        text "Negate (number)"
                                        {-# LINE 8491 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8500 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8505 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8510 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8515 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Negate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8527 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8532 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Negate_i_1 :: T_Instruction_1
                       sem_Instruction_Negate_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 630 "src\\PrettyTree.ag" #-}
                                        text "Negate (integer)"
                                        {-# LINE 8544 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8553 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8558 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8563 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8568 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Negate_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8580 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8585 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewActivation_1 :: T_Instruction_1
                       sem_Instruction_NewActivation_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 631 "src\\PrettyTree.ag" #-}
                                        text "New activation"
                                        {-# LINE 8597 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8606 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8611 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8616 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8621 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_NewActivation_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8634 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8639 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewArray_1 :: T_Instruction_1
                       sem_Instruction_NewArray_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 632 "src\\PrettyTree.ag" #-}
                                        text "New array" <+> props [("entries", num argCount_)]
                                        {-# LINE 8651 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8660 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8665 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8670 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8675 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_NewArray_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8688 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8693 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewCatch_1 :: T_Instruction_1
                       sem_Instruction_NewCatch_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 491 "src\\PrettyTree.ag" #-}
                                        lookupException (Ref exception_) _lhsIexcptEnv
                                        {-# LINE 8705 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _edesc | _edesc `seq` (True) ->
                                 (case (({-# LINE 498 "src\\PrettyTree.ag" #-}
                                         nameView _lhsItbls (expTp _edesc    )
                                         {-# LINE 8710 "src/PrettyTree.hs" #-}
                                         )) of
                                  { tp_val_ | tp_val_ `seq` (True) ->
                                  (case ((sem_NmV tp_val_)) of
                                   { tp_inst_ | tp_inst_ `seq` (True) ->
                                   (case (({-# LINE 499 "src\\PrettyTree.ag" #-}
                                           Short
                                           {-# LINE 8717 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _tpOinfo | _tpOinfo `seq` (True) ->
                                    (case (({-# LINE 494 "src\\PrettyTree.ag" #-}
                                            nameView _lhsItbls (expName _edesc    )
                                            {-# LINE 8722 "src/PrettyTree.hs" #-}
                                            )) of
                                     { nm_val_ | nm_val_ `seq` (True) ->
                                     (case ((sem_NmV nm_val_)) of
                                      { nm_inst_ | nm_inst_ `seq` (True) ->
                                      (case (({-# LINE 495 "src\\PrettyTree.ag" #-}
                                              Short
                                              {-# LINE 8729 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _nmOinfo | _nmOinfo `seq` (True) ->
                                       (case (tp_inst_ _tpOinfo) of
                                        { ( _tpIisEmpty,_tpIoutput) | True ->
                                            (case (nm_inst_ _nmOinfo) of
                                             { ( _nmIisEmpty,_nmIoutput) | True ->
                                                 (case (({-# LINE 633 "src\\PrettyTree.ag" #-}
                                                         text "New catch" <+> props
                                                           [ ("from", num $ expFrom _edesc    ), ("to", num $ expTo _edesc    )
                                                           , ("target", num $ expTarget _edesc    )
                                                           , ("type", _tpIoutput), ("name", _nmIoutput) ]
                                                         {-# LINE 8741 "src/PrettyTree.hs" #-}
                                                         )) of
                                                  { _descr | _descr `seq` (True) ->
                                                  (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                          case _lhsImbLoc of
                                                            Nothing -> text (replicate 8 ' ')
                                                            Just i  -> let str = show i
                                                                           sps = replicate (max 0 (6 - length str))  ' '
                                                                       in  text str <> text sps <> text ": "
                                                          {-# LINE 8750 "src/PrettyTree.hs" #-}
                                                          )) of
                                                   { _lbl | _lbl `seq` (True) ->
                                                   (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                           _lbl     <> text "INSTR"
                                                           {-# LINE 8755 "src/PrettyTree.hs" #-}
                                                           )) of
                                                    { _prefix | _prefix `seq` (True) ->
                                                    (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                            _prefix     <+> _descr
                                                            {-# LINE 8760 "src/PrettyTree.hs" #-}
                                                            )) of
                                                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                     (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                             _lhsIrevLocation
                                                             {-# LINE 8765 "src/PrettyTree.hs" #-}
                                                             )) of
                                                      { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                      ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_NewCatch_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8778 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8783 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewClass_1 :: T_Instruction_1
                       sem_Instruction_NewClass_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 501 "src\\PrettyTree.ag" #-}
                                        lookupClass (Ref class_) _lhsItbls
                                        {-# LINE 8795 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _cdesc | _cdesc `seq` (True) ->
                                 (case (({-# LINE 503 "src\\PrettyTree.ag" #-}
                                         nameView _lhsItbls (clName _cdesc    )
                                         {-# LINE 8800 "src/PrettyTree.hs" #-}
                                         )) of
                                  { nm_val_ | nm_val_ `seq` (True) ->
                                  (case ((sem_NmV nm_val_)) of
                                   { nm_inst_ | nm_inst_ `seq` (True) ->
                                   (case (({-# LINE 504 "src\\PrettyTree.ag" #-}
                                           Short
                                           {-# LINE 8807 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _nmOinfo | _nmOinfo `seq` (True) ->
                                    (case (nm_inst_ _nmOinfo) of
                                     { ( _nmIisEmpty,_nmIoutput) | True ->
                                         (case (({-# LINE 637 "src\\PrettyTree.ag" #-}
                                                 text "New class" <+> _nmIoutput
                                                 {-# LINE 8814 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _descr | _descr `seq` (True) ->
                                          (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                  case _lhsImbLoc of
                                                    Nothing -> text (replicate 8 ' ')
                                                    Just i  -> let str = show i
                                                                   sps = replicate (max 0 (6 - length str))  ' '
                                                               in  text str <> text sps <> text ": "
                                                  {-# LINE 8823 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _lbl | _lbl `seq` (True) ->
                                           (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                   _lbl     <> text "INSTR"
                                                   {-# LINE 8828 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _prefix | _prefix `seq` (True) ->
                                            (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                    _prefix     <+> _descr
                                                    {-# LINE 8833 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                             (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                     _lhsIrevLocation
                                                     {-# LINE 8838 "src/PrettyTree.hs" #-}
                                                     )) of
                                              { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                              ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_NewClass_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8851 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8856 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewFunction_1 :: T_Instruction_1
                       sem_Instruction_NewFunction_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 488 "src\\PrettyTree.ag" #-}
                                        methodView _lhsItbls (Ref method_)
                                        {-# LINE 8868 "src/PrettyTree.hs" #-}
                                        )) of
                                 { m_val_ | m_val_ `seq` (True) ->
                                 (case ((sem_MethodV m_val_)) of
                                  { m_inst_ | m_inst_ `seq` (True) ->
                                  (case (({-# LINE 489 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 8875 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _mOinfo | _mOinfo `seq` (True) ->
                                   (case (m_inst_ _mOinfo) of
                                    { ( _mIoutput) | True ->
                                        (case (({-# LINE 638 "src\\PrettyTree.ag" #-}
                                                text "New function" <+> _mIoutput
                                                {-# LINE 8882 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 8891 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 8896 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 8901 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 8906 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_NewFunction_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8919 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8924 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NewObject_1 :: T_Instruction_1
                       sem_Instruction_NewObject_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 639 "src\\PrettyTree.ag" #-}
                                        text "New object" <+> props [("args", num argCount_)]
                                        {-# LINE 8936 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8945 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 8950 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 8955 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 8960 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_NewObject_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 8972 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 8977 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NextName_1 :: T_Instruction_1
                       sem_Instruction_NextName_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 640 "src\\PrettyTree.ag" #-}
                                        text "NextName"
                                        {-# LINE 8989 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 8998 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9003 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9008 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9013 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_NextName_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9025 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9030 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_NextValue_1 :: T_Instruction_1
                       sem_Instruction_NextValue_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 641 "src\\PrettyTree.ag" #-}
                                        text "NextValue"
                                        {-# LINE 9042 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9051 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9056 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9061 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9066 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_NextValue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9078 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9083 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Nop_1 :: T_Instruction_1
                       sem_Instruction_Nop_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 642 "src\\PrettyTree.ag" #-}
                                        text "Nop"
                                        {-# LINE 9095 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9104 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9109 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9114 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9119 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Nop_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9131 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9136 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Not_1 :: T_Instruction_1
                       sem_Instruction_Not_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 643 "src\\PrettyTree.ag" #-}
                                        text "Not"
                                        {-# LINE 9148 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9157 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9162 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9167 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9172 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Not_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9184 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9189 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Pop_1 :: T_Instruction_1
                       sem_Instruction_Pop_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 644 "src\\PrettyTree.ag" #-}
                                        text "Pop"
                                        {-# LINE 9201 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9210 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9215 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9220 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9225 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Pop_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9237 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9242 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PopScope_1 :: T_Instruction_1
                       sem_Instruction_PopScope_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 645 "src\\PrettyTree.ag" #-}
                                        text "Pop scope"
                                        {-# LINE 9254 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9263 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9268 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9273 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9278 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PopScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9291 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9296 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushByte_1 :: T_Instruction_1
                       sem_Instruction_PushByte_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 646 "src\\PrettyTree.ag" #-}
                                        text "Push byte" <+> num val_
                                        {-# LINE 9308 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9317 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9322 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9327 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9332 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushByte_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9345 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9350 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushDouble_1 :: T_Instruction_1
                       sem_Instruction_PushDouble_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 507 "src\\PrettyTree.ag" #-}
                                        lookupDouble (Ref name_) _lhsItbls
                                        {-# LINE 9362 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _val | _val `seq` (True) ->
                                 (case (({-# LINE 647 "src\\PrettyTree.ag" #-}
                                         text "Push double" <+> num _val
                                         {-# LINE 9367 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _descr | _descr `seq` (True) ->
                                  (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                          case _lhsImbLoc of
                                            Nothing -> text (replicate 8 ' ')
                                            Just i  -> let str = show i
                                                           sps = replicate (max 0 (6 - length str))  ' '
                                                       in  text str <> text sps <> text ": "
                                          {-# LINE 9376 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _lbl | _lbl `seq` (True) ->
                                   (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                           _lbl     <> text "INSTR"
                                           {-# LINE 9381 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _prefix | _prefix `seq` (True) ->
                                    (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                            _prefix     <+> _descr
                                            {-# LINE 9386 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                     (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                             _lhsIrevLocation
                                             {-# LINE 9391 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                      ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }))
                   in  sem_Instruction_PushDouble_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9403 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9408 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushFalse_1 :: T_Instruction_1
                       sem_Instruction_PushFalse_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 648 "src\\PrettyTree.ag" #-}
                                        text "Push false"
                                        {-# LINE 9420 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9429 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9434 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9439 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9444 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushFalse_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9457 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9462 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushInt_1 :: T_Instruction_1
                       sem_Instruction_PushInt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 505 "src\\PrettyTree.ag" #-}
                                        lookupInt    (Ref name_) _lhsItbls
                                        {-# LINE 9474 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _val | _val `seq` (True) ->
                                 (case (({-# LINE 649 "src\\PrettyTree.ag" #-}
                                         text "Push signed integer" <+> num _val
                                         {-# LINE 9479 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _descr | _descr `seq` (True) ->
                                  (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                          case _lhsImbLoc of
                                            Nothing -> text (replicate 8 ' ')
                                            Just i  -> let str = show i
                                                           sps = replicate (max 0 (6 - length str))  ' '
                                                       in  text str <> text sps <> text ": "
                                          {-# LINE 9488 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _lbl | _lbl `seq` (True) ->
                                   (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                           _lbl     <> text "INSTR"
                                           {-# LINE 9493 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _prefix | _prefix `seq` (True) ->
                                    (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                            _prefix     <+> _descr
                                            {-# LINE 9498 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                     (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                             _lhsIrevLocation
                                             {-# LINE 9503 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                      ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }))
                   in  sem_Instruction_PushInt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9516 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9521 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushNamespace_1 :: T_Instruction_1
                       sem_Instruction_PushNamespace_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 484 "src\\PrettyTree.ag" #-}
                                        namespaceView _lhsItbls (Ref name_)
                                        {-# LINE 9533 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nms_val_ | nms_val_ `seq` (True) ->
                                 (case ((sem_NsV nms_val_)) of
                                  { nms_inst_ | nms_inst_ `seq` (True) ->
                                  (case (({-# LINE 485 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 9540 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmsOinfo | _nmsOinfo `seq` (True) ->
                                   (case (nms_inst_ _nmsOinfo) of
                                    { ( _nmsIisEmpty,_nmsIoutput) | True ->
                                        (case (({-# LINE 650 "src\\PrettyTree.ag" #-}
                                                text "Push namespace" <+> _nmsIoutput
                                                {-# LINE 9547 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 9556 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 9561 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 9566 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 9571 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_PushNamespace_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9583 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9588 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushNaN_1 :: T_Instruction_1
                       sem_Instruction_PushNaN_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 651 "src\\PrettyTree.ag" #-}
                                        text "Push NaN"
                                        {-# LINE 9600 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9609 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9614 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9619 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9624 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushNaN_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9636 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9641 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushNull_1 :: T_Instruction_1
                       sem_Instruction_PushNull_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 652 "src\\PrettyTree.ag" #-}
                                        text "Push null"
                                        {-# LINE 9653 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9662 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9667 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9672 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9677 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushNull_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9689 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9694 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushScope_1 :: T_Instruction_1
                       sem_Instruction_PushScope_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 653 "src\\PrettyTree.ag" #-}
                                        text "Push scope"
                                        {-# LINE 9706 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9715 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9720 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9725 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9730 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushScope_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9743 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9748 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushShort_1 :: T_Instruction_1
                       sem_Instruction_PushShort_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 654 "src\\PrettyTree.ag" #-}
                                        text "Push short" <+> num val_
                                        {-# LINE 9760 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9769 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9774 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9779 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9784 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushShort_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9797 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9802 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushString_1 :: T_Instruction_1
                       sem_Instruction_PushString_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 480 "src\\PrettyTree.ag" #-}
                                        stringView _lhsItbls (Ref name_)
                                        {-# LINE 9814 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_StrV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 481 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 9821 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 655 "src\\PrettyTree.ag" #-}
                                                text "Push string" <+> _nmIoutput
                                                {-# LINE 9828 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 9837 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 9842 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 9847 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 9852 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_PushString_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9864 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9869 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushTrue_1 :: T_Instruction_1
                       sem_Instruction_PushTrue_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 656 "src\\PrettyTree.ag" #-}
                                        text "Push true"
                                        {-# LINE 9881 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 9890 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 9895 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 9900 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 9905 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushTrue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9918 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9923 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushUInt_1 :: T_Instruction_1
                       sem_Instruction_PushUInt_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 506 "src\\PrettyTree.ag" #-}
                                        lookupUInt   (Ref name_) _lhsItbls
                                        {-# LINE 9935 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _val | _val `seq` (True) ->
                                 (case (({-# LINE 657 "src\\PrettyTree.ag" #-}
                                         text "Push unsigned integer" <+> num _val
                                         {-# LINE 9940 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _descr | _descr `seq` (True) ->
                                  (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                          case _lhsImbLoc of
                                            Nothing -> text (replicate 8 ' ')
                                            Just i  -> let str = show i
                                                           sps = replicate (max 0 (6 - length str))  ' '
                                                       in  text str <> text sps <> text ": "
                                          {-# LINE 9949 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _lbl | _lbl `seq` (True) ->
                                   (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                           _lbl     <> text "INSTR"
                                           {-# LINE 9954 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _prefix | _prefix `seq` (True) ->
                                    (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                            _prefix     <+> _descr
                                            {-# LINE 9959 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                     (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                             _lhsIrevLocation
                                             {-# LINE 9964 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                      ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }))
                   in  sem_Instruction_PushUInt_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 9976 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 9981 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushUndefined_1 :: T_Instruction_1
                       sem_Instruction_PushUndefined_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 658 "src\\PrettyTree.ag" #-}
                                        text "Push undefined"
                                        {-# LINE 9993 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10002 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10007 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10012 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10017 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushUndefined_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10029 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10034 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_PushWith_1 :: T_Instruction_1
                       sem_Instruction_PushWith_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 659 "src\\PrettyTree.ag" #-}
                                        text "Push with-scope"
                                        {-# LINE 10046 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10055 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10060 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10065 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10070 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_PushWith_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10082 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10087 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_ReturnValue_1 :: T_Instruction_1
                       sem_Instruction_ReturnValue_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 660 "src\\PrettyTree.ag" #-}
                                        text "Return (value)"
                                        {-# LINE 10099 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10108 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10113 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10118 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10123 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_ReturnValue_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10135 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10140 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_ReturnVoid_1 :: T_Instruction_1
                       sem_Instruction_ReturnVoid_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 661 "src\\PrettyTree.ag" #-}
                                        text "Return (void)"
                                        {-# LINE 10152 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10161 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10166 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10171 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10176 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_ReturnVoid_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10188 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10193 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Rshift_1 :: T_Instruction_1
                       sem_Instruction_Rshift_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 662 "src\\PrettyTree.ag" #-}
                                        text "Right shift"
                                        {-# LINE 10205 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10214 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10219 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10224 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10229 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Rshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10242 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10247 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetLocal_1 :: T_Instruction_1
                       sem_Instruction_SetLocal_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 663 "src\\PrettyTree.ag" #-}
                                        text "Set local" <+> num reg_
                                        {-# LINE 10259 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10268 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10273 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10278 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10283 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetLocal_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10295 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10300 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetLocal0_1 :: T_Instruction_1
                       sem_Instruction_SetLocal0_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 664 "src\\PrettyTree.ag" #-}
                                        text "Set local0"
                                        {-# LINE 10312 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10321 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10326 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10331 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10336 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetLocal0_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10348 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10353 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetLocal1_1 :: T_Instruction_1
                       sem_Instruction_SetLocal1_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 665 "src\\PrettyTree.ag" #-}
                                        text "Set local1"
                                        {-# LINE 10365 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10374 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10379 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10384 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10389 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetLocal1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10401 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10406 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetLocal2_1 :: T_Instruction_1
                       sem_Instruction_SetLocal2_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 666 "src\\PrettyTree.ag" #-}
                                        text "Set local2"
                                        {-# LINE 10418 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10427 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10432 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10437 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10442 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetLocal2_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10454 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10459 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetLocal3_1 :: T_Instruction_1
                       sem_Instruction_SetLocal3_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 667 "src\\PrettyTree.ag" #-}
                                        text "Set local3"
                                        {-# LINE 10471 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10480 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10485 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10490 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10495 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetLocal3_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10508 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10513 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetGlobalSlot_1 :: T_Instruction_1
                       sem_Instruction_SetGlobalSlot_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 668 "src\\PrettyTree.ag" #-}
                                        text "Set global slot" <+> num slot_
                                        {-# LINE 10525 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10534 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10539 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10544 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10549 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetGlobalSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10562 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10567 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetProperty_1 :: T_Instruction_1
                       sem_Instruction_SetProperty_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 10579 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 10586 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 669 "src\\PrettyTree.ag" #-}
                                                text "Set property" <+> _nmIoutput
                                                {-# LINE 10593 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 10602 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 10607 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 10612 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 10617 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_SetProperty_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10629 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10634 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetPropertyLate_1 :: T_Instruction_1
                       sem_Instruction_SetPropertyLate_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 670 "src\\PrettyTree.ag" #-}
                                        text "Set property (late)"
                                        {-# LINE 10646 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10655 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10660 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10665 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10670 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetPropertyLate_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10683 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10688 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetSlot_1 :: T_Instruction_1
                       sem_Instruction_SetSlot_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 671 "src\\PrettyTree.ag" #-}
                                        text "Set slot" <+> num slot_
                                        {-# LINE 10700 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10709 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10714 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10719 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10724 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SetSlot_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10737 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10742 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SetSuper_1 :: T_Instruction_1
                       sem_Instruction_SetSuper_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 476 "src\\PrettyTree.ag" #-}
                                        nameView _lhsItbls (Ref name_)
                                        {-# LINE 10754 "src/PrettyTree.hs" #-}
                                        )) of
                                 { nm_val_ | nm_val_ `seq` (True) ->
                                 (case ((sem_NmV nm_val_)) of
                                  { nm_inst_ | nm_inst_ `seq` (True) ->
                                  (case (({-# LINE 477 "src\\PrettyTree.ag" #-}
                                          Short
                                          {-# LINE 10761 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _nmOinfo | _nmOinfo `seq` (True) ->
                                   (case (nm_inst_ _nmOinfo) of
                                    { ( _nmIisEmpty,_nmIoutput) | True ->
                                        (case (({-# LINE 672 "src\\PrettyTree.ag" #-}
                                                text "Set super" <+> _nmIoutput
                                                {-# LINE 10768 "src/PrettyTree.hs" #-}
                                                )) of
                                         { _descr | _descr `seq` (True) ->
                                         (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                                 case _lhsImbLoc of
                                                   Nothing -> text (replicate 8 ' ')
                                                   Just i  -> let str = show i
                                                                  sps = replicate (max 0 (6 - length str))  ' '
                                                              in  text str <> text sps <> text ": "
                                                 {-# LINE 10777 "src/PrettyTree.hs" #-}
                                                 )) of
                                          { _lbl | _lbl `seq` (True) ->
                                          (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                                  _lbl     <> text "INSTR"
                                                  {-# LINE 10782 "src/PrettyTree.hs" #-}
                                                  )) of
                                           { _prefix | _prefix `seq` (True) ->
                                           (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                                   _prefix     <+> _descr
                                                   {-# LINE 10787 "src/PrettyTree.hs" #-}
                                                   )) of
                                            { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                            (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                                    _lhsIrevLocation
                                                    {-# LINE 10792 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                             ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }))
                   in  sem_Instruction_SetSuper_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10804 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10809 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SignExtend1_1 :: T_Instruction_1
                       sem_Instruction_SignExtend1_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 673 "src\\PrettyTree.ag" #-}
                                        text "Sign extend 1"
                                        {-# LINE 10821 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10830 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10835 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10840 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10845 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SignExtend1_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10857 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10862 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SignExtend8_1 :: T_Instruction_1
                       sem_Instruction_SignExtend8_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 674 "src\\PrettyTree.ag" #-}
                                        text "Sign extend 8"
                                        {-# LINE 10874 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10883 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10888 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10893 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10898 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SignExtend8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10910 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10915 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_SignExtend16_1 :: T_Instruction_1
                       sem_Instruction_SignExtend16_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 675 "src\\PrettyTree.ag" #-}
                                        text "Sign extend 16"
                                        {-# LINE 10927 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10936 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10941 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10946 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 10951 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_SignExtend16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 10963 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 10968 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StoreFloat32_1 :: T_Instruction_1
                       sem_Instruction_StoreFloat32_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 676 "src\\PrettyTree.ag" #-}
                                        text "Store float 32"
                                        {-# LINE 10980 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 10989 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 10994 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 10999 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11004 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StoreFloat32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11016 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11021 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StoreFloat64_1 :: T_Instruction_1
                       sem_Instruction_StoreFloat64_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 677 "src\\PrettyTree.ag" #-}
                                        text "Store float 64"
                                        {-# LINE 11033 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11042 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11047 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11052 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11057 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StoreFloat64_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11069 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11074 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect32_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect32_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 678 "src\\PrettyTree.ag" #-}
                                        text "Store indirect 32"
                                        {-# LINE 11086 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11095 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11100 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11105 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11110 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StoreIndirect32_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11122 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11127 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect16_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect16_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 679 "src\\PrettyTree.ag" #-}
                                        text "Store indirect 16"
                                        {-# LINE 11139 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11148 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11153 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11158 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11163 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StoreIndirect16_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11175 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11180 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StoreIndirect8_1 :: T_Instruction_1
                       sem_Instruction_StoreIndirect8_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 680 "src\\PrettyTree.ag" #-}
                                        text "Store indirect 8"
                                        {-# LINE 11192 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11201 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11206 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11211 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11216 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StoreIndirect8_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11228 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11233 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_StrictEquals_1 :: T_Instruction_1
                       sem_Instruction_StrictEquals_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 681 "src\\PrettyTree.ag" #-}
                                        text "Equals (strict)"
                                        {-# LINE 11245 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11254 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11259 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11264 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11269 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_StrictEquals_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11281 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11286 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Substract_1 :: T_Instruction_1
                       sem_Instruction_Substract_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 682 "src\\PrettyTree.ag" #-}
                                        text "Substract (number)"
                                        {-# LINE 11298 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11307 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11312 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11317 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11322 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Substract_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11334 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11339 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Substract_i_1 :: T_Instruction_1
                       sem_Instruction_Substract_i_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 683 "src\\PrettyTree.ag" #-}
                                        text "Substract (integer)"
                                        {-# LINE 11351 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11360 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11365 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11370 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11375 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Substract_i_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11387 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11392 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Swap_1 :: T_Instruction_1
                       sem_Instruction_Swap_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 684 "src\\PrettyTree.ag" #-}
                                        text "Swap"
                                        {-# LINE 11404 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11413 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11418 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11423 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11428 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Swap_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11440 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11445 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Throw_1 :: T_Instruction_1
                       sem_Instruction_Throw_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 685 "src\\PrettyTree.ag" #-}
                                        text "Throw"
                                        {-# LINE 11457 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11466 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11471 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11476 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11481 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Throw_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11493 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11498 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Timestamp_1 :: T_Instruction_1
                       sem_Instruction_Timestamp_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 686 "src\\PrettyTree.ag" #-}
                                        text "Timestamp"
                                        {-# LINE 11510 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11519 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11524 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11529 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11534 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Timestamp_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11546 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11551 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_TypeOf_1 :: T_Instruction_1
                       sem_Instruction_TypeOf_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 687 "src\\PrettyTree.ag" #-}
                                        text "Type of"
                                        {-# LINE 11563 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11572 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11577 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11582 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11587 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_TypeOf_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    (\ _lhsIlocation ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11599 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 440 "src\\PrettyTree.ag" #-}
                  Nothing
                  {-# LINE 11604 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Urshift_1 :: T_Instruction_1
                       sem_Instruction_Urshift_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 688 "src\\PrettyTree.ag" #-}
                                        text "Unsigned right shift"
                                        {-# LINE 11616 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _descr | _descr `seq` (True) ->
                                 (case (({-# LINE 442 "src\\PrettyTree.ag" #-}
                                         case _lhsImbLoc of
                                           Nothing -> text (replicate 8 ' ')
                                           Just i  -> let str = show i
                                                          sps = replicate (max 0 (6 - length str))  ' '
                                                      in  text str <> text sps <> text ": "
                                         {-# LINE 11625 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lbl | _lbl `seq` (True) ->
                                  (case (({-# LINE 447 "src\\PrettyTree.ag" #-}
                                          _lbl     <> text "INSTR"
                                          {-# LINE 11630 "src/PrettyTree.hs" #-}
                                          )) of
                                   { _prefix | _prefix `seq` (True) ->
                                   (case (({-# LINE 448 "src\\PrettyTree.ag" #-}
                                           _prefix     <+> _descr
                                           {-# LINE 11635 "src/PrettyTree.hs" #-}
                                           )) of
                                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                    (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                            _lhsIrevLocation
                                            {-# LINE 11640 "src/PrettyTree.hs" #-}
                                            )) of
                                     { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                     ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }))
                   in  sem_Instruction_Urshift_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    (\ _lhsIlocation ->
         (case (({-# LINE 7 "src\\ByteCodeLocationInfo.ag" #-}
                 index_
                 {-# LINE 11653 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 450 "src\\PrettyTree.ag" #-}
                  Just index_
                  {-# LINE 11658 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instruction_Location_1 :: T_Instruction_1
                       sem_Instruction_Location_1 =
                           (\ _lhsIexcptEnv
                              _lhsImbLoc
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 451 "src\\PrettyTree.ag" #-}
                                        empty
                                        {-# LINE 11670 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                 (case (({-# LINE 11 "src\\ByteCodeLocationInfo.ag" #-}
                                         index_
                                         {-# LINE 11675 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                  ( _lhsOoutput,_lhsOrevLocation) }) }))
                   in  sem_Instruction_Location_1)) of
            { ( sem_Instruction_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instruction_1) }) }) }))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = Int ->
                      (Maybe Int) ->
                      ( Int,(Maybe Int),T_Instructions_1)
type T_Instructions_1 = ExceptionDescrs ->
                        Options ->
                        Int ->
                        SymbolTables ->
                        ( Doc,Int)
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIlocation
       _lhsImbLoc ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11705 "src/PrettyTree.hs" #-}
                 )) of
          { _hdOlocation | _hdOlocation `seq` (True) ->
          (case (hd_ _hdOlocation) of
           { ( _hdIlocation,_hdImbLoc,hd_1) | True ->
               (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                       _hdIlocation
                       {-# LINE 11712 "src/PrettyTree.hs" #-}
                       )) of
                { _tlOlocation | _tlOlocation `seq` (True) ->
                (case (({-# LINE 433 "src\\PrettyTree.ag" #-}
                        _hdImbLoc
                        {-# LINE 11717 "src/PrettyTree.hs" #-}
                        )) of
                 { _tlOmbLoc | _tlOmbLoc `seq` (True) ->
                 (case (tl_ _tlOlocation _tlOmbLoc) of
                  { ( _tlIlocation,_tlImbLoc,tl_1) | True ->
                      (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                              _tlIlocation
                              {-# LINE 11724 "src/PrettyTree.hs" #-}
                              )) of
                       { _lhsOlocation | _lhsOlocation `seq` (True) ->
                       (case (({-# LINE 433 "src\\PrettyTree.ag" #-}
                               _tlImbLoc
                               {-# LINE 11729 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
                        (case ((let sem_Instructions_Cons_1 :: T_Instructions_1
                                    sem_Instructions_Cons_1 =
                                        (\ _lhsIexcptEnv
                                           _lhsIopts
                                           _lhsIrevLocation
                                           _lhsItbls ->
                                             (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                     _lhsItbls
                                                     {-# LINE 11740 "src/PrettyTree.hs" #-}
                                                     )) of
                                              { _tlOtbls | _tlOtbls `seq` (True) ->
                                              (case (({-# LINE 465 "src\\PrettyTree.ag" #-}
                                                      _lhsIexcptEnv
                                                      {-# LINE 11745 "src/PrettyTree.hs" #-}
                                                      )) of
                                               { _tlOexcptEnv | _tlOexcptEnv `seq` (True) ->
                                               (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                       _lhsItbls
                                                       {-# LINE 11750 "src/PrettyTree.hs" #-}
                                                       )) of
                                                { _hdOtbls | _hdOtbls `seq` (True) ->
                                                (case (({-# LINE 433 "src\\PrettyTree.ag" #-}
                                                        _lhsImbLoc
                                                        {-# LINE 11755 "src/PrettyTree.hs" #-}
                                                        )) of
                                                 { _hdOmbLoc | _hdOmbLoc `seq` (True) ->
                                                 (case (({-# LINE 465 "src\\PrettyTree.ag" #-}
                                                         _lhsIexcptEnv
                                                         {-# LINE 11760 "src/PrettyTree.hs" #-}
                                                         )) of
                                                  { _hdOexcptEnv | _hdOexcptEnv `seq` (True) ->
                                                  (case (({-# LINE 13 "src\\ByteCodeLocationInfo.ag" #-}
                                                          _lhsIrevLocation
                                                          {-# LINE 11765 "src/PrettyTree.hs" #-}
                                                          )) of
                                                   { _tlOrevLocation | _tlOrevLocation `seq` (True) ->
                                                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                           _lhsIopts
                                                           {-# LINE 11770 "src/PrettyTree.hs" #-}
                                                           )) of
                                                    { _tlOopts | _tlOopts `seq` (True) ->
                                                    (case (tl_1 _tlOexcptEnv _tlOopts _tlOrevLocation _tlOtbls) of
                                                     { ( _tlIoutput,_tlIrevLocation) | True ->
                                                         (case (({-# LINE 14 "src\\ByteCodeLocationInfo.ag" #-}
                                                                 _tlIrevLocation
                                                                 {-# LINE 11777 "src/PrettyTree.hs" #-}
                                                                 )) of
                                                          { _hdOrevLocation | _hdOrevLocation `seq` (True) ->
                                                          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                                  _lhsIopts
                                                                  {-# LINE 11782 "src/PrettyTree.hs" #-}
                                                                  )) of
                                                           { _hdOopts | _hdOopts `seq` (True) ->
                                                           (case (hd_1 _hdOexcptEnv _hdOmbLoc _hdOopts _hdOrevLocation _hdOtbls) of
                                                            { ( _hdIoutput,_hdIrevLocation) | True ->
                                                                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                                        _hdIoutput $+$ _tlIoutput
                                                                        {-# LINE 11789 "src/PrettyTree.hs" #-}
                                                                        )) of
                                                                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                                 (case (({-# LINE 15 "src\\ByteCodeLocationInfo.ag" #-}
                                                                         _hdIrevLocation
                                                                         {-# LINE 11794 "src/PrettyTree.hs" #-}
                                                                         )) of
                                                                  { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                                                  ( _lhsOoutput,_lhsOrevLocation) }) }) }) }) }) }) }) }) }) }) }) }) }))
                                in  sem_Instructions_Cons_1)) of
                         { ( sem_Instructions_1) | True ->
                         ( _lhsOlocation,_lhsOmbLoc,sem_Instructions_1) }) }) }) }) }) }) }) }))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIlocation
       _lhsImbLoc ->
         (case (({-# LINE 6 "src\\ByteCodeLocationInfo.ag" #-}
                 _lhsIlocation
                 {-# LINE 11807 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOlocation | _lhsOlocation `seq` (True) ->
          (case (({-# LINE 433 "src\\PrettyTree.ag" #-}
                  _lhsImbLoc
                  {-# LINE 11812 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOmbLoc | _lhsOmbLoc `seq` (True) ->
           (case ((let sem_Instructions_Nil_1 :: T_Instructions_1
                       sem_Instructions_Nil_1 =
                           (\ _lhsIexcptEnv
                              _lhsIopts
                              _lhsIrevLocation
                              _lhsItbls ->
                                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                        empty
                                        {-# LINE 11823 "src/PrettyTree.hs" #-}
                                        )) of
                                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                 (case (({-# LINE 10 "src\\ByteCodeLocationInfo.ag" #-}
                                         _lhsIrevLocation
                                         {-# LINE 11828 "src/PrettyTree.hs" #-}
                                         )) of
                                  { _lhsOrevLocation | _lhsOrevLocation `seq` (True) ->
                                  ( _lhsOoutput,_lhsOrevLocation) }) }))
                   in  sem_Instructions_Nil_1)) of
            { ( sem_Instructions_1) | True ->
            ( _lhsOlocation,_lhsOmbLoc,sem_Instructions_1) }) }) }))
-- Interfaces --------------------------------------------------
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil list)
-- semantic domain
type T_Interfaces = Options ->
                    SymbolTables ->
                    ( Doc)
sem_Interfaces_Cons :: Word32 ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 11853 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 11858 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _tlIoutput
                        {-# LINE 11865 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 11875 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- ItfV --------------------------------------------------------
-- cata
sem_ItfV :: ItfV ->
            T_ItfV
sem_ItfV (ItfV_Itf _tp) =
    (sem_ItfV_Itf (sem_TypeV _tp))
-- semantic domain
type T_ItfV = PPInfo ->
              ( Doc)
sem_ItfV_Itf :: T_TypeV ->
                T_ItfV
sem_ItfV_Itf tp_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 11894 "src/PrettyTree.hs" #-}
                 )) of
          { _tpOinfo | _tpOinfo `seq` (True) ->
          (case (tp_ _tpOinfo) of
           { ( _tpIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _tpIoutput
                       {-# LINE 11901 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- ItfsV -------------------------------------------------------
-- cata
sem_ItfsV :: ItfsV ->
             T_ItfsV
sem_ItfsV list =
    (Prelude.foldr sem_ItfsV_Cons sem_ItfsV_Nil (Prelude.map sem_ItfV list))
-- semantic domain
type T_ItfsV = PPInfo ->
               ( Doc)
sem_ItfsV_Cons :: T_ItfV ->
                  T_ItfsV ->
                  T_ItfsV
sem_ItfsV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 11921 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 11926 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 11935 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_ItfsV_Nil :: T_ItfsV
sem_ItfsV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 11944 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MbNmV -------------------------------------------------------
-- cata
sem_MbNmV :: MbNmV ->
             T_MbNmV
sem_MbNmV (Prelude.Just x) =
    (sem_MbNmV_Just (sem_NmV x))
sem_MbNmV Prelude.Nothing =
    sem_MbNmV_Nothing
-- semantic domain
type T_MbNmV = PPInfo ->
               ( Bool,Bool,Doc)
sem_MbNmV_Just :: T_NmV ->
                  T_MbNmV
sem_MbNmV_Just just_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 248 "src\\PrettyTree.ag" #-}
                 True
                 {-# LINE 11965 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisDefined | _lhsOisDefined `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 11970 "src/PrettyTree.hs" #-}
                  )) of
           { _justOinfo | _justOinfo `seq` (True) ->
           (case (just_ _justOinfo) of
            { ( _justIisEmpty,_justIoutput) | True ->
                (case (({-# LINE 185 "src\\PrettyTree.ag" #-}
                        _justIisEmpty
                        {-# LINE 11977 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
                 (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                         _justIoutput
                         {-# LINE 11982 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOisDefined,_lhsOisEmpty,_lhsOoutput) }) }) }) }) }))
sem_MbNmV_Nothing :: T_MbNmV
sem_MbNmV_Nothing =
    (\ _lhsIinfo ->
         (case (({-# LINE 249 "src\\PrettyTree.ag" #-}
                 False
                 {-# LINE 11991 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisDefined | _lhsOisDefined `seq` (True) ->
          (case (({-# LINE 188 "src\\PrettyTree.ag" #-}
                  True
                  {-# LINE 11996 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
           (case (({-# LINE 237 "src\\PrettyTree.ag" #-}
                   text "_"
                   {-# LINE 12001 "src/PrettyTree.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOisDefined,_lhsOisEmpty,_lhsOoutput) }) }) }))
-- MbStrV ------------------------------------------------------
-- cata
sem_MbStrV :: MbStrV ->
              T_MbStrV
sem_MbStrV (Prelude.Just x) =
    (sem_MbStrV_Just (sem_StrV x))
sem_MbStrV Prelude.Nothing =
    sem_MbStrV_Nothing
-- semantic domain
type T_MbStrV = PPInfo ->
                ( Bool,Bool,Doc)
sem_MbStrV_Just :: T_StrV ->
                   T_MbStrV
sem_MbStrV_Just just_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 253 "src\\PrettyTree.ag" #-}
                 True
                 {-# LINE 12022 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisDefined | _lhsOisDefined `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 12027 "src/PrettyTree.hs" #-}
                  )) of
           { _justOinfo | _justOinfo `seq` (True) ->
           (case (just_ _justOinfo) of
            { ( _justIisEmpty,_justIoutput) | True ->
                (case (({-# LINE 185 "src\\PrettyTree.ag" #-}
                        _justIisEmpty
                        {-# LINE 12034 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
                 (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                         _justIoutput
                         {-# LINE 12039 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOisDefined,_lhsOisEmpty,_lhsOoutput) }) }) }) }) }))
sem_MbStrV_Nothing :: T_MbStrV
sem_MbStrV_Nothing =
    (\ _lhsIinfo ->
         (case (({-# LINE 254 "src\\PrettyTree.ag" #-}
                 False
                 {-# LINE 12048 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisDefined | _lhsOisDefined `seq` (True) ->
          (case (({-# LINE 187 "src\\PrettyTree.ag" #-}
                  True
                  {-# LINE 12053 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
           (case (({-# LINE 234 "src\\PrettyTree.ag" #-}
                   text "_"
                   {-# LINE 12058 "src/PrettyTree.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOisDefined,_lhsOisEmpty,_lhsOoutput) }) }) }))
-- MbSuperV ----------------------------------------------------
-- cata
sem_MbSuperV :: MbSuperV ->
                T_MbSuperV
sem_MbSuperV (Prelude.Just x) =
    (sem_MbSuperV_Just (sem_SuperV x))
sem_MbSuperV Prelude.Nothing =
    sem_MbSuperV_Nothing
-- semantic domain
type T_MbSuperV = PPInfo ->
                  ( Doc)
sem_MbSuperV_Just :: T_SuperV ->
                     T_MbSuperV
sem_MbSuperV_Just just_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 12079 "src/PrettyTree.hs" #-}
                 )) of
          { _justOinfo | _justOinfo `seq` (True) ->
          (case (just_ _justOinfo) of
           { ( _justIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _justIoutput
                       {-# LINE 12086 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
sem_MbSuperV_Nothing :: T_MbSuperV
sem_MbSuperV_Nothing =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12095 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MetaInfo ----------------------------------------------------
-- cata
sem_MetaInfo :: MetaInfo ->
                T_MetaInfo
sem_MetaInfo (MetaInfo_Info _name _items) =
    (sem_MetaInfo_Info _name (sem_MetaItems _items))
-- semantic domain
type T_MetaInfo = Options ->
                  SymbolTables ->
                  ( Doc)
sem_MetaInfo_Info :: Word32 ->
                     T_MetaItems ->
                     T_MetaInfo
sem_MetaInfo_Info name_ items_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12117 "src/PrettyTree.hs" #-}
                 )) of
          { _itemsOtbls | _itemsOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12122 "src/PrettyTree.hs" #-}
                  )) of
           { _itemsOopts | _itemsOopts `seq` (True) ->
           (case (items_ _itemsOopts _itemsOtbls) of
            { ( _itemsIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _itemsIoutput
                        {-# LINE 12129 "src/PrettyTree.hs" #-}
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
type T_MetaInfos = Options ->
                   SymbolTables ->
                   ( Doc)
sem_MetaInfos_Cons :: T_MetaInfo ->
                      T_MetaInfos ->
                      T_MetaInfos
sem_MetaInfos_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12151 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12156 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 12163 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 12168 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 12175 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_MetaInfos_Nil :: T_MetaInfos
sem_MetaInfos_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12185 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MetaItem ----------------------------------------------------
-- cata
sem_MetaItem :: MetaItem ->
                T_MetaItem
sem_MetaItem (MetaItem_Item _key _value) =
    (sem_MetaItem_Item _key _value)
-- semantic domain
type T_MetaItem = Options ->
                  SymbolTables ->
                  ( Doc)
sem_MetaItem_Item :: Word32 ->
                     Word32 ->
                     T_MetaItem
sem_MetaItem_Item key_ value_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12207 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MetaItems ---------------------------------------------------
-- cata
sem_MetaItems :: MetaItems ->
                 T_MetaItems
sem_MetaItems list =
    (Prelude.foldr sem_MetaItems_Cons sem_MetaItems_Nil (Prelude.map sem_MetaItem list))
-- semantic domain
type T_MetaItems = Options ->
                   SymbolTables ->
                   ( Doc)
sem_MetaItems_Cons :: T_MetaItem ->
                      T_MetaItems ->
                      T_MetaItems
sem_MetaItems_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12229 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12234 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 12241 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 12246 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 12253 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_MetaItems_Nil :: T_MetaItems
sem_MetaItems_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12263 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_MethodFlag = Options ->
                    SymbolTables ->
                    ( Doc)
sem_MethodFlag_NeedArgs :: T_MethodFlag
sem_MethodFlag_NeedArgs =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12293 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MethodFlag_NeedAct :: T_MethodFlag
sem_MethodFlag_NeedAct =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12303 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MethodFlag_NeedRest :: T_MethodFlag
sem_MethodFlag_NeedRest =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12313 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MethodFlag_HasOptionals :: T_MethodFlag
sem_MethodFlag_HasOptionals =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12323 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MethodFlag_SetDXNS :: T_MethodFlag
sem_MethodFlag_SetDXNS =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12333 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MethodFlag_HasParamNames :: T_MethodFlag
sem_MethodFlag_HasParamNames =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12343 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MethodFlags -------------------------------------------------
-- cata
sem_MethodFlags :: MethodFlags ->
                   T_MethodFlags
sem_MethodFlags list =
    (Prelude.foldr sem_MethodFlags_Cons sem_MethodFlags_Nil (Prelude.map sem_MethodFlag list))
-- semantic domain
type T_MethodFlags = Options ->
                     SymbolTables ->
                     ( Doc)
sem_MethodFlags_Cons :: T_MethodFlag ->
                        T_MethodFlags ->
                        T_MethodFlags
sem_MethodFlags_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12365 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12370 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 12377 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 12382 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 12389 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_MethodFlags_Nil :: T_MethodFlags
sem_MethodFlags_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12399 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MethodInfo --------------------------------------------------
-- cata
sem_MethodInfo :: MethodInfo ->
                  T_MethodInfo
sem_MethodInfo (MethodInfo_Info _return _params _name _flags _options _names) =
    (sem_MethodInfo_Info _return (sem_ParamTypes _params) _name (sem_MethodFlags _flags) (sem_Optionals _options) (sem_ParamNames _names))
-- semantic domain
type T_MethodInfo = Options ->
                    SymbolTables ->
                    ( Doc)
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12425 "src/PrettyTree.hs" #-}
                 )) of
          { _optionsOtbls | _optionsOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12430 "src/PrettyTree.hs" #-}
                  )) of
           { _optionsOopts | _optionsOopts `seq` (True) ->
           (case (options_ _optionsOopts _optionsOtbls) of
            { ( _optionsIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 12437 "src/PrettyTree.hs" #-}
                        )) of
                 { _flagsOtbls | _flagsOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 12442 "src/PrettyTree.hs" #-}
                         )) of
                  { _flagsOopts | _flagsOopts `seq` (True) ->
                  (case (flags_ _flagsOopts _flagsOtbls) of
                   { ( _flagsIoutput) | True ->
                       (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                               _lhsItbls
                               {-# LINE 12449 "src/PrettyTree.hs" #-}
                               )) of
                        { _paramsOtbls | _paramsOtbls `seq` (True) ->
                        (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                _lhsIopts
                                {-# LINE 12454 "src/PrettyTree.hs" #-}
                                )) of
                         { _paramsOopts | _paramsOopts `seq` (True) ->
                         (case (params_ _paramsOopts _paramsOtbls) of
                          { ( _paramsIoutput) | True ->
                              (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                      _paramsIoutput $+$ _flagsIoutput $+$ _optionsIoutput
                                      {-# LINE 12461 "src/PrettyTree.hs" #-}
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
type T_MethodInfos = Options ->
                     SymbolTables ->
                     ( Doc)
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12483 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 12488 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 12495 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 12500 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 12507 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12517 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- MethodV -----------------------------------------------------
-- cata
sem_MethodV :: MethodV ->
               T_MethodV
sem_MethodV (MethodV_Method _mbNm _sig) =
    (sem_MethodV_Method (sem_MbStrV _mbNm) (sem_SigV _sig))
-- semantic domain
type T_MethodV = PPInfo ->
                 ( Doc)
sem_MethodV_Method :: T_MbStrV ->
                      T_SigV ->
                      T_MethodV
sem_MethodV_Method mbNm_ sig_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 240 "src\\PrettyTree.ag" #-}
                 Short
                 {-# LINE 12537 "src/PrettyTree.hs" #-}
                 )) of
          { _mbNmOinfo | _mbNmOinfo `seq` (True) ->
          (case (({-# LINE 241 "src\\PrettyTree.ag" #-}
                  Short
                  {-# LINE 12542 "src/PrettyTree.hs" #-}
                  )) of
           { _sigOinfo | _sigOinfo `seq` (True) ->
           (case (sig_ _sigOinfo) of
            { ( _sigIoutput) | True ->
                (case (mbNm_ _mbNmOinfo) of
                 { ( _mbNmIisDefined,_mbNmIisEmpty,_mbNmIoutput) | True ->
                     (case (({-# LINE 242 "src\\PrettyTree.ag" #-}
                             if _mbNmIisDefined
                             then _mbNmIoutput <> _sigIoutput
                             else _sigIoutput
                             {-# LINE 12553 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
-- MethodsV ----------------------------------------------------
-- cata
sem_MethodsV :: MethodsV ->
                T_MethodsV
sem_MethodsV list =
    (Prelude.foldr sem_MethodsV_Cons sem_MethodsV_Nil (Prelude.map sem_MethodV list))
-- semantic domain
type T_MethodsV = PPInfo ->
                  ( Doc)
sem_MethodsV_Cons :: T_MethodV ->
                     T_MethodsV ->
                     T_MethodsV
sem_MethodsV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 12573 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (tl_ _tlOinfo) of
           { ( _tlIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _lhsIinfo
                       {-# LINE 12580 "src/PrettyTree.hs" #-}
                       )) of
                { _hdOinfo | _hdOinfo `seq` (True) ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 12587 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_MethodsV_Nil :: T_MethodsV
sem_MethodsV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12596 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_MultinameInfos = Word32 ->
                        Options ->
                        SymbolTables ->
                        ( Doc)
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12695 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 164 "src\\PrettyTree.ag" #-}
                  nameView _lhsItbls (Ref _lhsIindex)
                  {-# LINE 12700 "src/PrettyTree.hs" #-}
                  )) of
           { pp_val_ | pp_val_ `seq` (True) ->
           (case ((sem_NmV pp_val_)) of
            { pp_inst_ | pp_inst_ `seq` (True) ->
            (case (({-# LINE 165 "src\\PrettyTree.ag" #-}
                    Verbose
                    {-# LINE 12707 "src/PrettyTree.hs" #-}
                    )) of
             { _ppOinfo | _ppOinfo `seq` (True) ->
             (case (({-# LINE 147 "src\\PrettyTree.ag" #-}
                     1 + _lhsIindex
                     {-# LINE 12712 "src/PrettyTree.hs" #-}
                     )) of
              { _tlOindex | _tlOindex `seq` (True) ->
              (case (pp_inst_ _ppOinfo) of
               { ( _ppIisEmpty,_ppIoutput) | True ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 12719 "src/PrettyTree.hs" #-}
                           )) of
                    { _tlOopts | _tlOopts `seq` (True) ->
                    (case (tl_ _tlOindex _tlOopts _tlOtbls) of
                     { ( _tlIoutput) | True ->
                         (case (({-# LINE 155 "src\\PrettyTree.ag" #-}
                                 num _lhsIindex <+> text "->" <+> _ppIoutput $+$ _tlIoutput
                                 {-# LINE 12726 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12737 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_MultinameKind = Options ->
                       SymbolTables ->
                       ( Doc)
sem_MultinameKind_QName :: T_MultinameKind
sem_MultinameKind_QName =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12777 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_QNameA :: T_MultinameKind
sem_MultinameKind_QNameA =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12787 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_RTQName :: T_MultinameKind
sem_MultinameKind_RTQName =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12797 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_RTQNameA :: T_MultinameKind
sem_MultinameKind_RTQNameA =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12807 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_RTQNameL :: T_MultinameKind
sem_MultinameKind_RTQNameL =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12817 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_RTQNameLA :: T_MultinameKind
sem_MultinameKind_RTQNameLA =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12827 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_Multiname :: T_MultinameKind
sem_MultinameKind_Multiname =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12837 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_MultinameA :: T_MultinameKind
sem_MultinameKind_MultinameA =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12847 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_MultinameL :: T_MultinameKind
sem_MultinameKind_MultinameL =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12857 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_MultinameLA :: T_MultinameKind
sem_MultinameKind_MultinameLA =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12867 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_MultinameKind_Generic :: T_MultinameKind
sem_MultinameKind_Generic =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12877 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_NamespaceInfos = Word32 ->
                        Options ->
                        SymbolTables ->
                        ( Doc)
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 12914 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 159 "src\\PrettyTree.ag" #-}
                  namespaceView _lhsItbls (Ref _lhsIindex)
                  {-# LINE 12919 "src/PrettyTree.hs" #-}
                  )) of
           { pp_val_ | pp_val_ `seq` (True) ->
           (case ((sem_NsV pp_val_)) of
            { pp_inst_ | pp_inst_ `seq` (True) ->
            (case (({-# LINE 160 "src\\PrettyTree.ag" #-}
                    Verbose
                    {-# LINE 12926 "src/PrettyTree.hs" #-}
                    )) of
             { _ppOinfo | _ppOinfo `seq` (True) ->
             (case (({-# LINE 145 "src\\PrettyTree.ag" #-}
                     1 + _lhsIindex
                     {-# LINE 12931 "src/PrettyTree.hs" #-}
                     )) of
              { _tlOindex | _tlOindex `seq` (True) ->
              (case (pp_inst_ _ppOinfo) of
               { ( _ppIisEmpty,_ppIoutput) | True ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 12938 "src/PrettyTree.hs" #-}
                           )) of
                    { _tlOopts | _tlOopts `seq` (True) ->
                    (case (tl_ _tlOindex _tlOopts _tlOtbls) of
                     { ( _tlIoutput) | True ->
                         (case (({-# LINE 153 "src\\PrettyTree.ag" #-}
                                 num _lhsIindex <+> text "->" <+> _ppIoutput $+$ _tlIoutput
                                 {-# LINE 12945 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 12956 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_NamespaceNames = Options ->
                        SymbolTables ->
                        ( Doc)
sem_NamespaceNames_Cons :: Word32 ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13019 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 173 "src\\PrettyTree.ag" #-}
                  namespaceView _lhsItbls (Ref hd_)
                  {-# LINE 13024 "src/PrettyTree.hs" #-}
                  )) of
           { pp_val_ | pp_val_ `seq` (True) ->
           (case ((sem_NsV pp_val_)) of
            { pp_inst_ | pp_inst_ `seq` (True) ->
            (case (({-# LINE 174 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 13031 "src/PrettyTree.hs" #-}
                    )) of
             { _ppOinfo | _ppOinfo `seq` (True) ->
             (case (pp_inst_ _ppOinfo) of
              { ( _ppIisEmpty,_ppIoutput) | True ->
                  (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                          _lhsIopts
                          {-# LINE 13038 "src/PrettyTree.hs" #-}
                          )) of
                   { _tlOopts | _tlOopts `seq` (True) ->
                   (case (tl_ _tlOopts _tlOtbls) of
                    { ( _tlIoutput) | True ->
                        (case (({-# LINE 171 "src\\PrettyTree.ag" #-}
                                _ppIoutput <+> braces (text "ns-id:" <+> num hd_) <+> _tlIoutput
                                {-# LINE 13045 "src/PrettyTree.hs" #-}
                                )) of
                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                         ( _lhsOoutput) }) }) }) }) }) }) }) }))
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13055 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- NmV ---------------------------------------------------------
-- cata
sem_NmV :: NmV ->
           T_NmV
sem_NmV (NmV_Qual _id _ns _nm) =
    (sem_NmV_Qual _id (sem_NsV _ns) (sem_StrV _nm))
sem_NmV (NmV_Quals _id _set _nm) =
    (sem_NmV_Quals _id (sem_NsSetV _set) (sem_StrV _nm))
sem_NmV (NmV_Other _id) =
    (sem_NmV_Other _id)
-- semantic domain
type T_NmV = PPInfo ->
             ( Bool,Doc)
sem_NmV_Qual :: NameRef ->
                T_NsV ->
                T_StrV ->
                T_NmV
sem_NmV_Qual id_ ns_ nm_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13080 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (nm_ _nmOinfo) of
           { ( _nmIisEmpty,_nmIoutput) | True ->
               (case (({-# LINE 190 "src\\PrettyTree.ag" #-}
                       _nmIisEmpty
                       {-# LINE 13087 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
                (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                        _lhsIinfo
                        {-# LINE 13092 "src/PrettyTree.hs" #-}
                        )) of
                 { _nsOinfo | _nsOinfo `seq` (True) ->
                 (case (({-# LINE 202 "src\\PrettyTree.ag" #-}
                         if _nmIisEmpty
                         then text "*"
                         else _nmIoutput
                         {-# LINE 13099 "src/PrettyTree.hs" #-}
                         )) of
                  { _nmOut | _nmOut `seq` (True) ->
                  (case (ns_ _nsOinfo) of
                   { ( _nsIisEmpty,_nsIoutput) | True ->
                       (case (({-# LINE 205 "src\\PrettyTree.ag" #-}
                               case _lhsIinfo of
                                 Short   -> if _nsIisEmpty
                                            then _nmOut
                                            else _nsIoutput <> text ":" <> _nmOut
                                 Verbose -> props [ ("namespace", _nsIoutput), ("name", _nmIoutput) ]
                               {-# LINE 13110 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOisEmpty,_lhsOoutput) }) }) }) }) }) }) }))
sem_NmV_Quals :: NameRef ->
                 T_NsSetV ->
                 T_StrV ->
                 T_NmV
sem_NmV_Quals id_ set_ nm_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 191 "src\\PrettyTree.ag" #-}
                 False
                 {-# LINE 13122 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
          (case (({-# LINE 211 "src\\PrettyTree.ag" #-}
                  Short
                  {-# LINE 13127 "src/PrettyTree.hs" #-}
                  )) of
           { _info | _info `seq` (True) ->
           (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                   _info
                   {-# LINE 13132 "src/PrettyTree.hs" #-}
                   )) of
            { _nmOinfo | _nmOinfo `seq` (True) ->
            (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                    _info
                    {-# LINE 13137 "src/PrettyTree.hs" #-}
                    )) of
             { _setOinfo | _setOinfo `seq` (True) ->
             (case (nm_ _nmOinfo) of
              { ( _nmIisEmpty,_nmIoutput) | True ->
                  (case (set_ _setOinfo) of
                   { ( _setIoutput) | True ->
                       (case (({-# LINE 210 "src\\PrettyTree.ag" #-}
                               _setIoutput <> text ":" <> _nmIoutput
                               {-# LINE 13146 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOisEmpty,_lhsOoutput) }) }) }) }) }) }) }))
sem_NmV_Other :: NameRef ->
                 T_NmV
sem_NmV_Other id_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 189 "src\\PrettyTree.ag" #-}
                 False
                 {-# LINE 13156 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
          (case (({-# LINE 212 "src\\PrettyTree.ag" #-}
                  text "name/other" <> parens (text $ show $ refVal id_)
                  {-# LINE 13161 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOisEmpty,_lhsOoutput) }) }))
-- NmsV --------------------------------------------------------
-- cata
sem_NmsV :: NmsV ->
            T_NmsV
sem_NmsV list =
    (Prelude.foldr sem_NmsV_Cons sem_NmsV_Nil (Prelude.map sem_NmV list))
-- semantic domain
type T_NmsV = PPInfo ->
              ( Doc)
sem_NmsV_Cons :: T_NmV ->
                 T_NmsV ->
                 T_NmsV
sem_NmsV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13181 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 13186 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIisEmpty,_hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 13195 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_NmsV_Nil :: T_NmsV
sem_NmsV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13204 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- NsSetV ------------------------------------------------------
-- cata
sem_NsSetV :: NsSetV ->
              T_NsSetV
sem_NsSetV (NsSetV_Set _id _spaces) =
    (sem_NsSetV_Set _id (sem_NssV _spaces))
-- semantic domain
type T_NsSetV = PPInfo ->
                ( Doc)
sem_NsSetV_Set :: NamesetRef ->
                  T_NssV ->
                  T_NsSetV
sem_NsSetV_Set id_ spaces_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13224 "src/PrettyTree.hs" #-}
                 )) of
          { _spacesOinfo | _spacesOinfo `seq` (True) ->
          (case (spaces_ _spacesOinfo) of
           { ( _spacesIoutput) | True ->
               (case (({-# LINE 222 "src\\PrettyTree.ag" #-}
                       braces _spacesIoutput
                       {-# LINE 13231 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- NsV ---------------------------------------------------------
-- cata
sem_NsV :: NsV ->
           T_NsV
sem_NsV (NsV_Ns _id _nm) =
    (sem_NsV_Ns _id (sem_StrV _nm))
-- semantic domain
type T_NsV = PPInfo ->
             ( Bool,Doc)
sem_NsV_Ns :: NamespaceRef ->
              T_StrV ->
              T_NsV
sem_NsV_Ns id_ nm_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13251 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (nm_ _nmOinfo) of
           { ( _nmIisEmpty,_nmIoutput) | True ->
               (case (({-# LINE 192 "src\\PrettyTree.ag" #-}
                       _nmIisEmpty
                       {-# LINE 13258 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
                (case (({-# LINE 215 "src\\PrettyTree.ag" #-}
                        if (refVal id_) == 0
                        then text "*"
                        else case _lhsIinfo of
                               Short   -> _nmIoutput
                               Verbose -> _nmIoutput <+> braces (text "ns-id:" <+> num (refVal id_))
                        {-# LINE 13267 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOisEmpty,_lhsOoutput) }) }) }) }))
-- NssV --------------------------------------------------------
-- cata
sem_NssV :: NssV ->
            T_NssV
sem_NssV list =
    (Prelude.foldr sem_NssV_Cons sem_NssV_Nil (Prelude.map sem_NsV list))
-- semantic domain
type T_NssV = PPInfo ->
              ( Doc)
sem_NssV_Cons :: T_NsV ->
                 T_NssV ->
                 T_NssV
sem_NssV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13287 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 13292 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIisEmpty,_hdIoutput) | True ->
                     (case (({-# LINE 225 "src\\PrettyTree.ag" #-}
                             _hdIoutput <+> _tlIoutput
                             {-# LINE 13301 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_NssV_Nil :: T_NssV
sem_NssV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 226 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13310 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- Optional ----------------------------------------------------
-- cata
sem_Optional :: Optional ->
                T_Optional
sem_Optional (Optional_Detail _val _kind) =
    (sem_Optional_Detail _val (sem_ValueKind _kind))
-- semantic domain
type T_Optional = Options ->
                  SymbolTables ->
                  ( Doc)
sem_Optional_Detail :: Word32 ->
                       T_ValueKind ->
                       T_Optional
sem_Optional_Detail val_ kind_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13332 "src/PrettyTree.hs" #-}
                 )) of
          { _kindOtbls | _kindOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 13337 "src/PrettyTree.hs" #-}
                  )) of
           { _kindOopts | _kindOopts `seq` (True) ->
           (case (kind_ _kindOopts _kindOtbls) of
            { ( _kindIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _kindIoutput
                        {-# LINE 13344 "src/PrettyTree.hs" #-}
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
type T_Optionals = Options ->
                   SymbolTables ->
                   ( Doc)
sem_Optionals_Cons :: T_Optional ->
                      T_Optionals ->
                      T_Optionals
sem_Optionals_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13366 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 13371 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 13378 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 13383 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 13390 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_Optionals_Nil :: T_Optionals
sem_Optionals_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13400 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_ParamTypes = Options ->
                    SymbolTables ->
                    ( Doc)
sem_ParamTypes_Cons :: Word32 ->
                       T_ParamTypes ->
                       T_ParamTypes
sem_ParamTypes_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13438 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 13443 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _tlIoutput
                        {-# LINE 13450 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13460 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- ParamV ------------------------------------------------------
-- cata
sem_ParamV :: ParamV ->
              T_ParamV
sem_ParamV (ParamV_Param _mbNm _tp) =
    (sem_ParamV_Param (sem_MbStrV _mbNm) (sem_TypeV _tp))
-- semantic domain
type T_ParamV = PPInfo ->
                ( Doc)
sem_ParamV_Param :: T_MbStrV ->
                    T_TypeV ->
                    T_ParamV
sem_ParamV_Param mbNm_ tp_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13480 "src/PrettyTree.hs" #-}
                 )) of
          { _tpOinfo | _tpOinfo `seq` (True) ->
          (case (tp_ _tpOinfo) of
           { ( _tpIoutput) | True ->
               (case (({-# LINE 266 "src\\PrettyTree.ag" #-}
                       _tpIoutput
                       {-# LINE 13487 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- ParamsV -----------------------------------------------------
-- cata
sem_ParamsV :: ParamsV ->
               T_ParamsV
sem_ParamsV list =
    (Prelude.foldr sem_ParamsV_Cons sem_ParamsV_Nil (Prelude.map sem_ParamV list))
-- semantic domain
type T_ParamsV = PPInfo ->
                 ( ([Doc]))
sem_ParamsV_Cons :: T_ParamV ->
                    T_ParamsV ->
                    T_ParamsV
sem_ParamsV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 199 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 13507 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 13512 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 262 "src\\PrettyTree.ag" #-}
                             _hdIoutput : _tlIoutput
                             {-# LINE 13521 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_ParamsV_Nil :: T_ParamsV
sem_ParamsV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 263 "src\\PrettyTree.ag" #-}
                 []
                 {-# LINE 13530 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = Word32 ->
                     Options ->
                     SymbolTables ->
                     ( Doc)
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 143 "src\\PrettyTree.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 13554 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13559 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOtbls | _tlOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 13564 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOindex _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 151 "src\\PrettyTree.ag" #-}
                         num _lhsIindex <+> text "->" <+> num hd_ $+$ _tlIoutput
                         {-# LINE 13571 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13582 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = Options ->
                  SymbolTables ->
                  ( Doc)
sem_PoolInfo_Info :: T_PoolInts ->
                     T_PoolUInts ->
                     T_PoolDoubles ->
                     T_PoolStrings ->
                     T_NamespaceInfos ->
                     T_SetInfos ->
                     T_MultinameInfos ->
                     T_PoolInfo
sem_PoolInfo_Info integers_ uintegers_ doubles_ strings_ namespaces_ namesets_ multinames_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13609 "src/PrettyTree.hs" #-}
                 )) of
          { _multinamesOtbls | _multinamesOtbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13614 "src/PrettyTree.hs" #-}
                  )) of
           { _namesetsOtbls | _namesetsOtbls `seq` (True) ->
           (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                   _lhsItbls
                   {-# LINE 13619 "src/PrettyTree.hs" #-}
                   )) of
            { _namespacesOtbls | _namespacesOtbls `seq` (True) ->
            (case (({-# LINE 139 "src\\PrettyTree.ag" #-}
                    1
                    {-# LINE 13624 "src/PrettyTree.hs" #-}
                    )) of
             { _multinamesOindex | _multinamesOindex `seq` (True) ->
             (case (({-# LINE 138 "src\\PrettyTree.ag" #-}
                     1
                     {-# LINE 13629 "src/PrettyTree.hs" #-}
                     )) of
              { _namesetsOindex | _namesetsOindex `seq` (True) ->
              (case (({-# LINE 137 "src\\PrettyTree.ag" #-}
                      1
                      {-# LINE 13634 "src/PrettyTree.hs" #-}
                      )) of
               { _namespacesOindex | _namespacesOindex `seq` (True) ->
               (case (({-# LINE 136 "src\\PrettyTree.ag" #-}
                       1
                       {-# LINE 13639 "src/PrettyTree.hs" #-}
                       )) of
                { _stringsOindex | _stringsOindex `seq` (True) ->
                (case (({-# LINE 135 "src\\PrettyTree.ag" #-}
                        1
                        {-# LINE 13644 "src/PrettyTree.hs" #-}
                        )) of
                 { _doublesOindex | _doublesOindex `seq` (True) ->
                 (case (({-# LINE 134 "src\\PrettyTree.ag" #-}
                         1
                         {-# LINE 13649 "src/PrettyTree.hs" #-}
                         )) of
                  { _uintegersOindex | _uintegersOindex `seq` (True) ->
                  (case (({-# LINE 133 "src\\PrettyTree.ag" #-}
                          1
                          {-# LINE 13654 "src/PrettyTree.hs" #-}
                          )) of
                   { _integersOindex | _integersOindex `seq` (True) ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 13659 "src/PrettyTree.hs" #-}
                           )) of
                    { _multinamesOopts | _multinamesOopts `seq` (True) ->
                    (case (multinames_ _multinamesOindex _multinamesOopts _multinamesOtbls) of
                     { ( _multinamesIoutput) | True ->
                         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                 _lhsIopts
                                 {-# LINE 13666 "src/PrettyTree.hs" #-}
                                 )) of
                          { _namesetsOopts | _namesetsOopts `seq` (True) ->
                          (case (namesets_ _namesetsOindex _namesetsOopts _namesetsOtbls) of
                           { ( _namesetsIoutput) | True ->
                               (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                       _lhsIopts
                                       {-# LINE 13673 "src/PrettyTree.hs" #-}
                                       )) of
                                { _namespacesOopts | _namespacesOopts `seq` (True) ->
                                (case (namespaces_ _namespacesOindex _namespacesOopts _namespacesOtbls) of
                                 { ( _namespacesIoutput) | True ->
                                     (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                             _lhsItbls
                                             {-# LINE 13680 "src/PrettyTree.hs" #-}
                                             )) of
                                      { _stringsOtbls | _stringsOtbls `seq` (True) ->
                                      (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                              _lhsIopts
                                              {-# LINE 13685 "src/PrettyTree.hs" #-}
                                              )) of
                                       { _stringsOopts | _stringsOopts `seq` (True) ->
                                       (case (strings_ _stringsOindex _stringsOopts _stringsOtbls) of
                                        { ( _stringsIoutput) | True ->
                                            (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                    _lhsItbls
                                                    {-# LINE 13692 "src/PrettyTree.hs" #-}
                                                    )) of
                                             { _doublesOtbls | _doublesOtbls `seq` (True) ->
                                             (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                     _lhsIopts
                                                     {-# LINE 13697 "src/PrettyTree.hs" #-}
                                                     )) of
                                              { _doublesOopts | _doublesOopts `seq` (True) ->
                                              (case (doubles_ _doublesOindex _doublesOopts _doublesOtbls) of
                                               { ( _doublesIoutput) | True ->
                                                   (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                           _lhsItbls
                                                           {-# LINE 13704 "src/PrettyTree.hs" #-}
                                                           )) of
                                                    { _uintegersOtbls | _uintegersOtbls `seq` (True) ->
                                                    (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                            _lhsIopts
                                                            {-# LINE 13709 "src/PrettyTree.hs" #-}
                                                            )) of
                                                     { _uintegersOopts | _uintegersOopts `seq` (True) ->
                                                     (case (uintegers_ _uintegersOindex _uintegersOopts _uintegersOtbls) of
                                                      { ( _uintegersIoutput) | True ->
                                                          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                                                                  _lhsItbls
                                                                  {-# LINE 13716 "src/PrettyTree.hs" #-}
                                                                  )) of
                                                           { _integersOtbls | _integersOtbls `seq` (True) ->
                                                           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                                                   _lhsIopts
                                                                   {-# LINE 13721 "src/PrettyTree.hs" #-}
                                                                   )) of
                                                            { _integersOopts | _integersOopts `seq` (True) ->
                                                            (case (integers_ _integersOindex _integersOopts _integersOtbls) of
                                                             { ( _integersIoutput) | True ->
                                                                 (case (({-# LINE 121 "src\\PrettyTree.ag" #-}
                                                                         vert $ map (\(k,v) -> text "TABLE" <+> text k $+$ nest 2 v)
                                                                           [ ("integers",   _integersIoutput)
                                                                           , ("uintegers",  _uintegersIoutput)
                                                                           , ("doubles",    _doublesIoutput)
                                                                           , ("strings",    _stringsIoutput)
                                                                           , ("namespaces", _namespacesIoutput)
                                                                           , ("namesets",   _namesetsIoutput)
                                                                           , ("multinames", _multinamesIoutput)
                                                                           ]
                                                                         {-# LINE 13736 "src/PrettyTree.hs" #-}
                                                                         )) of
                                                                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                                                  ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = Word32 ->
                  Options ->
                  SymbolTables ->
                  ( Doc)
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 141 "src\\PrettyTree.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 13760 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13765 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOtbls | _tlOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 13770 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOindex _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 149 "src\\PrettyTree.ag" #-}
                         num _lhsIindex <+> text "->" <+> num hd_ $+$ _tlIoutput
                         {-# LINE 13777 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13788 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = Word32 ->
                     Options ->
                     SymbolTables ->
                     ( Doc)
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 144 "src\\PrettyTree.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 13812 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13817 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOtbls | _tlOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 13822 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOindex _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 152 "src\\PrettyTree.ag" #-}
                         num _lhsIindex <+> text "->" <+> str hd_ $+$ _tlIoutput
                         {-# LINE 13829 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13840 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = Word32 ->
                   Options ->
                   SymbolTables ->
                   ( Doc)
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 142 "src\\PrettyTree.ag" #-}
                 1 + _lhsIindex
                 {-# LINE 13864 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOindex | _tlOindex `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13869 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOtbls | _tlOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 13874 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOindex _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 150 "src\\PrettyTree.ag" #-}
                         num _lhsIindex <+> text "->" <+> num hd_ $+$ _tlIoutput
                         {-# LINE 13881 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13892 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- Rect --------------------------------------------------------
-- cata
sem_Rect :: Rect ->
            T_Rect
sem_Rect (Rect_Rect _bits _xMin _xMax _yMin _yMax) =
    (sem_Rect_Rect _bits _xMin _xMax _yMin _yMax)
-- semantic domain
type T_Rect = Options ->
              ( Doc)
sem_Rect_Rect :: Int ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 T_Rect
sem_Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_ =
    (\ _lhsIopts ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 13915 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- ScriptInfo --------------------------------------------------
-- cata
sem_ScriptInfo :: ScriptInfo ->
                  T_ScriptInfo
sem_ScriptInfo (ScriptInfo_Info _method _traits) =
    (sem_ScriptInfo_Info _method (sem_Traits _traits))
-- semantic domain
type T_ScriptInfo = Options ->
                    SymbolTables ->
                    ( Doc)
sem_ScriptInfo_Info :: Word32 ->
                       T_Traits ->
                       T_ScriptInfo
sem_ScriptInfo_Info method_ traits_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13937 "src/PrettyTree.hs" #-}
                 )) of
          { _traitsOtbls | _traitsOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 13942 "src/PrettyTree.hs" #-}
                  )) of
           { _traitsOopts | _traitsOopts `seq` (True) ->
           (case (traits_ _traitsOopts _traitsOtbls) of
            { ( _traitsIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _traitsIoutput
                        {-# LINE 13949 "src/PrettyTree.hs" #-}
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
type T_ScriptInfos = Options ->
                     SymbolTables ->
                     ( Doc)
sem_ScriptInfos_Cons :: T_ScriptInfo ->
                        T_ScriptInfos ->
                        T_ScriptInfos
sem_ScriptInfos_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 13971 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 13976 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOtbls | _hdOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 13981 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 13988 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 13995 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_ScriptInfos_Nil :: T_ScriptInfos
sem_ScriptInfos_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 14005 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- SetInfo -----------------------------------------------------
-- cata
sem_SetInfo :: SetInfo ->
               T_SetInfo
sem_SetInfo (SetInfo_Info _names) =
    (sem_SetInfo_Info (sem_NamespaceNames _names))
-- semantic domain
type T_SetInfo = Options ->
                 SymbolTables ->
                 ( Doc)
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 14026 "src/PrettyTree.hs" #-}
                 )) of
          { _namesOtbls | _namesOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 14031 "src/PrettyTree.hs" #-}
                  )) of
           { _namesOopts | _namesOopts `seq` (True) ->
           (case (names_ _namesOopts _namesOtbls) of
            { ( _namesIoutput) | True ->
                (case (({-# LINE 168 "src\\PrettyTree.ag" #-}
                        text "SET" <+> _namesIoutput
                        {-# LINE 14038 "src/PrettyTree.hs" #-}
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
type T_SetInfos = Word32 ->
                  Options ->
                  SymbolTables ->
                  ( Doc)
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 14062 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 14067 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOtbls | _hdOtbls `seq` (True) ->
           (case (({-# LINE 146 "src\\PrettyTree.ag" #-}
                   1 + _lhsIindex
                   {-# LINE 14072 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOindex | _tlOindex `seq` (True) ->
            (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                    _lhsIopts
                    {-# LINE 14077 "src/PrettyTree.hs" #-}
                    )) of
             { _tlOopts | _tlOopts `seq` (True) ->
             (case (tl_ _tlOindex _tlOopts _tlOtbls) of
              { ( _tlIoutput) | True ->
                  (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                          _lhsIopts
                          {-# LINE 14084 "src/PrettyTree.hs" #-}
                          )) of
                   { _hdOopts | _hdOopts `seq` (True) ->
                   (case (hd_ _hdOopts _hdOtbls) of
                    { ( _hdIoutput) | True ->
                        (case (({-# LINE 154 "src\\PrettyTree.ag" #-}
                                num _lhsIindex <+> text "->" <+> _hdIoutput $+$ _tlIoutput
                                {-# LINE 14091 "src/PrettyTree.hs" #-}
                                )) of
                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                         ( _lhsOoutput) }) }) }) }) }) }) }) }))
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    (\ _lhsIindex
       _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 14102 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- SigV --------------------------------------------------------
-- cata
sem_SigV :: SigV ->
            T_SigV
sem_SigV (SigV_Sig _id _ret _params) =
    (sem_SigV_Sig _id (sem_TypeV _ret) (sem_ParamsV _params))
-- semantic domain
type T_SigV = PPInfo ->
              ( Doc)
sem_SigV_Sig :: MethodRef ->
                T_TypeV ->
                T_ParamsV ->
                T_SigV
sem_SigV_Sig id_ ret_ params_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 258 "src\\PrettyTree.ag" #-}
                 Short
                 {-# LINE 14123 "src/PrettyTree.hs" #-}
                 )) of
          { _retOinfo | _retOinfo `seq` (True) ->
          (case (({-# LINE 257 "src\\PrettyTree.ag" #-}
                  Short
                  {-# LINE 14128 "src/PrettyTree.hs" #-}
                  )) of
           { _paramsOinfo | _paramsOinfo `seq` (True) ->
           (case (params_ _paramsOinfo) of
            { ( _paramsIoutput) | True ->
                (case (ret_ _retOinfo) of
                 { ( _retIoutput) | True ->
                     (case (({-# LINE 259 "src\\PrettyTree.ag" #-}
                             parens (hsep $ punctuate (text ",") _paramsIoutput) <> text ":" <> _retIoutput
                             {-# LINE 14137 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
-- StrV --------------------------------------------------------
-- cata
sem_StrV :: StrV ->
            T_StrV
sem_StrV (StrV_Str _id _val) =
    (sem_StrV_Str _id _val)
-- semantic domain
type T_StrV = PPInfo ->
              ( Bool,Doc)
sem_StrV_Str :: StringRef ->
                String ->
                T_StrV
sem_StrV_Str id_ val_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 186 "src\\PrettyTree.ag" #-}
                 null val_
                 {-# LINE 14157 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOisEmpty | _lhsOisEmpty `seq` (True) ->
          (case (({-# LINE 229 "src\\PrettyTree.ag" #-}
                  case _lhsIinfo of
                    Short   -> text val_
                    Verbose -> text val_ <+> braces (text "str-id:" <+> num (refVal id_))
                  {-# LINE 14164 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOoutput | _lhsOoutput `seq` (True) ->
           ( _lhsOisEmpty,_lhsOoutput) }) }))
-- SuperV ------------------------------------------------------
-- cata
sem_SuperV :: SuperV ->
              T_SuperV
sem_SuperV (SuperV_Super _tp) =
    (sem_SuperV_Super (sem_TypeV _tp))
-- semantic domain
type T_SuperV = PPInfo ->
                ( Doc)
sem_SuperV_Super :: T_TypeV ->
                    T_SuperV
sem_SuperV_Super tp_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 14183 "src/PrettyTree.hs" #-}
                 )) of
          { _tpOinfo | _tpOinfo `seq` (True) ->
          (case (tp_ _tpOinfo) of
           { ( _tpIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _tpIoutput
                       {-# LINE 14190 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = Options ->
                 ([SymbolTables]) ->
                 ( Doc,([SymbolTables]))
data Inh_SwfFile = Inh_SwfFile {opts_Inh_SwfFile :: !(Options),tbls_Inh_SwfFile :: !(([SymbolTables]))}
data Syn_SwfFile = Syn_SwfFile {output_Syn_SwfFile :: !(Doc),tbls_Syn_SwfFile :: !(([SymbolTables]))}
wrap_SwfFile :: T_SwfFile ->
                Inh_SwfFile ->
                Syn_SwfFile
wrap_SwfFile sem (Inh_SwfFile _lhsIopts _lhsItbls) =
    (let ( _lhsOoutput,_lhsOtbls) | True = sem _lhsIopts _lhsItbls
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
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 14225 "src/PrettyTree.hs" #-}
                 )) of
          { _tagsOtbls | _tagsOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 14230 "src/PrettyTree.hs" #-}
                  )) of
           { _tagsOopts | _tagsOopts `seq` (True) ->
           (case (tags_ _tagsOopts _tagsOtbls) of
            { ( _tagsIoutput,_tagsItbls) | True ->
                (case (({-# LINE 43 "src\\PrettyTree.ag" #-}
                        text "SWF" <+> space <+> props
                          [ ("compressed", bool compressed_)
                          , ("version",    num version_)
                          , ("length",     num length_)
                          ]
                        $+$ _tagsIoutput
                        {-# LINE 14242 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                         _tagsItbls
                         {-# LINE 14247 "src/PrettyTree.hs" #-}
                         )) of
                  { _lhsOtbls | _lhsOtbls `seq` (True) ->
                  ( _lhsOoutput,_lhsOtbls) }) }) }) }) }))
-- TableV ------------------------------------------------------
-- cata
sem_TableV :: TableV ->
              T_TableV
sem_TableV (TableV_Table _classes _methods) =
    (sem_TableV_Table (sem_ClassesV _classes) (sem_MethodsV _methods))
-- semantic domain
type T_TableV = PPInfo ->
                ( Doc)
sem_TableV_Table :: T_ClassesV ->
                    T_MethodsV ->
                    T_TableV
sem_TableV_Table classes_ methods_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 14267 "src/PrettyTree.hs" #-}
                 )) of
          { _classesOinfo | _classesOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 14272 "src/PrettyTree.hs" #-}
                  )) of
           { _methodsOinfo | _methodsOinfo `seq` (True) ->
           (case (methods_ _methodsOinfo) of
            { ( _methodsIoutput) | True ->
                (case (classes_ _classesOinfo) of
                 { ( _classesIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _classesIoutput $+$ _methodsIoutput
                             {-# LINE 14281 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
-- TablesV -----------------------------------------------------
-- cata
sem_TablesV :: TablesV ->
               T_TablesV
sem_TablesV list =
    (Prelude.foldr sem_TablesV_Cons sem_TablesV_Nil (Prelude.map sem_TableV list))
-- semantic domain
type T_TablesV = PPInfo ->
                 ( Doc)
sem_TablesV_Cons :: T_TableV ->
                    T_TablesV ->
                    T_TablesV
sem_TablesV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 14301 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 14306 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 14315 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_TablesV_Nil :: T_TablesV
sem_TablesV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 14324 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_Tag = Options ->
             ([SymbolTables]) ->
             ( Doc,([SymbolTables]))
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 14353 "src/PrettyTree.hs" #-}
                 )) of
          { _fileOtbls | _fileOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 14358 "src/PrettyTree.hs" #-}
                  )) of
           { _fileOopts | _fileOopts `seq` (True) ->
           (case (file_ _fileOopts _fileOtbls) of
            { ( _fileIoutput,_fileItbls) | True ->
                (case (flags_) of
                 { ( _flagsIdoLazyInit) | True ->
                     (case (({-# LINE 75 "src\\PrettyTree.ag" #-}
                             text "TAG Abc" <+> space <+> props
                               [ ("lazy init", bool _flagsIdoLazyInit)
                               , ("name",      str name_)
                               ]
                             $+$ _fileIoutput
                             {-# LINE 14371 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                              _fileItbls
                              {-# LINE 14376 "src/PrettyTree.hs" #-}
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
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 81 "src\\PrettyTree.ag" #-}
                 text "TAG FileAttributes" <+> space <+> props
                   [ ("use blit",    bool useDirectBlit_)
                   , ("use gpu",     bool useGPU_)
                   , ("hasAS3",      bool hasAS3_)
                   , ("use network", bool useNetwork_)
                   ]
                 {-# LINE 14396 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 14401 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOtbls | _lhsOtbls `seq` (True) ->
           ( _lhsOoutput,_lhsOtbls) }) }))
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (kind_) of
          { ( _kindIself) | True ->
              (case (({-# LINE 88 "src\\PrettyTree.ag" #-}
                      text "TAG" <+> text (show _kindIself) <+> space <+> props
                        [ ("length", num length_)
                        ]
                      {-# LINE 14418 "src/PrettyTree.hs" #-}
                      )) of
               { _lhsOoutput | _lhsOoutput `seq` (True) ->
               (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                       _lhsItbls
                       {-# LINE 14423 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOtbls | _lhsOtbls `seq` (True) ->
                ( _lhsOoutput,_lhsOtbls) }) }) }))
sem_Tag_End :: T_Tag
sem_Tag_End =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 14433 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 14438 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOtbls | _lhsOtbls `seq` (True) ->
           ( _lhsOoutput,_lhsOtbls) }) }))
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
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_End
            {-# LINE 14582 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14587 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_ShowFrame :: T_TagKind
sem_TagKind_ShowFrame =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_ShowFrame
            {-# LINE 14595 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14600 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineShape :: T_TagKind
sem_TagKind_DefineShape =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineShape
            {-# LINE 14608 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14613 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_PlaceObject :: T_TagKind
sem_TagKind_PlaceObject =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_PlaceObject
            {-# LINE 14621 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14626 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_RemoveObject :: T_TagKind
sem_TagKind_RemoveObject =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_RemoveObject
            {-# LINE 14634 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14639 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBits :: T_TagKind
sem_TagKind_DefineBits =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBits
            {-# LINE 14647 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14652 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineButton :: T_TagKind
sem_TagKind_DefineButton =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineButton
            {-# LINE 14660 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14665 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_JPEGTables :: T_TagKind
sem_TagKind_JPEGTables =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_JPEGTables
            {-# LINE 14673 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14678 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SetBackgroundColor :: T_TagKind
sem_TagKind_SetBackgroundColor =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SetBackgroundColor
            {-# LINE 14686 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14691 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFont :: T_TagKind
sem_TagKind_DefineFont =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFont
            {-# LINE 14699 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14704 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineText :: T_TagKind
sem_TagKind_DefineText =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineText
            {-# LINE 14712 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14717 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DoAction :: T_TagKind
sem_TagKind_DoAction =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DoAction
            {-# LINE 14725 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14730 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFontInfo :: T_TagKind
sem_TagKind_DefineFontInfo =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFontInfo
            {-# LINE 14738 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14743 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineSound :: T_TagKind
sem_TagKind_DefineSound =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineSound
            {-# LINE 14751 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14756 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_StartSound :: T_TagKind
sem_TagKind_StartSound =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_StartSound
            {-# LINE 14764 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14769 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineButtonSound :: T_TagKind
sem_TagKind_DefineButtonSound =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineButtonSound
            {-# LINE 14777 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14782 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SoundStreamHead :: T_TagKind
sem_TagKind_SoundStreamHead =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SoundStreamHead
            {-# LINE 14790 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14795 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SoundStreamBlock :: T_TagKind
sem_TagKind_SoundStreamBlock =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SoundStreamBlock
            {-# LINE 14803 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14808 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBitsLossless :: T_TagKind
sem_TagKind_DefineBitsLossless =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBitsLossless
            {-# LINE 14816 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14821 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBitsJPEG2 :: T_TagKind
sem_TagKind_DefineBitsJPEG2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBitsJPEG2
            {-# LINE 14829 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14834 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineShape2 :: T_TagKind
sem_TagKind_DefineShape2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineShape2
            {-# LINE 14842 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14847 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineButtonCxform :: T_TagKind
sem_TagKind_DefineButtonCxform =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineButtonCxform
            {-# LINE 14855 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14860 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_Protect :: T_TagKind
sem_TagKind_Protect =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_Protect
            {-# LINE 14868 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14873 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_PlaceObject2 :: T_TagKind
sem_TagKind_PlaceObject2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_PlaceObject2
            {-# LINE 14881 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14886 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_RemoveObject2 :: T_TagKind
sem_TagKind_RemoveObject2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_RemoveObject2
            {-# LINE 14894 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14899 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineShape3 :: T_TagKind
sem_TagKind_DefineShape3 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineShape3
            {-# LINE 14907 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14912 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineText2 :: T_TagKind
sem_TagKind_DefineText2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineText2
            {-# LINE 14920 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14925 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineButton2 :: T_TagKind
sem_TagKind_DefineButton2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineButton2
            {-# LINE 14933 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14938 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBitsJPEG3 :: T_TagKind
sem_TagKind_DefineBitsJPEG3 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBitsJPEG3
            {-# LINE 14946 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14951 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBitsLossless2 :: T_TagKind
sem_TagKind_DefineBitsLossless2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBitsLossless2
            {-# LINE 14959 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14964 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineEditText :: T_TagKind
sem_TagKind_DefineEditText =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineEditText
            {-# LINE 14972 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14977 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineSprite :: T_TagKind
sem_TagKind_DefineSprite =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineSprite
            {-# LINE 14985 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 14990 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_FrameLabel :: T_TagKind
sem_TagKind_FrameLabel =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_FrameLabel
            {-# LINE 14998 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15003 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SoundStreamHead2 :: T_TagKind
sem_TagKind_SoundStreamHead2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SoundStreamHead2
            {-# LINE 15011 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15016 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineMorphShape :: T_TagKind
sem_TagKind_DefineMorphShape =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineMorphShape
            {-# LINE 15024 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15029 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFont2 :: T_TagKind
sem_TagKind_DefineFont2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFont2
            {-# LINE 15037 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15042 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_ExportAssets :: T_TagKind
sem_TagKind_ExportAssets =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_ExportAssets
            {-# LINE 15050 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15055 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_ImportAssets :: T_TagKind
sem_TagKind_ImportAssets =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_ImportAssets
            {-# LINE 15063 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15068 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_EnableDebugger :: T_TagKind
sem_TagKind_EnableDebugger =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_EnableDebugger
            {-# LINE 15076 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15081 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DoInitAction :: T_TagKind
sem_TagKind_DoInitAction =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DoInitAction
            {-# LINE 15089 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15094 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineVideoStream :: T_TagKind
sem_TagKind_DefineVideoStream =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineVideoStream
            {-# LINE 15102 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15107 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_VideoFrame :: T_TagKind
sem_TagKind_VideoFrame =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_VideoFrame
            {-# LINE 15115 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15120 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFontInfo2 :: T_TagKind
sem_TagKind_DefineFontInfo2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFontInfo2
            {-# LINE 15128 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15133 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_EnableDebugger2 :: T_TagKind
sem_TagKind_EnableDebugger2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_EnableDebugger2
            {-# LINE 15141 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15146 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_ScriptLimits :: T_TagKind
sem_TagKind_ScriptLimits =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_ScriptLimits
            {-# LINE 15154 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15159 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SetTabIndex :: T_TagKind
sem_TagKind_SetTabIndex =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SetTabIndex
            {-# LINE 15167 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15172 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_FileAttributes :: T_TagKind
sem_TagKind_FileAttributes =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_FileAttributes
            {-# LINE 15180 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15185 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_PlaceObject3 :: T_TagKind
sem_TagKind_PlaceObject3 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_PlaceObject3
            {-# LINE 15193 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15198 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_ImportAssets2 :: T_TagKind
sem_TagKind_ImportAssets2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_ImportAssets2
            {-# LINE 15206 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15211 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFontAlignZones :: T_TagKind
sem_TagKind_DefineFontAlignZones =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFontAlignZones
            {-# LINE 15219 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15224 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_CSMTextSettings :: T_TagKind
sem_TagKind_CSMTextSettings =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_CSMTextSettings
            {-# LINE 15232 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15237 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFont3 :: T_TagKind
sem_TagKind_DefineFont3 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFont3
            {-# LINE 15245 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15250 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_SymbolClass :: T_TagKind
sem_TagKind_SymbolClass =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_SymbolClass
            {-# LINE 15258 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15263 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_Metadata :: T_TagKind
sem_TagKind_Metadata =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_Metadata
            {-# LINE 15271 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15276 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineScalingGrid :: T_TagKind
sem_TagKind_DefineScalingGrid =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineScalingGrid
            {-# LINE 15284 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15289 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DoABC :: T_TagKind
sem_TagKind_DoABC =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DoABC
            {-# LINE 15297 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15302 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineShape4 :: T_TagKind
sem_TagKind_DefineShape4 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineShape4
            {-# LINE 15310 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15315 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineMorphShape2 :: T_TagKind
sem_TagKind_DefineMorphShape2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineMorphShape2
            {-# LINE 15323 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15328 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineSceneAndFrameLabelData :: T_TagKind
sem_TagKind_DefineSceneAndFrameLabelData =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineSceneAndFrameLabelData
            {-# LINE 15336 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15341 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBinaryData :: T_TagKind
sem_TagKind_DefineBinaryData =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBinaryData
            {-# LINE 15349 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15354 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFontName :: T_TagKind
sem_TagKind_DefineFontName =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFontName
            {-# LINE 15362 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15367 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_StartSound2 :: T_TagKind
sem_TagKind_StartSound2 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_StartSound2
            {-# LINE 15375 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15380 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineBitsJPEG4 :: T_TagKind
sem_TagKind_DefineBitsJPEG4 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineBitsJPEG4
            {-# LINE 15388 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15393 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_DefineFont4 :: T_TagKind
sem_TagKind_DefineFont4 =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_DefineFont4
            {-# LINE 15401 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15406 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
sem_TagKind_Other :: Word16 ->
                     T_TagKind
sem_TagKind_Other code_ =
    (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
            TagKind_Other code_
            {-# LINE 15415 "src/PrettyTree.hs" #-}
            )) of
     { _self | _self `seq` (True) ->
     (case (({-# LINE 92 "src\\PrettyTree.ag" #-}
             _self
             {-# LINE 15420 "src/PrettyTree.hs" #-}
             )) of
      { _lhsOself | _lhsOself `seq` (True) ->
      ( _lhsOself) }) })
-- Tags --------------------------------------------------------
-- cata
sem_Tags :: Tags ->
            T_Tags
sem_Tags list =
    (Prelude.foldr sem_Tags_Cons sem_Tags_Nil (Prelude.map sem_Tag list))
-- semantic domain
type T_Tags = Options ->
              ([SymbolTables]) ->
              ( Doc,([SymbolTables]))
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 15442 "src/PrettyTree.hs" #-}
                 )) of
          { _hdOtbls | _hdOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 15447 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOopts | _hdOopts `seq` (True) ->
           (case (hd_ _hdOopts _hdOtbls) of
            { ( _hdIoutput,_hdItbls) | True ->
                (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                        _hdItbls
                        {-# LINE 15454 "src/PrettyTree.hs" #-}
                        )) of
                 { _tlOtbls | _tlOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 15459 "src/PrettyTree.hs" #-}
                         )) of
                  { _tlOopts | _tlOopts `seq` (True) ->
                  (case (tl_ _tlOopts _tlOtbls) of
                   { ( _tlIoutput,_tlItbls) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 15466 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                                _tlItbls
                                {-# LINE 15471 "src/PrettyTree.hs" #-}
                                )) of
                         { _lhsOtbls | _lhsOtbls `seq` (True) ->
                         ( _lhsOoutput,_lhsOtbls) }) }) }) }) }) }) }) }))
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 15481 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          (case (({-# LINE 50 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 15486 "src/PrettyTree.hs" #-}
                  )) of
           { _lhsOtbls | _lhsOtbls `seq` (True) ->
           ( _lhsOoutput,_lhsOtbls) }) }))
-- Trait -------------------------------------------------------
-- cata
sem_Trait :: Trait ->
             T_Trait
sem_Trait (Trait_Trait _name _data _attrs _meta) =
    (sem_Trait_Trait _name (sem_TraitData _data) (sem_TraitAttrs _attrs) (sem_TraitMeta _meta))
-- semantic domain
type T_Trait = Options ->
               SymbolTables ->
               ( Doc)
sem_Trait_Trait :: Word32 ->
                   T_TraitData ->
                   T_TraitAttrs ->
                   T_TraitMeta ->
                   T_Trait
sem_Trait_Trait name_ data_ attrs_ meta_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 15510 "src/PrettyTree.hs" #-}
                 )) of
          { _dataOtbls | _dataOtbls `seq` (True) ->
          (case (({-# LINE 325 "src\\PrettyTree.ag" #-}
                  nameView _lhsItbls (Ref name_)
                  {-# LINE 15515 "src/PrettyTree.hs" #-}
                  )) of
           { nm_val_ | nm_val_ `seq` (True) ->
           (case ((sem_NmV nm_val_)) of
            { nm_inst_ | nm_inst_ `seq` (True) ->
            (case (({-# LINE 326 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 15522 "src/PrettyTree.hs" #-}
                    )) of
             { _nmOinfo | _nmOinfo `seq` (True) ->
             (case (nm_inst_ _nmOinfo) of
              { ( _nmIisEmpty,_nmIoutput) | True ->
                  (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                          _lhsItbls
                          {-# LINE 15529 "src/PrettyTree.hs" #-}
                          )) of
                   { _attrsOtbls | _attrsOtbls `seq` (True) ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 15534 "src/PrettyTree.hs" #-}
                           )) of
                    { _attrsOopts | _attrsOopts `seq` (True) ->
                    (case (attrs_ _attrsOopts _attrsOtbls) of
                     { ( _attrsIoutput) | True ->
                         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                                 _lhsIopts
                                 {-# LINE 15541 "src/PrettyTree.hs" #-}
                                 )) of
                          { _dataOopts | _dataOopts `seq` (True) ->
                          (case (data_ _dataOopts _dataOtbls) of
                           { ( _dataIoutput) | True ->
                               (case (({-# LINE 328 "src\\PrettyTree.ag" #-}
                                       text "TRAIT" <+> _nmIoutput <+> parens (text $ show name_) <+>
                                           space <+> _attrsIoutput
                                       $+$ nest 2 _dataIoutput
                                       {-# LINE 15550 "src/PrettyTree.hs" #-}
                                       )) of
                                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                ( _lhsOoutput) }) }) }) }) }) }) }) }) }) }) }))
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
type T_TraitAttr = Options ->
                   SymbolTables ->
                   ( Doc)
sem_TraitAttr_Final :: T_TraitAttr
sem_TraitAttr_Final =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 334 "src\\PrettyTree.ag" #-}
                 text "final"
                 {-# LINE 15574 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 335 "src\\PrettyTree.ag" #-}
                 text "override"
                 {-# LINE 15584 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 336 "src\\PrettyTree.ag" #-}
                 text "metadata"
                 {-# LINE 15594 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- TraitAttrs --------------------------------------------------
-- cata
sem_TraitAttrs :: TraitAttrs ->
                  T_TraitAttrs
sem_TraitAttrs list =
    (Prelude.foldr sem_TraitAttrs_Cons sem_TraitAttrs_Nil (Prelude.map sem_TraitAttr list))
-- semantic domain
type T_TraitAttrs = Options ->
                    SymbolTables ->
                    ( Doc)
sem_TraitAttrs_Cons :: T_TraitAttr ->
                       T_TraitAttrs ->
                       T_TraitAttrs
sem_TraitAttrs_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 15616 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 15621 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                        _lhsItbls
                        {-# LINE 15628 "src/PrettyTree.hs" #-}
                        )) of
                 { _hdOtbls | _hdOtbls `seq` (True) ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 15633 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 332 "src\\PrettyTree.ag" #-}
                               _hdIoutput <+> _tlIoutput
                               {-# LINE 15640 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 15650 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
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
type T_TraitData = Options ->
                   SymbolTables ->
                   ( Doc)
sem_TraitData_Slot :: Word32 ->
                      Word32 ->
                      Word32 ->
                      T_ValueKind ->
                      T_TraitData
sem_TraitData_Slot slotId_ tp_ vindex_ vkind_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 348 "src\\PrettyTree.ag" #-}
                 nameView _lhsItbls (Ref tp_)
                 {-# LINE 15686 "src/PrettyTree.hs" #-}
                 )) of
          { nm_val_ | nm_val_ `seq` (True) ->
          (case ((sem_NmV nm_val_)) of
           { nm_inst_ | nm_inst_ `seq` (True) ->
           (case (({-# LINE 349 "src\\PrettyTree.ag" #-}
                   Short
                   {-# LINE 15693 "src/PrettyTree.hs" #-}
                   )) of
            { _nmOinfo | _nmOinfo `seq` (True) ->
            (case (({-# LINE 339 "src\\PrettyTree.ag" #-}
                    text "SLOT"
                    {-# LINE 15698 "src/PrettyTree.hs" #-}
                    )) of
             { _desc | _desc `seq` (True) ->
             (case (nm_inst_ _nmOinfo) of
              { ( _nmIisEmpty,_nmIoutput) | True ->
                  (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                          _lhsItbls
                          {-# LINE 15705 "src/PrettyTree.hs" #-}
                          )) of
                   { _vkindOtbls | _vkindOtbls `seq` (True) ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 15710 "src/PrettyTree.hs" #-}
                           )) of
                    { _vkindOopts | _vkindOopts `seq` (True) ->
                    (case (vkind_ _vkindOopts _vkindOtbls) of
                     { ( _vkindIoutput) | True ->
                         (case (({-# LINE 351 "src\\PrettyTree.ag" #-}
                                 _desc     <+> props
                                   [ ("slotid", num slotId_)
                                   , ("type",   _nmIoutput)
                                   , ("vindex", num vindex_)
                                   , ("vkind",  _vkindIoutput)
                                   ]
                                 {-# LINE 15722 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_TraitData_Const :: Word32 ->
                       Word32 ->
                       Word32 ->
                       T_ValueKind ->
                       T_TraitData
sem_TraitData_Const slotId_ tp_ vindex_ vkind_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 348 "src\\PrettyTree.ag" #-}
                 nameView _lhsItbls (Ref tp_)
                 {-# LINE 15736 "src/PrettyTree.hs" #-}
                 )) of
          { nm_val_ | nm_val_ `seq` (True) ->
          (case ((sem_NmV nm_val_)) of
           { nm_inst_ | nm_inst_ `seq` (True) ->
           (case (({-# LINE 349 "src\\PrettyTree.ag" #-}
                   Short
                   {-# LINE 15743 "src/PrettyTree.hs" #-}
                   )) of
            { _nmOinfo | _nmOinfo `seq` (True) ->
            (case (({-# LINE 340 "src\\PrettyTree.ag" #-}
                    text "CONST"
                    {-# LINE 15748 "src/PrettyTree.hs" #-}
                    )) of
             { _desc | _desc `seq` (True) ->
             (case (nm_inst_ _nmOinfo) of
              { ( _nmIisEmpty,_nmIoutput) | True ->
                  (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                          _lhsItbls
                          {-# LINE 15755 "src/PrettyTree.hs" #-}
                          )) of
                   { _vkindOtbls | _vkindOtbls `seq` (True) ->
                   (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                           _lhsIopts
                           {-# LINE 15760 "src/PrettyTree.hs" #-}
                           )) of
                    { _vkindOopts | _vkindOopts `seq` (True) ->
                    (case (vkind_ _vkindOopts _vkindOtbls) of
                     { ( _vkindIoutput) | True ->
                         (case (({-# LINE 351 "src\\PrettyTree.ag" #-}
                                 _desc     <+> props
                                   [ ("slotid", num slotId_)
                                   , ("type",   _nmIoutput)
                                   , ("vindex", num vindex_)
                                   , ("vkind",  _vkindIoutput)
                                   ]
                                 {-# LINE 15772 "src/PrettyTree.hs" #-}
                                 )) of
                          { _lhsOoutput | _lhsOoutput `seq` (True) ->
                          ( _lhsOoutput) }) }) }) }) }) }) }) }) }))
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 359 "src\\PrettyTree.ag" #-}
                 methodView _lhsItbls (Ref method_)
                 {-# LINE 15784 "src/PrettyTree.hs" #-}
                 )) of
          { m_val_ | m_val_ `seq` (True) ->
          (case ((sem_MethodV m_val_)) of
           { m_inst_ | m_inst_ `seq` (True) ->
           (case (({-# LINE 341 "src\\PrettyTree.ag" #-}
                   text "METHOD"
                   {-# LINE 15791 "src/PrettyTree.hs" #-}
                   )) of
            { _desc | _desc `seq` (True) ->
            (case (({-# LINE 360 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 15796 "src/PrettyTree.hs" #-}
                    )) of
             { _mOinfo | _mOinfo `seq` (True) ->
             (case (m_inst_ _mOinfo) of
              { ( _mIoutput) | True ->
                  (case (({-# LINE 362 "src\\PrettyTree.ag" #-}
                          _desc     <+> props
                            [ ("disp-id", num dispId_)
                            , ("method",  _mIoutput)
                            ]
                          {-# LINE 15806 "src/PrettyTree.hs" #-}
                          )) of
                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                   ( _lhsOoutput) }) }) }) }) }) }))
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 359 "src\\PrettyTree.ag" #-}
                 methodView _lhsItbls (Ref method_)
                 {-# LINE 15818 "src/PrettyTree.hs" #-}
                 )) of
          { m_val_ | m_val_ `seq` (True) ->
          (case ((sem_MethodV m_val_)) of
           { m_inst_ | m_inst_ `seq` (True) ->
           (case (({-# LINE 342 "src\\PrettyTree.ag" #-}
                   text "GETTER"
                   {-# LINE 15825 "src/PrettyTree.hs" #-}
                   )) of
            { _desc | _desc `seq` (True) ->
            (case (({-# LINE 360 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 15830 "src/PrettyTree.hs" #-}
                    )) of
             { _mOinfo | _mOinfo `seq` (True) ->
             (case (m_inst_ _mOinfo) of
              { ( _mIoutput) | True ->
                  (case (({-# LINE 362 "src\\PrettyTree.ag" #-}
                          _desc     <+> props
                            [ ("disp-id", num dispId_)
                            , ("method",  _mIoutput)
                            ]
                          {-# LINE 15840 "src/PrettyTree.hs" #-}
                          )) of
                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                   ( _lhsOoutput) }) }) }) }) }) }))
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 359 "src\\PrettyTree.ag" #-}
                 methodView _lhsItbls (Ref method_)
                 {-# LINE 15852 "src/PrettyTree.hs" #-}
                 )) of
          { m_val_ | m_val_ `seq` (True) ->
          (case ((sem_MethodV m_val_)) of
           { m_inst_ | m_inst_ `seq` (True) ->
           (case (({-# LINE 343 "src\\PrettyTree.ag" #-}
                   text "SETTER"
                   {-# LINE 15859 "src/PrettyTree.hs" #-}
                   )) of
            { _desc | _desc `seq` (True) ->
            (case (({-# LINE 360 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 15864 "src/PrettyTree.hs" #-}
                    )) of
             { _mOinfo | _mOinfo `seq` (True) ->
             (case (m_inst_ _mOinfo) of
              { ( _mIoutput) | True ->
                  (case (({-# LINE 362 "src\\PrettyTree.ag" #-}
                          _desc     <+> props
                            [ ("disp-id", num dispId_)
                            , ("method",  _mIoutput)
                            ]
                          {-# LINE 15874 "src/PrettyTree.hs" #-}
                          )) of
                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                   ( _lhsOoutput) }) }) }) }) }) }))
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 359 "src\\PrettyTree.ag" #-}
                 methodView _lhsItbls (Ref method_)
                 {-# LINE 15886 "src/PrettyTree.hs" #-}
                 )) of
          { m_val_ | m_val_ `seq` (True) ->
          (case ((sem_MethodV m_val_)) of
           { m_inst_ | m_inst_ `seq` (True) ->
           (case (({-# LINE 344 "src\\PrettyTree.ag" #-}
                   text "FUN"
                   {-# LINE 15893 "src/PrettyTree.hs" #-}
                   )) of
            { _desc | _desc `seq` (True) ->
            (case (({-# LINE 360 "src\\PrettyTree.ag" #-}
                    Short
                    {-# LINE 15898 "src/PrettyTree.hs" #-}
                    )) of
             { _mOinfo | _mOinfo `seq` (True) ->
             (case (m_inst_ _mOinfo) of
              { ( _mIoutput) | True ->
                  (case (({-# LINE 362 "src\\PrettyTree.ag" #-}
                          _desc     <+> props
                            [ ("disp-id", num dispId_)
                            , ("method",  _mIoutput)
                            ]
                          {-# LINE 15908 "src/PrettyTree.hs" #-}
                          )) of
                   { _lhsOoutput | _lhsOoutput `seq` (True) ->
                   ( _lhsOoutput) }) }) }) }) }) }))
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 367 "src\\PrettyTree.ag" #-}
                 Ref class_
                 {-# LINE 15920 "src/PrettyTree.hs" #-}
                 )) of
          { _key | _key `seq` (True) ->
          (case (({-# LINE 368 "src\\PrettyTree.ag" #-}
                  lookupClass _key     _lhsItbls
                  {-# LINE 15925 "src/PrettyTree.hs" #-}
                  )) of
           { _descr | _descr `seq` (True) ->
           (case (({-# LINE 371 "src\\PrettyTree.ag" #-}
                   nameView _lhsItbls (clName _descr    )
                   {-# LINE 15930 "src/PrettyTree.hs" #-}
                   )) of
            { nm_val_ | nm_val_ `seq` (True) ->
            (case ((sem_NmV nm_val_)) of
             { nm_inst_ | nm_inst_ `seq` (True) ->
             (case (({-# LINE 372 "src\\PrettyTree.ag" #-}
                     Short
                     {-# LINE 15937 "src/PrettyTree.hs" #-}
                     )) of
              { _nmOinfo | _nmOinfo `seq` (True) ->
              (case (nm_inst_ _nmOinfo) of
               { ( _nmIisEmpty,_nmIoutput) | True ->
                   (case (({-# LINE 374 "src\\PrettyTree.ag" #-}
                           text "CLASS" <+> _nmIoutput
                           {-# LINE 15944 "src/PrettyTree.hs" #-}
                           )) of
                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                    ( _lhsOoutput) }) }) }) }) }) }) }))
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
type T_TraitKind = Options ->
                   SymbolTables ->
                   ( Doc)
sem_TraitKind_Slot :: T_TraitKind
sem_TraitKind_Slot =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 15976 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 15986 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 15996 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16006 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16016 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16026 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16036 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- TraitMeta ---------------------------------------------------
-- cata
sem_TraitMeta :: TraitMeta ->
                 T_TraitMeta
sem_TraitMeta list =
    (Prelude.foldr sem_TraitMeta_Cons sem_TraitMeta_Nil list)
-- semantic domain
type T_TraitMeta = Options ->
                   SymbolTables ->
                   ( Doc)
sem_TraitMeta_Cons :: Word32 ->
                      T_TraitMeta ->
                      T_TraitMeta
sem_TraitMeta_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 16058 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                  _lhsIopts
                  {-# LINE 16063 "src/PrettyTree.hs" #-}
                  )) of
           { _tlOopts | _tlOopts `seq` (True) ->
           (case (tl_ _tlOopts _tlOtbls) of
            { ( _tlIoutput) | True ->
                (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                        _tlIoutput
                        {-# LINE 16070 "src/PrettyTree.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOoutput) }) }) }) }))
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16080 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- TraitV ------------------------------------------------------
-- cata
sem_TraitV :: TraitV ->
              T_TraitV
sem_TraitV (TraitV_Method _nm _sig) =
    (sem_TraitV_Method (sem_NmV _nm) (sem_SigV _sig))
sem_TraitV (TraitV_Field _nm _tp) =
    (sem_TraitV_Field (sem_NmV _nm) (sem_TypeV _tp))
sem_TraitV (TraitV_Other _nm) =
    (sem_TraitV_Other (sem_NmV _nm))
-- semantic domain
type T_TraitV = PPInfo ->
                ( Doc)
sem_TraitV_Method :: T_NmV ->
                     T_SigV ->
                     T_TraitV
sem_TraitV_Method nm_ sig_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 16104 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 16109 "src/PrettyTree.hs" #-}
                  )) of
           { _sigOinfo | _sigOinfo `seq` (True) ->
           (case (sig_ _sigOinfo) of
            { ( _sigIoutput) | True ->
                (case (nm_ _nmOinfo) of
                 { ( _nmIisEmpty,_nmIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _nmIoutput $+$ _sigIoutput
                             {-# LINE 16118 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_TraitV_Field :: T_NmV ->
                    T_TypeV ->
                    T_TraitV
sem_TraitV_Field nm_ tp_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 16129 "src/PrettyTree.hs" #-}
                 )) of
          { _tpOinfo | _tpOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 16134 "src/PrettyTree.hs" #-}
                  )) of
           { _nmOinfo | _nmOinfo `seq` (True) ->
           (case (tp_ _tpOinfo) of
            { ( _tpIoutput) | True ->
                (case (nm_ _nmOinfo) of
                 { ( _nmIisEmpty,_nmIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _nmIoutput $+$ _tpIoutput
                             {-# LINE 16143 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_TraitV_Other :: T_NmV ->
                    T_TraitV
sem_TraitV_Other nm_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 16153 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (nm_ _nmOinfo) of
           { ( _nmIisEmpty,_nmIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _nmIoutput
                       {-# LINE 16160 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
-- Traits ------------------------------------------------------
-- cata
sem_Traits :: Traits ->
              T_Traits
sem_Traits list =
    (Prelude.foldr sem_Traits_Cons sem_Traits_Nil (Prelude.map sem_Trait list))
-- semantic domain
type T_Traits = Options ->
                SymbolTables ->
                ( Doc)
sem_Traits_Cons :: T_Trait ->
                   T_Traits ->
                   T_Traits
sem_Traits_Cons hd_ tl_ =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                 _lhsItbls
                 {-# LINE 16182 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOtbls | _tlOtbls `seq` (True) ->
          (case (({-# LINE 51 "src\\PrettyTree.ag" #-}
                  _lhsItbls
                  {-# LINE 16187 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOtbls | _hdOtbls `seq` (True) ->
           (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                   _lhsIopts
                   {-# LINE 16192 "src/PrettyTree.hs" #-}
                   )) of
            { _tlOopts | _tlOopts `seq` (True) ->
            (case (tl_ _tlOopts _tlOtbls) of
             { ( _tlIoutput) | True ->
                 (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                         _lhsIopts
                         {-# LINE 16199 "src/PrettyTree.hs" #-}
                         )) of
                  { _hdOopts | _hdOopts `seq` (True) ->
                  (case (hd_ _hdOopts _hdOtbls) of
                   { ( _hdIoutput) | True ->
                       (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                               _hdIoutput $+$ _tlIoutput
                               {-# LINE 16206 "src/PrettyTree.hs" #-}
                               )) of
                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                        ( _lhsOoutput) }) }) }) }) }) }) }))
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16216 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- TraitsV -----------------------------------------------------
-- cata
sem_TraitsV :: TraitsV ->
               T_TraitsV
sem_TraitsV list =
    (Prelude.foldr sem_TraitsV_Cons sem_TraitsV_Nil (Prelude.map sem_TraitV list))
-- semantic domain
type T_TraitsV = PPInfo ->
                 ( Doc)
sem_TraitsV_Cons :: T_TraitV ->
                    T_TraitsV ->
                    T_TraitsV
sem_TraitsV_Cons hd_ tl_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 16236 "src/PrettyTree.hs" #-}
                 )) of
          { _tlOinfo | _tlOinfo `seq` (True) ->
          (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                  _lhsIinfo
                  {-# LINE 16241 "src/PrettyTree.hs" #-}
                  )) of
           { _hdOinfo | _hdOinfo `seq` (True) ->
           (case (tl_ _tlOinfo) of
            { ( _tlIoutput) | True ->
                (case (hd_ _hdOinfo) of
                 { ( _hdIoutput) | True ->
                     (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                             _hdIoutput $+$ _tlIoutput
                             {-# LINE 16250 "src/PrettyTree.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }))
sem_TraitsV_Nil :: T_TraitsV
sem_TraitsV_Nil =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16259 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
-- TypeV -------------------------------------------------------
-- cata
sem_TypeV :: TypeV ->
             T_TypeV
sem_TypeV (TypeV_Type _isNull _nm) =
    (sem_TypeV_Type _isNull (sem_NmV _nm))
-- semantic domain
type T_TypeV = PPInfo ->
               ( Doc)
sem_TypeV_Type :: Bool ->
                  T_NmV ->
                  T_TypeV
sem_TypeV_Type isNull_ nm_ =
    (\ _lhsIinfo ->
         (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                 _lhsIinfo
                 {-# LINE 16279 "src/PrettyTree.hs" #-}
                 )) of
          { _nmOinfo | _nmOinfo `seq` (True) ->
          (case (nm_ _nmOinfo) of
           { ( _nmIisEmpty,_nmIoutput) | True ->
               (case (({-# LINE 197 "src\\PrettyTree.ag" #-}
                       _nmIoutput
                       {-# LINE 16286 "src/PrettyTree.hs" #-}
                       )) of
                { _lhsOoutput | _lhsOoutput `seq` (True) ->
                ( _lhsOoutput) }) }) }))
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
type T_ValueKind = Options ->
                   SymbolTables ->
                   ( Doc)
sem_ValueKind_Int :: T_ValueKind
sem_ValueKind_Int =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16334 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16344 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16354 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16364 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16374 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16384 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16394 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16404 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16414 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16424 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16434 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16444 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16454 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16464 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    (\ _lhsIopts
       _lhsItbls ->
         (case (({-# LINE 40 "src\\PrettyTree.ag" #-}
                 empty
                 {-# LINE 16474 "src/PrettyTree.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }))