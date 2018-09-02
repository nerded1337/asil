

{-# LANGUAGE TypeSynonymInstances #-}
-- UUAGC 0.9.52.1 (src/PrettyCode.ag)
module PrettyCode(ppSwf,ppAbc) where

{-# LINE 12 "src\\PrettyCode.ag" #-}

import Codec.Compression.Zlib
import Data.Binary.Builder
import Data.Binary.IEEE754
import qualified Data.Binary.Put as P
import Data.Bits
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Monoid
import ByteCode
{-# LINE 20 "src/PrettyCode.hs" #-}
{-# LINE 28 "src\\PrettyCode.ag" #-}

ppSwf :: SwfFile -> ByteString
ppSwf m = str where
  inh = Inh_SwfFile {}
  sem = sem_SwfFile m
  syn = wrap_SwfFile sem inh
  out = output_Syn_SwfFile syn
  str = toLazyByteString out

ppAbc :: AbcFile -> ByteString
ppAbc m = str where
  inh = Inh_AbcFile {}
  sem = sem_AbcFile m
  syn = wrap_AbcFile sem inh
  out = output_Syn_AbcFile syn
  str = toLazyByteString out
{-# LINE 38 "src/PrettyCode.hs" #-}

{-# LINE 46 "src\\PrettyCode.ag" #-}

class ToBuilder a where
  toBuilder :: a -> Builder

instance ToBuilder ByteString where
  toBuilder = fromLazyByteString

instance ToBuilder Builder where
  toBuilder = id

instance ToBuilder Word8 where
  toBuilder = singleton

infixr 3 ##

(##) :: (ToBuilder a, ToBuilder b) => a -> b -> Builder
a ## b = toBuilder a `append` toBuilder b

u8 :: Word8 -> Builder
u8 = singleton . fromIntegral

w32 :: Word32 -> Builder
w32 = putWord32le

d64 :: Double -> Builder
d64 = toBuilder . P.runPut . putFloat64le

u16 :: Word16 -> Builder
u16 = putWord16le

s24 :: Word32 -> Builder
s24 w = u8 w1 ## u8 w2 ## u8 w3
  where w1 = fromIntegral (w .&. 0xFF)
        w2 = fromIntegral (shiftR w 8 .&. 0xFF)
        w3 = fromIntegral (shiftR w 16 .&. 0xFF)

-- Note: ignoring here the exact size.
u30 :: Word32 -> Builder
u30 = v32 False

s32 :: Word32 -> Builder
s32 = v32 True

u32 :: Word32 -> Builder
u32 = v32 False

-- Rather complicated due to compression of negative numbers.
v32 :: Bool -> Word32 -> Builder
v32 isSigned w
  | isSigned && w `testBit` 31 = negative
  | otherwise                  = positive
  where
    part w' = let r  = shiftR w' 7  -- drop seven bits, the residual bits form the remainder
                  v  = w' .&. 0x7F  -- take the first seven bits
                  ps | r > 0     = part r
                     | otherwise = []
              in fromIntegral v : ps
    ws = part w  -- non-empty sequence of 7 bits
    
    mark [x]    = [x]                         -- last byte unchanged
    mark (x:xs) = setBit x 7 : mark xs        -- mark 7th bit of all but the last byte

    positive = build (mark ws)  -- render to a sequence of bytes

    negative = let -- given a reversed byte sequence, strips off those bytes with the 7 bits set
                   strip [x] = [x]   -- least significant byte unchanged
                   strip (x:y:xs)
                     | x == 0x7F && y `testBit` 6 = strip (y:xs)  -- only if the next byte has the sign-bit set
                     | otherwise                  = x : y : xs

                   wr = reverse ws
                   wr' = (extend $ head wr) : tail wr  -- sign extend the first byte
                   
                   extend = let extend' n x | not (x `testBit` n) = bit n .|. extend' (n-1) x
                                            | otherwise           = x
                            in extend' 6  -- a n == 0 situation cannot happen: number is negative, there must be a sign bit
    
                   ws' = mark . reverse . strip $ wr'
               in build ws'
    
    build = foldr (\x r -> u8 x ## r) empty

u30size :: Int -> Builder
u30size = u30 . fromIntegral

nullString :: ByteString -> Builder
nullString s = s ## u8 0
{-# LINE 128 "src/PrettyCode.hs" #-}

{-# LINE 135 "src\\PrettyCode.ag" #-}

newtype BitBuilder = BitBuilder ([Bool] -> [Bool])

instance Semigroup BitBuilder where
--   mempty = BitBuilder id
  (BitBuilder f) <> (BitBuilder g) = BitBuilder (f . g)

infixr 3 ###

(###) :: BitBuilder -> BitBuilder -> BitBuilder
(###) = (<>)

-- | To put a number in, put the most significant bits first.
putBits :: [Bool] -> BitBuilder
putBits xs = BitBuilder (xs ++)

putBit :: Bool -> BitBuilder
putBit = putBits . return

putWord :: Word32 -> Int -> BitBuilder
putWord w n
  | n <= 0    = error "putWord: n <= 0"
  | otherwise = putBits [ testBit w (n-i) | i <- [1 .. n] ]

padded :: BitBuilder -> Builder
padded (BitBuilder f) = builder where
  bits    = f []
  len     = length bits
  pad     = (8 - (len `rem` 8)) `mod` 8
  bits'   = bits ++ replicate pad False
  bytes   = map b2w8 $ split bits'
  builder = foldr (\b r -> u8 b ## r) mempty bytes
  
  split [] = []
  split xs = let (ys,zs) = splitAt 8 xs in ys : split zs
  
  b2w8 = foldl (\r b -> shiftL r 1 .|. (if b then 1 else 0)) 0
{-# LINE 168 "src/PrettyCode.hs" #-}

{-# LINE 235 "src\\PrettyCode.ag" #-}

fromTagKind :: TagKind -> Word16
fromTagKind k = case k of
  TagKind_End                              -> 0
  TagKind_ShowFrame                        -> 1
  TagKind_DefineShape                      -> 2
  TagKind_PlaceObject                      -> 4
  TagKind_RemoveObject                     -> 5
  TagKind_DefineBits                       -> 6
  TagKind_DefineButton                     -> 7
  TagKind_JPEGTables                       -> 8
  TagKind_SetBackgroundColor               -> 9
  TagKind_DefineFont                       -> 10
  TagKind_DefineText                       -> 11
  TagKind_DoAction                         -> 12
  TagKind_DefineFontInfo                   -> 13
  TagKind_DefineSound                      -> 14
  TagKind_StartSound                       -> 15
  TagKind_DefineButtonSound                -> 17
  TagKind_SoundStreamHead                  -> 18
  TagKind_SoundStreamBlock                 -> 19
  TagKind_DefineBitsLossless               -> 20
  TagKind_DefineBitsJPEG2                  -> 21
  TagKind_DefineShape2                     -> 22
  TagKind_DefineButtonCxform               -> 23
  TagKind_Protect                          -> 24
  TagKind_PlaceObject2                     -> 26
  TagKind_RemoveObject2                    -> 28
  TagKind_DefineShape3                     -> 32
  TagKind_DefineText2                      -> 33
  TagKind_DefineButton2                    -> 34
  TagKind_DefineBitsJPEG3                  -> 35
  TagKind_DefineBitsLossless2              -> 36
  TagKind_DefineEditText                   -> 37
  TagKind_DefineSprite                     -> 39
  TagKind_FrameLabel                       -> 43
  TagKind_SoundStreamHead2                 -> 45
  TagKind_DefineMorphShape                 -> 46
  TagKind_DefineFont2                      -> 48
  TagKind_ExportAssets                     -> 56
  TagKind_ImportAssets                     -> 57
  TagKind_EnableDebugger                   -> 58
  TagKind_DoInitAction                     -> 59
  TagKind_DefineVideoStream                -> 60
  TagKind_VideoFrame                       -> 61
  TagKind_DefineFontInfo2                  -> 62
  TagKind_EnableDebugger2                  -> 64
  TagKind_ScriptLimits                     -> 65
  TagKind_SetTabIndex                      -> 66
  TagKind_FileAttributes                   -> 69
  TagKind_PlaceObject3                     -> 70
  TagKind_ImportAssets2                    -> 71
  TagKind_DefineFontAlignZones             -> 73
  TagKind_CSMTextSettings                  -> 74
  TagKind_DefineFont3                      -> 75
  TagKind_SymbolClass                      -> 76
  TagKind_Metadata                         -> 77
  TagKind_DefineScalingGrid                -> 78
  TagKind_DoABC                            -> 82
  TagKind_DefineShape4                     -> 83
  TagKind_DefineMorphShape2                -> 84
  TagKind_DefineSceneAndFrameLabelData     -> 86
  TagKind_DefineBinaryData                 -> 87
  TagKind_DefineFontName                   -> 88
  TagKind_StartSound2                      -> 89
  TagKind_DefineBitsJPEG4                  -> 90
  TagKind_DefineFont4                      -> 91
  TagKind_Other c                          -> c
{-# LINE 239 "src/PrettyCode.hs" #-}

{-# LINE 352 "src\\PrettyCode.ag" #-}

u30size1 n = u30size (if n == 0 then 0 else n+1)
{-# LINE 244 "src/PrettyCode.hs" #-}

{-# LINE 365 "src\\PrettyCode.ag" #-}

fromNamespaceKind k = case k of
  NamespaceKind_General   -> 0x08
  NamespaceKind_Package   -> 0x16
  NamespaceKind_Internal  -> 0x17
  NamespaceKind_Protected -> 0x18
  NamespaceKind_Explicit  -> 0x19
  NamespaceKind_Static    -> 0x1A
  NamespaceKind_Private   -> 0x05
{-# LINE 256 "src/PrettyCode.hs" #-}

{-# LINE 404 "src\\PrettyCode.ag" #-}

mergeMethodFlags :: MethodFlags -> Word8
mergeMethodFlags = foldr (.|.) 0 . map fromFlag
  where fromFlag fl = case fl of
          MethodFlag_NeedArgs      -> 0x01
          MethodFlag_NeedAct       -> 0x02
          MethodFlag_NeedRest      -> 0x04
          MethodFlag_HasOptionals  -> 0x08
          MethodFlag_SetDXNS       -> 0x40
          MethodFlag_HasParamNames -> 0x80
{-# LINE 269 "src/PrettyCode.hs" #-}

{-# LINE 420 "src\\PrettyCode.ag" #-}

fromValueKind :: ValueKind -> Word8
fromValueKind k = case k of
  ValueKind_Int       -> 0x03
  ValueKind_UInt      -> 0x04
  ValueKind_Double    -> 0x06
  ValueKind_Utf8      -> 0x01
  ValueKind_True      -> 0x0B
  ValueKind_False     -> 0x0A
  ValueKind_Null      -> 0x0C
  ValueKind_Undefined -> 0x00
  ValueKind_Namespace -> 0x08
  ValueKind_Package   -> 0x16
  ValueKind_Internal  -> 0x17
  ValueKind_Protected -> 0x18
  ValueKind_Explicit  -> 0x19
  ValueKind_Static    -> 0x1A
  ValueKind_Private   -> 0x05
{-# LINE 290 "src/PrettyCode.hs" #-}

{-# LINE 456 "src\\PrettyCode.ag" #-}

mergeInstanceFlags :: InstanceFlags -> Word8
mergeInstanceFlags = foldr (.|.) 0 . map fromFlag
  where fromFlag fl = case fl of
          InstanceFlag_ClassSealed     -> 0x01
          InstanceFlag_ClassFinal      -> 0x02
          InstanceFlag_ClassInterface  -> 0x04
          InstanceFlag_ClassProtected  -> 0x08
{-# LINE 301 "src/PrettyCode.hs" #-}

{-# LINE 481 "src\\PrettyCode.ag" #-}

mergeTraitFlags :: TraitAttrs -> Word8
mergeTraitFlags = foldr (.|.) 0 . map fromFlag
  where fromFlag fl = case fl of
          TraitAttr_Final     -> 0x01
          TraitAttr_Override  -> 0x02
          TraitAttr_Metadata  -> 0x04
{-# LINE 311 "src/PrettyCode.hs" #-}
-- AbcFile -----------------------------------------------------
-- cata
sem_AbcFile :: AbcFile ->
               T_AbcFile
sem_AbcFile (AbcFile_File _minorVersion _majorVersion _constantPool _methods _metadatas _instances _classes _scripts _bodies) =
    (sem_AbcFile_File _minorVersion _majorVersion (sem_PoolInfo _constantPool) (sem_MethodInfos _methods) (sem_MetaInfos _metadatas) (sem_InstanceInfos _instances) (sem_ClassInfos _classes) (sem_ScriptInfos _scripts) (sem_BodyInfos _bodies))
-- semantic domain
type T_AbcFile = ( Builder)
data Inh_AbcFile = Inh_AbcFile {}
data Syn_AbcFile = Syn_AbcFile {output_Syn_AbcFile :: !(Builder)}
wrap_AbcFile :: T_AbcFile ->
                Inh_AbcFile ->
                Syn_AbcFile
wrap_AbcFile sem (Inh_AbcFile) =
    (let ( _lhsOoutput) | True = sem
     in  (Syn_AbcFile _lhsOoutput))
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
     { ( _bodiesIcount,_bodiesIoutput) | True ->
         (case (scripts_) of
          { ( _scriptsIcount,_scriptsIoutput) | True ->
              (case (classes_) of
               { ( _classesIcount,_classesIoutput) | True ->
                   (case (instances_) of
                    { ( _instancesIcount,_instancesIoutput) | True ->
                        (case (metadatas_) of
                         { ( _metadatasIcount,_metadatasIoutput) | True ->
                             (case (methods_) of
                              { ( _methodsIcount,_methodsIoutput) | True ->
                                  (case (constantPool_) of
                                   { ( _constantPoolIoutput) | True ->
                                       (case (({-# LINE 336 "src\\PrettyCode.ag" #-}
                                               u16 minorVersion_ ## u16 majorVersion_ ## _constantPoolIoutput ##
                                               u30size _methodsIcount   ## _methodsIoutput   ##
                                               u30size _metadatasIcount ## _metadatasIoutput ##
                                               u30size _instancesIcount ## _instancesIoutput ## _classesIoutput ##
                                               u30size _scriptsIcount   ## _scriptsIoutput   ##
                                               u30size _bodiesIcount    ## _bodiesIoutput
                                               {-# LINE 360 "src/PrettyCode.hs" #-}
                                               )) of
                                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                        ( _lhsOoutput) }) }) }) }) }) }) }) })
-- AbcFlag -----------------------------------------------------
-- cata
sem_AbcFlag :: AbcFlag ->
               T_AbcFlag
sem_AbcFlag (AbcFlag_LazyInit) =
    (sem_AbcFlag_LazyInit)
-- semantic domain
type T_AbcFlag = ( Word32)
sem_AbcFlag_LazyInit :: T_AbcFlag
sem_AbcFlag_LazyInit =
    (case (({-# LINE 233 "src\\PrettyCode.ag" #-}
            1
            {-# LINE 376 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOflags | _lhsOflags `seq` (True) ->
     ( _lhsOflags) })
-- AbcFlags ----------------------------------------------------
-- cata
sem_AbcFlags :: AbcFlags ->
                T_AbcFlags
sem_AbcFlags list =
    (Prelude.foldr sem_AbcFlags_Cons sem_AbcFlags_Nil (Prelude.map sem_AbcFlag list))
-- semantic domain
type T_AbcFlags = ( Word32)
sem_AbcFlags_Cons :: T_AbcFlag ->
                     T_AbcFlags ->
                     T_AbcFlags
sem_AbcFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIflags) | True ->
         (case (hd_) of
          { ( _hdIflags) | True ->
              (case (({-# LINE 185 "src\\PrettyCode.ag" #-}
                      _hdIflags .|. _tlIflags
                      {-# LINE 398 "src/PrettyCode.hs" #-}
                      )) of
               { _lhsOflags | _lhsOflags `seq` (True) ->
               ( _lhsOflags) }) }) })
sem_AbcFlags_Nil :: T_AbcFlags
sem_AbcFlags_Nil =
    (case (({-# LINE 185 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 406 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOflags | _lhsOflags `seq` (True) ->
     ( _lhsOflags) })
-- BodyInfo ----------------------------------------------------
-- cata
sem_BodyInfo :: BodyInfo ->
                T_BodyInfo
sem_BodyInfo (BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth _instructions _exceptions _traits) =
    (sem_BodyInfo_Info _method _maxStack _localCount _initScopeDepth _maxScopeDepth (sem_Instructions _instructions) (sem_Exceptions _exceptions) (sem_Traits _traits))
-- semantic domain
type T_BodyInfo = ( Builder)
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
    (case (instructions_) of
     { ( _instructionsIoutput) | True ->
         (case (({-# LINE 525 "src\\PrettyCode.ag" #-}
                 toLazyByteString _instructionsIoutput
                 {-# LINE 432 "src/PrettyCode.hs" #-}
                 )) of
          { _content | _content `seq` (True) ->
          (case (({-# LINE 526 "src\\PrettyCode.ag" #-}
                  fromIntegral $ B.length _content
                  {-# LINE 437 "src/PrettyCode.hs" #-}
                  )) of
           { _length | _length `seq` (True) ->
           (case (traits_) of
            { ( _traitsIcount,_traitsIoutput) | True ->
                (case (exceptions_) of
                 { ( _exceptionsIcount,_exceptionsIoutput) | True ->
                     (case (({-# LINE 520 "src\\PrettyCode.ag" #-}
                             u30 method_ ## u30 maxStack_ ## u30 localCount_ ##
                             u30 initScopeDepth_ ## u30 maxScopeDepth_ ##
                             u30size _length     ## _content     ##
                             u30size _exceptionsIcount ## _exceptionsIoutput ##
                             u30size _traitsIcount ## _traitsIoutput
                             {-# LINE 450 "src/PrettyCode.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }) })
-- BodyInfos ---------------------------------------------------
-- cata
sem_BodyInfos :: BodyInfos ->
                 T_BodyInfos
sem_BodyInfos list =
    (Prelude.foldr sem_BodyInfos_Cons sem_BodyInfos_Nil (Prelude.map sem_BodyInfo list))
-- semantic domain
type T_BodyInfos = ( Int,Builder)
sem_BodyInfos_Cons :: T_BodyInfo ->
                      T_BodyInfos ->
                      T_BodyInfos
sem_BodyInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 317 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 470 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 317 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 475 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 317 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 480 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 487 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_BodyInfos_Nil :: T_BodyInfos
sem_BodyInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 495 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 500 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- CaseOffsets -------------------------------------------------
-- cata
sem_CaseOffsets :: CaseOffsets ->
                   T_CaseOffsets
sem_CaseOffsets list =
    (Prelude.foldr sem_CaseOffsets_Cons sem_CaseOffsets_Nil list)
-- semantic domain
type T_CaseOffsets = ( Int,Builder)
sem_CaseOffsets_Cons :: Word32 ->
                        T_CaseOffsets ->
                        T_CaseOffsets
sem_CaseOffsets_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 5 "src\\PrettyInstr.ag" #-}
                 _tlIcount
                 {-# LINE 520 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 5 "src\\PrettyInstr.ag" #-}
                  (+1)
                  {-# LINE 525 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 5 "src\\PrettyInstr.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 530 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 187 "src\\PrettyInstr.ag" #-}
                    s24 hd_ ## _tlIoutput
                    {-# LINE 535 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_CaseOffsets_Nil :: T_CaseOffsets
sem_CaseOffsets_Nil =
    (case (({-# LINE 4 "src\\PrettyInstr.ag" #-}
            0
            {-# LINE 543 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 548 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- ClassInfo ---------------------------------------------------
-- cata
sem_ClassInfo :: ClassInfo ->
                 T_ClassInfo
sem_ClassInfo (ClassInfo_Info _con _traits) =
    (sem_ClassInfo_Info _con (sem_Traits _traits))
-- semantic domain
type T_ClassInfo = ( Builder)
sem_ClassInfo_Info :: Word32 ->
                      T_Traits ->
                      T_ClassInfo
sem_ClassInfo_Info con_ traits_ =
    (case (traits_) of
     { ( _traitsIcount,_traitsIoutput) | True ->
         (case (({-# LINE 514 "src\\PrettyCode.ag" #-}
                 u30 con_ ## u30size _traitsIcount ## _traitsIoutput
                 {-# LINE 568 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- ClassInfos --------------------------------------------------
-- cata
sem_ClassInfos :: ClassInfos ->
                  T_ClassInfos
sem_ClassInfos list =
    (Prelude.foldr sem_ClassInfos_Cons sem_ClassInfos_Nil (Prelude.map sem_ClassInfo list))
-- semantic domain
type T_ClassInfos = ( Int,Builder)
sem_ClassInfos_Cons :: T_ClassInfo ->
                       T_ClassInfos ->
                       T_ClassInfos
sem_ClassInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 315 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 588 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 315 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 593 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 315 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 598 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 605 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_ClassInfos_Nil :: T_ClassInfos
sem_ClassInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 613 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 618 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- DebugType ---------------------------------------------------
-- cata
sem_DebugType :: DebugType ->
                 T_DebugType
sem_DebugType (DebugType_Local) =
    (sem_DebugType_Local)
-- semantic domain
type T_DebugType = ( Builder)
sem_DebugType_Local :: T_DebugType
sem_DebugType_Local =
    (case (({-# LINE 184 "src\\PrettyInstr.ag" #-}
            u8 0x01
            {-# LINE 634 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- Exception ---------------------------------------------------
-- cata
sem_Exception :: Exception ->
                 T_Exception
sem_Exception (Exception_Info _from _to _target _tp _name) =
    (sem_Exception_Info _from _to _target _tp _name)
-- semantic domain
type T_Exception = ( Builder)
sem_Exception_Info :: Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      Word32 ->
                      T_Exception
sem_Exception_Info from_ to_ target_ tp_ name_ =
    (case (({-# LINE 532 "src\\PrettyCode.ag" #-}
            u30 from_ ## u30 to_ ## u30 target_ ## u30 tp_ ## u30 name_
            {-# LINE 655 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- Exceptions --------------------------------------------------
-- cata
sem_Exceptions :: Exceptions ->
                  T_Exceptions
sem_Exceptions list =
    (Prelude.foldr sem_Exceptions_Cons sem_Exceptions_Nil (Prelude.map sem_Exception list))
-- semantic domain
type T_Exceptions = ( Int,Builder)
sem_Exceptions_Cons :: T_Exception ->
                       T_Exceptions ->
                       T_Exceptions
sem_Exceptions_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 333 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 675 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 333 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 680 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 333 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 685 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 692 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_Exceptions_Nil :: T_Exceptions
sem_Exceptions_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 700 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 705 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_InstanceFlag = ( InstanceFlag,Builder)
sem_InstanceFlag_ClassSealed :: T_InstanceFlag
sem_InstanceFlag_ClassSealed =
    (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
            InstanceFlag_ClassSealed
            {-# LINE 727 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 732 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 737 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_InstanceFlag_ClassFinal :: T_InstanceFlag
sem_InstanceFlag_ClassFinal =
    (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
            InstanceFlag_ClassFinal
            {-# LINE 745 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 750 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 755 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_InstanceFlag_ClassInterface :: T_InstanceFlag
sem_InstanceFlag_ClassInterface =
    (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
            InstanceFlag_ClassInterface
            {-# LINE 763 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 768 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 773 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_InstanceFlag_ClassProtected :: T_InstanceFlag
sem_InstanceFlag_ClassProtected =
    (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
            InstanceFlag_ClassProtected
            {-# LINE 781 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 786 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 791 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
-- InstanceFlags -----------------------------------------------
-- cata
sem_InstanceFlags :: InstanceFlags ->
                     T_InstanceFlags
sem_InstanceFlags list =
    (Prelude.foldr sem_InstanceFlags_Cons sem_InstanceFlags_Nil (Prelude.map sem_InstanceFlag list))
-- semantic domain
type T_InstanceFlags = ( InstanceFlags,Builder)
sem_InstanceFlags_Cons :: T_InstanceFlag ->
                          T_InstanceFlags ->
                          T_InstanceFlags
sem_InstanceFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIflags,_tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIflags,_hdIoutput) | True ->
              (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
                      (:) _hdIflags _tlIflags
                      {-# LINE 813 "src/PrettyCode.hs" #-}
                      )) of
               { _flags | _flags `seq` (True) ->
               (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
                       _flags
                       {-# LINE 818 "src/PrettyCode.hs" #-}
                       )) of
                { _lhsOflags | _lhsOflags `seq` (True) ->
                (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                        _hdIoutput ## _tlIoutput
                        {-# LINE 823 "src/PrettyCode.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOflags,_lhsOoutput) }) }) }) }) })
sem_InstanceFlags_Nil :: T_InstanceFlags
sem_InstanceFlags_Nil =
    (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
            []
            {-# LINE 831 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 446 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 836 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 841 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
-- InstanceInfo ------------------------------------------------
-- cata
sem_InstanceInfo :: InstanceInfo ->
                    T_InstanceInfo
sem_InstanceInfo (InstanceInfo_Info _name _super _flags _protectedNs _interfaces _constructor _traits) =
    (sem_InstanceInfo_Info _name _super (sem_InstanceFlags _flags) _protectedNs (sem_Interfaces _interfaces) _constructor (sem_Traits _traits))
-- semantic domain
type T_InstanceInfo = ( Builder)
sem_InstanceInfo_Info :: Word32 ->
                         Word32 ->
                         T_InstanceFlags ->
                         Word32 ->
                         T_Interfaces ->
                         Word32 ->
                         T_Traits ->
                         T_InstanceInfo
sem_InstanceInfo_Info name_ super_ flags_ protectedNs_ interfaces_ constructor_ traits_ =
    (case (flags_) of
     { ( _flagsIflags,_flagsIoutput) | True ->
         (case (({-# LINE 452 "src\\PrettyCode.ag" #-}
                 if InstanceFlag_ClassProtected `elem` _flagsIflags
                 then u30 protectedNs_
                 else empty
                 {-# LINE 868 "src/PrettyCode.hs" #-}
                 )) of
          { _prot | _prot `seq` (True) ->
          (case (traits_) of
           { ( _traitsIcount,_traitsIoutput) | True ->
               (case (interfaces_) of
                { ( _interfacesIcount,_interfacesIoutput) | True ->
                    (case (({-# LINE 449 "src\\PrettyCode.ag" #-}
                            u30 name_ ## u30 super_ ## u8 (mergeInstanceFlags _flagsIflags) ## _prot     ##
                            u30size _interfacesIcount ## _interfacesIoutput ## u30 constructor_ ##
                            u30size _traitsIcount ## _traitsIoutput
                            {-# LINE 879 "src/PrettyCode.hs" #-}
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
type T_InstanceInfos = ( Int,Builder)
sem_InstanceInfos_Cons :: T_InstanceInfo ->
                          T_InstanceInfos ->
                          T_InstanceInfos
sem_InstanceInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 314 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 899 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 314 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 904 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 314 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 909 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 916 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_InstanceInfos_Nil :: T_InstanceInfos
sem_InstanceInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 924 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 929 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_Instruction = ( Builder)
sem_Instruction_Add :: T_Instruction
sem_Instruction_Add =
    (case (({-# LINE 8 "src\\PrettyInstr.ag" #-}
            u8 0xA0
            {-# LINE 1295 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Add_i :: T_Instruction
sem_Instruction_Add_i =
    (case (({-# LINE 9 "src\\PrettyInstr.ag" #-}
            u8 0xC5
            {-# LINE 1303 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Add_d :: T_Instruction
sem_Instruction_Add_d =
    (case (({-# LINE 10 "src\\PrettyInstr.ag" #-}
            u8 0x9B
            {-# LINE 1311 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_ApplyType :: Word32 ->
                             T_Instruction
sem_Instruction_ApplyType name_ =
    (case (({-# LINE 11 "src\\PrettyInstr.ag" #-}
            u8 0x53 ## u30 name_
            {-# LINE 1320 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_AsType :: Word32 ->
                          T_Instruction
sem_Instruction_AsType name_ =
    (case (({-# LINE 12 "src\\PrettyInstr.ag" #-}
            u8 0x86 ## u30 name_
            {-# LINE 1329 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_AsTypeLate :: T_Instruction
sem_Instruction_AsTypeLate =
    (case (({-# LINE 13 "src\\PrettyInstr.ag" #-}
            u8 0x87
            {-# LINE 1337 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Breakpoint :: T_Instruction
sem_Instruction_Breakpoint =
    (case (({-# LINE 18 "src\\PrettyInstr.ag" #-}
            u8 0x01
            {-# LINE 1345 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_BreakLine :: Word32 ->
                             T_Instruction
sem_Instruction_BreakLine line_ =
    (case (({-# LINE 19 "src\\PrettyInstr.ag" #-}
            u8 0xF2 ## u30 line_
            {-# LINE 1354 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_BitAnd :: T_Instruction
sem_Instruction_BitAnd =
    (case (({-# LINE 14 "src\\PrettyInstr.ag" #-}
            u8 0xA8
            {-# LINE 1362 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_BitNot :: T_Instruction
sem_Instruction_BitNot =
    (case (({-# LINE 15 "src\\PrettyInstr.ag" #-}
            u8 0x97
            {-# LINE 1370 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_BitOr :: T_Instruction
sem_Instruction_BitOr =
    (case (({-# LINE 16 "src\\PrettyInstr.ag" #-}
            u8 0xA9
            {-# LINE 1378 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_BitXor :: T_Instruction
sem_Instruction_BitXor =
    (case (({-# LINE 17 "src\\PrettyInstr.ag" #-}
            u8 0xAA
            {-# LINE 1386 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Call :: Word32 ->
                        T_Instruction
sem_Instruction_Call argCount_ =
    (case (({-# LINE 20 "src\\PrettyInstr.ag" #-}
            u8 0x41 ## u30 argCount_
            {-# LINE 1395 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallInterface :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallInterface name_ argCount_ =
    (case (({-# LINE 21 "src\\PrettyInstr.ag" #-}
            u8 0x4D ## u30 name_ ## u30 argCount_
            {-# LINE 1405 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallMethod :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallMethod index_ argCount_ =
    (case (({-# LINE 22 "src\\PrettyInstr.ag" #-}
            u8 0x43 ## u30 index_ ## u30 argCount_
            {-# LINE 1415 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallProp :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_CallProp name_ argCount_ =
    (case (({-# LINE 23 "src\\PrettyInstr.ag" #-}
            u8 0x46 ## u30 name_ ## u30 argCount_
            {-# LINE 1425 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallPropLex :: Word32 ->
                               Word32 ->
                               T_Instruction
sem_Instruction_CallPropLex name_ argCount_ =
    (case (({-# LINE 24 "src\\PrettyInstr.ag" #-}
            u8 0x4C ## u30 name_ ## u30 argCount_
            {-# LINE 1435 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallPropVoid :: Word32 ->
                                Word32 ->
                                T_Instruction
sem_Instruction_CallPropVoid name_ argCount_ =
    (case (({-# LINE 25 "src\\PrettyInstr.ag" #-}
            u8 0x4F ## u30 name_ ## u30 argCount_
            {-# LINE 1445 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallStatic :: Word32 ->
                              Word32 ->
                              T_Instruction
sem_Instruction_CallStatic method_ argCount_ =
    (case (({-# LINE 26 "src\\PrettyInstr.ag" #-}
            u8 0x44 ## u30 method_ ## u30 argCount_
            {-# LINE 1455 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallSuper :: Word32 ->
                             Word32 ->
                             T_Instruction
sem_Instruction_CallSuper name_ argCount_ =
    (case (({-# LINE 27 "src\\PrettyInstr.ag" #-}
            u8 0x45 ## u30 name_ ## u30 argCount_
            {-# LINE 1465 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallSuperId :: T_Instruction
sem_Instruction_CallSuperId =
    (case (({-# LINE 28 "src\\PrettyInstr.ag" #-}
            u8 0x4B
            {-# LINE 1473 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CallSuperVoid :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_CallSuperVoid name_ argCount_ =
    (case (({-# LINE 29 "src\\PrettyInstr.ag" #-}
            u8 0x4E ## u30 name_ ## u30 argCount_
            {-# LINE 1483 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_CheckFilter :: T_Instruction
sem_Instruction_CheckFilter =
    (case (({-# LINE 30 "src\\PrettyInstr.ag" #-}
            u8 0x78
            {-# LINE 1491 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce :: Word32 ->
                          T_Instruction
sem_Instruction_Coerce name_ =
    (case (({-# LINE 31 "src\\PrettyInstr.ag" #-}
            u8 0x80 ## u30 name_
            {-# LINE 1500 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_a :: T_Instruction
sem_Instruction_Coerce_a =
    (case (({-# LINE 33 "src\\PrettyInstr.ag" #-}
            u8 0x82
            {-# LINE 1508 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_b :: T_Instruction
sem_Instruction_Coerce_b =
    (case (({-# LINE 32 "src\\PrettyInstr.ag" #-}
            u8 0x81
            {-# LINE 1516 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_d :: T_Instruction
sem_Instruction_Coerce_d =
    (case (({-# LINE 35 "src\\PrettyInstr.ag" #-}
            u8 0x84
            {-# LINE 1524 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_i :: T_Instruction
sem_Instruction_Coerce_i =
    (case (({-# LINE 34 "src\\PrettyInstr.ag" #-}
            u8 0x83
            {-# LINE 1532 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_o :: T_Instruction
sem_Instruction_Coerce_o =
    (case (({-# LINE 38 "src\\PrettyInstr.ag" #-}
            u8 0x89
            {-# LINE 1540 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_s :: T_Instruction
sem_Instruction_Coerce_s =
    (case (({-# LINE 36 "src\\PrettyInstr.ag" #-}
            u8 0x85
            {-# LINE 1548 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Coerce_u :: T_Instruction
sem_Instruction_Coerce_u =
    (case (({-# LINE 37 "src\\PrettyInstr.ag" #-}
            u8 0x88
            {-# LINE 1556 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Concat :: T_Instruction
sem_Instruction_Concat =
    (case (({-# LINE 39 "src\\PrettyInstr.ag" #-}
            u8 0x9A
            {-# LINE 1564 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Construct :: Word32 ->
                             T_Instruction
sem_Instruction_Construct argCount_ =
    (case (({-# LINE 40 "src\\PrettyInstr.ag" #-}
            u8 0x42 ## u30 argCount_
            {-# LINE 1573 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_ConstructProp :: Word32 ->
                                 Word32 ->
                                 T_Instruction
sem_Instruction_ConstructProp name_ argCount_ =
    (case (({-# LINE 41 "src\\PrettyInstr.ag" #-}
            u8 0x4A ## u30 name_ ## u30 argCount_
            {-# LINE 1583 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_ConstructSuper :: Word32 ->
                                  T_Instruction
sem_Instruction_ConstructSuper argCount_ =
    (case (({-# LINE 42 "src\\PrettyInstr.ag" #-}
            u8 0x49 ## u30 argCount_
            {-# LINE 1592 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_b :: T_Instruction
sem_Instruction_Convert_b =
    (case (({-# LINE 43 "src\\PrettyInstr.ag" #-}
            u8 0x76
            {-# LINE 1600 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_i :: T_Instruction
sem_Instruction_Convert_i =
    (case (({-# LINE 44 "src\\PrettyInstr.ag" #-}
            u8 0x73
            {-# LINE 1608 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_d :: T_Instruction
sem_Instruction_Convert_d =
    (case (({-# LINE 45 "src\\PrettyInstr.ag" #-}
            u8 0x75
            {-# LINE 1616 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_o :: T_Instruction
sem_Instruction_Convert_o =
    (case (({-# LINE 46 "src\\PrettyInstr.ag" #-}
            u8 0x77
            {-# LINE 1624 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_u :: T_Instruction
sem_Instruction_Convert_u =
    (case (({-# LINE 47 "src\\PrettyInstr.ag" #-}
            u8 0x74
            {-# LINE 1632 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Convert_s :: T_Instruction
sem_Instruction_Convert_s =
    (case (({-# LINE 48 "src\\PrettyInstr.ag" #-}
            u8 0x70
            {-# LINE 1640 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Debug :: T_DebugType ->
                         Word32 ->
                         Word32 ->
                         Word32 ->
                         T_Instruction
sem_Instruction_Debug tp_ name_ reg_ extra_ =
    (case (tp_) of
     { ( _tpIoutput) | True ->
         (case (({-# LINE 49 "src\\PrettyInstr.ag" #-}
                 u8 0xEF ## _tpIoutput ## u30 name_ ## u30 reg_ ## u30 extra_
                 {-# LINE 1654 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
sem_Instruction_DebugFile :: Word32 ->
                             T_Instruction
sem_Instruction_DebugFile name_ =
    (case (({-# LINE 50 "src\\PrettyInstr.ag" #-}
            u8 0xF1 ## u30 name_
            {-# LINE 1663 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DebugLine :: Word32 ->
                             T_Instruction
sem_Instruction_DebugLine line_ =
    (case (({-# LINE 51 "src\\PrettyInstr.ag" #-}
            u8 0xF0 ## u30 line_
            {-# LINE 1672 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DecLocal :: Word32 ->
                            T_Instruction
sem_Instruction_DecLocal reg_ =
    (case (({-# LINE 52 "src\\PrettyInstr.ag" #-}
            u8 0x94 ## u30 reg_
            {-# LINE 1681 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DecLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_DecLocal_i reg_ =
    (case (({-# LINE 53 "src\\PrettyInstr.ag" #-}
            u8 0xC3 ## u30 reg_
            {-# LINE 1690 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Decrement :: T_Instruction
sem_Instruction_Decrement =
    (case (({-# LINE 54 "src\\PrettyInstr.ag" #-}
            u8 0x93
            {-# LINE 1698 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Decrement_i :: T_Instruction
sem_Instruction_Decrement_i =
    (case (({-# LINE 55 "src\\PrettyInstr.ag" #-}
            u8 0xC1
            {-# LINE 1706 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DeleteProperty :: Word32 ->
                                  T_Instruction
sem_Instruction_DeleteProperty name_ =
    (case (({-# LINE 56 "src\\PrettyInstr.ag" #-}
            u8 0x6A ## u30 name_
            {-# LINE 1715 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DeletePropertyLate :: T_Instruction
sem_Instruction_DeletePropertyLate =
    (case (({-# LINE 57 "src\\PrettyInstr.ag" #-}
            u8 0x6B
            {-# LINE 1723 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Divide :: T_Instruction
sem_Instruction_Divide =
    (case (({-# LINE 58 "src\\PrettyInstr.ag" #-}
            u8 0xA3
            {-# LINE 1731 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Dup :: T_Instruction
sem_Instruction_Dup =
    (case (({-# LINE 59 "src\\PrettyInstr.ag" #-}
            u8 0x2A
            {-# LINE 1739 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Dxns :: Word32 ->
                        T_Instruction
sem_Instruction_Dxns name_ =
    (case (({-# LINE 60 "src\\PrettyInstr.ag" #-}
            u8 0x06 ## u30 name_
            {-# LINE 1748 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_DxnsLate :: T_Instruction
sem_Instruction_DxnsLate =
    (case (({-# LINE 61 "src\\PrettyInstr.ag" #-}
            u8 0x07
            {-# LINE 1756 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Equals :: T_Instruction
sem_Instruction_Equals =
    (case (({-# LINE 62 "src\\PrettyInstr.ag" #-}
            u8 0xAB
            {-# LINE 1764 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_EscXAttr :: T_Instruction
sem_Instruction_EscXAttr =
    (case (({-# LINE 63 "src\\PrettyInstr.ag" #-}
            u8 0x72
            {-# LINE 1772 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_EscXElem :: T_Instruction
sem_Instruction_EscXElem =
    (case (({-# LINE 64 "src\\PrettyInstr.ag" #-}
            u8 0x71
            {-# LINE 1780 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_FindDef :: Word32 ->
                           T_Instruction
sem_Instruction_FindDef name_ =
    (case (({-# LINE 65 "src\\PrettyInstr.ag" #-}
            u8 0x5F ## u30 name_
            {-# LINE 1789 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_FindPropertyGlobalStrict :: Word32 ->
                                            T_Instruction
sem_Instruction_FindPropertyGlobalStrict name_ =
    (case (({-# LINE 66 "src\\PrettyInstr.ag" #-}
            u8 0x5B ## u30 name_
            {-# LINE 1798 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_FindPropertyGlobal :: Word32 ->
                                      T_Instruction
sem_Instruction_FindPropertyGlobal name_ =
    (case (({-# LINE 67 "src\\PrettyInstr.ag" #-}
            u8 0x5C ## u30 name_
            {-# LINE 1807 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_FindProperty :: Word32 ->
                                T_Instruction
sem_Instruction_FindProperty name_ =
    (case (({-# LINE 68 "src\\PrettyInstr.ag" #-}
            u8 0x5E ## u30 name_
            {-# LINE 1816 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_FindPropStrict :: Word32 ->
                                  T_Instruction
sem_Instruction_FindPropStrict name_ =
    (case (({-# LINE 69 "src\\PrettyInstr.ag" #-}
            u8 0x5D ## u30 name_
            {-# LINE 1825 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetDescendants :: Word32 ->
                                  T_Instruction
sem_Instruction_GetDescendants name_ =
    (case (({-# LINE 70 "src\\PrettyInstr.ag" #-}
            u8 0x59 ## u30 name_
            {-# LINE 1834 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetGlobalScope :: T_Instruction
sem_Instruction_GetGlobalScope =
    (case (({-# LINE 71 "src\\PrettyInstr.ag" #-}
            u8 0x64
            {-# LINE 1842 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_GetGlobalSlot slot_ =
    (case (({-# LINE 72 "src\\PrettyInstr.ag" #-}
            u8 0x6E ## u30 slot_
            {-# LINE 1851 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLex :: Word32 ->
                          T_Instruction
sem_Instruction_GetLex name_ =
    (case (({-# LINE 73 "src\\PrettyInstr.ag" #-}
            u8 0x60 ## u30 name_
            {-# LINE 1860 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_GetLocal reg_ =
    (case (({-# LINE 74 "src\\PrettyInstr.ag" #-}
            u8 0x62 ## u30 reg_
            {-# LINE 1869 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLocal0 :: T_Instruction
sem_Instruction_GetLocal0 =
    (case (({-# LINE 75 "src\\PrettyInstr.ag" #-}
            u8 0xD0
            {-# LINE 1877 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLocal1 :: T_Instruction
sem_Instruction_GetLocal1 =
    (case (({-# LINE 76 "src\\PrettyInstr.ag" #-}
            u8 0xD1
            {-# LINE 1885 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLocal2 :: T_Instruction
sem_Instruction_GetLocal2 =
    (case (({-# LINE 77 "src\\PrettyInstr.ag" #-}
            u8 0xD2
            {-# LINE 1893 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetLocal3 :: T_Instruction
sem_Instruction_GetLocal3 =
    (case (({-# LINE 78 "src\\PrettyInstr.ag" #-}
            u8 0xD3
            {-# LINE 1901 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetOuterScope :: Word32 ->
                                 T_Instruction
sem_Instruction_GetOuterScope name_ =
    (case (({-# LINE 79 "src\\PrettyInstr.ag" #-}
            u8 0x67
            {-# LINE 1910 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_GetProperty name_ =
    (case (({-# LINE 80 "src\\PrettyInstr.ag" #-}
            u8 0x66 ## u30 name_
            {-# LINE 1919 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetScopeObject :: Word8 ->
                                  T_Instruction
sem_Instruction_GetScopeObject index_ =
    (case (({-# LINE 81 "src\\PrettyInstr.ag" #-}
            u8 0x65 ## u8 index_
            {-# LINE 1928 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_GetSlot slot_ =
    (case (({-# LINE 82 "src\\PrettyInstr.ag" #-}
            u8 0x6C ## u30 slot_
            {-# LINE 1937 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_GetSuper name_ =
    (case (({-# LINE 83 "src\\PrettyInstr.ag" #-}
            u8 0x04 ## u30 name_
            {-# LINE 1946 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GreaterEquals :: T_Instruction
sem_Instruction_GreaterEquals =
    (case (({-# LINE 84 "src\\PrettyInstr.ag" #-}
            u8 0xB0
            {-# LINE 1954 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_GreaterThan :: T_Instruction
sem_Instruction_GreaterThan =
    (case (({-# LINE 85 "src\\PrettyInstr.ag" #-}
            u8 0xAF
            {-# LINE 1962 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_HasNext :: T_Instruction
sem_Instruction_HasNext =
    (case (({-# LINE 86 "src\\PrettyInstr.ag" #-}
            u8 0x1F
            {-# LINE 1970 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_HasNext2 :: Word32 ->
                            Word32 ->
                            T_Instruction
sem_Instruction_HasNext2 objectReg_ indexReg_ =
    (case (({-# LINE 87 "src\\PrettyInstr.ag" #-}
            u8 0x32 ## u30 objectReg_ ## u30 indexReg_
            {-# LINE 1980 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfEq :: Word32 ->
                        T_Instruction
sem_Instruction_IfEq offset_ =
    (case (({-# LINE 88 "src\\PrettyInstr.ag" #-}
            u8 0x13 ## s24 offset_
            {-# LINE 1989 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfFalse :: Word32 ->
                           T_Instruction
sem_Instruction_IfFalse offset_ =
    (case (({-# LINE 89 "src\\PrettyInstr.ag" #-}
            u8 0x12 ## s24 offset_
            {-# LINE 1998 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfGe :: Word32 ->
                        T_Instruction
sem_Instruction_IfGe offset_ =
    (case (({-# LINE 90 "src\\PrettyInstr.ag" #-}
            u8 0x18 ## s24 offset_
            {-# LINE 2007 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfGt :: Word32 ->
                        T_Instruction
sem_Instruction_IfGt offset_ =
    (case (({-# LINE 91 "src\\PrettyInstr.ag" #-}
            u8 0x17 ## s24 offset_
            {-# LINE 2016 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfLe :: Word32 ->
                        T_Instruction
sem_Instruction_IfLe offset_ =
    (case (({-# LINE 92 "src\\PrettyInstr.ag" #-}
            u8 0x16 ## s24 offset_
            {-# LINE 2025 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfLt :: Word32 ->
                        T_Instruction
sem_Instruction_IfLt offset_ =
    (case (({-# LINE 93 "src\\PrettyInstr.ag" #-}
            u8 0x15 ## s24 offset_
            {-# LINE 2034 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfNGe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGe offset_ =
    (case (({-# LINE 94 "src\\PrettyInstr.ag" #-}
            u8 0x0F ## s24 offset_
            {-# LINE 2043 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfNGt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNGt offset_ =
    (case (({-# LINE 95 "src\\PrettyInstr.ag" #-}
            u8 0x0E ## s24 offset_
            {-# LINE 2052 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfNLe :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLe offset_ =
    (case (({-# LINE 96 "src\\PrettyInstr.ag" #-}
            u8 0x0D ## s24 offset_
            {-# LINE 2061 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfNLt :: Word32 ->
                         T_Instruction
sem_Instruction_IfNLt offset_ =
    (case (({-# LINE 97 "src\\PrettyInstr.ag" #-}
            u8 0x0C ## s24 offset_
            {-# LINE 2070 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfNe :: Word32 ->
                        T_Instruction
sem_Instruction_IfNe offset_ =
    (case (({-# LINE 98 "src\\PrettyInstr.ag" #-}
            u8 0x14 ## s24 offset_
            {-# LINE 2079 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfStrictEq :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictEq offset_ =
    (case (({-# LINE 99 "src\\PrettyInstr.ag" #-}
            u8 0x19 ## s24 offset_
            {-# LINE 2088 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfStrictNe :: Word32 ->
                              T_Instruction
sem_Instruction_IfStrictNe offset_ =
    (case (({-# LINE 100 "src\\PrettyInstr.ag" #-}
            u8 0x1A ## s24 offset_
            {-# LINE 2097 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IfTrue :: Word32 ->
                          T_Instruction
sem_Instruction_IfTrue offset_ =
    (case (({-# LINE 101 "src\\PrettyInstr.ag" #-}
            u8 0x11 ## s24 offset_
            {-# LINE 2106 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_In :: T_Instruction
sem_Instruction_In =
    (case (({-# LINE 102 "src\\PrettyInstr.ag" #-}
            u8 0xB4
            {-# LINE 2114 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IncLocal :: Word32 ->
                            T_Instruction
sem_Instruction_IncLocal reg_ =
    (case (({-# LINE 103 "src\\PrettyInstr.ag" #-}
            u8 0x92 ## u30 reg_
            {-# LINE 2123 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IncLocal_i :: Word32 ->
                              T_Instruction
sem_Instruction_IncLocal_i reg_ =
    (case (({-# LINE 104 "src\\PrettyInstr.ag" #-}
            u8 0xC2 ## u30 reg_
            {-# LINE 2132 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Increment :: T_Instruction
sem_Instruction_Increment =
    (case (({-# LINE 105 "src\\PrettyInstr.ag" #-}
            u8 0x91
            {-# LINE 2140 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Increment_i :: T_Instruction
sem_Instruction_Increment_i =
    (case (({-# LINE 106 "src\\PrettyInstr.ag" #-}
            u8 0xC0
            {-# LINE 2148 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_InitProperty :: Word32 ->
                                T_Instruction
sem_Instruction_InitProperty name_ =
    (case (({-# LINE 107 "src\\PrettyInstr.ag" #-}
            u8 0x68 ## u30 name_
            {-# LINE 2157 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_InstanceOf :: T_Instruction
sem_Instruction_InstanceOf =
    (case (({-# LINE 108 "src\\PrettyInstr.ag" #-}
            u8 0xB1
            {-# LINE 2165 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IsType :: Word32 ->
                          T_Instruction
sem_Instruction_IsType name_ =
    (case (({-# LINE 109 "src\\PrettyInstr.ag" #-}
            u8 0xB2 ## u30 name_
            {-# LINE 2174 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_IsTypeLate :: T_Instruction
sem_Instruction_IsTypeLate =
    (case (({-# LINE 110 "src\\PrettyInstr.ag" #-}
            u8 0xB3
            {-# LINE 2182 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Jump :: Word32 ->
                        T_Instruction
sem_Instruction_Jump offset_ =
    (case (({-# LINE 111 "src\\PrettyInstr.ag" #-}
            u8 0x10 ## s24 offset_
            {-# LINE 2191 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Kill :: Word32 ->
                        T_Instruction
sem_Instruction_Kill reg_ =
    (case (({-# LINE 112 "src\\PrettyInstr.ag" #-}
            u8 0x08 ## u30 reg_
            {-# LINE 2200 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Label :: T_Instruction
sem_Instruction_Label =
    (case (({-# LINE 113 "src\\PrettyInstr.ag" #-}
            u8 0x09
            {-# LINE 2208 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LessEquals :: T_Instruction
sem_Instruction_LessEquals =
    (case (({-# LINE 114 "src\\PrettyInstr.ag" #-}
            u8 0xAE
            {-# LINE 2216 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LessThan :: T_Instruction
sem_Instruction_LessThan =
    (case (({-# LINE 115 "src\\PrettyInstr.ag" #-}
            u8 0xAD
            {-# LINE 2224 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LoadFloat32 :: T_Instruction
sem_Instruction_LoadFloat32 =
    (case (({-# LINE 116 "src\\PrettyInstr.ag" #-}
            u8 0x38
            {-# LINE 2232 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LoadFloat64 :: T_Instruction
sem_Instruction_LoadFloat64 =
    (case (({-# LINE 117 "src\\PrettyInstr.ag" #-}
            u8 0x39
            {-# LINE 2240 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LoadIndirect8 :: T_Instruction
sem_Instruction_LoadIndirect8 =
    (case (({-# LINE 118 "src\\PrettyInstr.ag" #-}
            u8 0x35
            {-# LINE 2248 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LoadIndirect16 :: T_Instruction
sem_Instruction_LoadIndirect16 =
    (case (({-# LINE 119 "src\\PrettyInstr.ag" #-}
            u8 0x36
            {-# LINE 2256 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LoadIndirect32 :: T_Instruction
sem_Instruction_LoadIndirect32 =
    (case (({-# LINE 120 "src\\PrettyInstr.ag" #-}
            u8 0x37
            {-# LINE 2264 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_LookupSwitch :: Word32 ->
                                T_CaseOffsets ->
                                T_Instruction
sem_Instruction_LookupSwitch defaultOffset_ caseOffsets_ =
    (case (caseOffsets_) of
     { ( _caseOffsetsIcount,_caseOffsetsIoutput) | True ->
         (case (({-# LINE 121 "src\\PrettyInstr.ag" #-}
                 u8 0x1B ## s24 defaultOffset_ ## u30size (_caseOffsetsIcount - 1) ## _caseOffsetsIoutput
                 {-# LINE 2276 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
sem_Instruction_Lshift :: T_Instruction
sem_Instruction_Lshift =
    (case (({-# LINE 122 "src\\PrettyInstr.ag" #-}
            u8 0xA5
            {-# LINE 2284 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Modulo :: T_Instruction
sem_Instruction_Modulo =
    (case (({-# LINE 123 "src\\PrettyInstr.ag" #-}
            u8 0xA4
            {-# LINE 2292 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Multiply :: T_Instruction
sem_Instruction_Multiply =
    (case (({-# LINE 124 "src\\PrettyInstr.ag" #-}
            u8 0xA2
            {-# LINE 2300 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Multiply_i :: T_Instruction
sem_Instruction_Multiply_i =
    (case (({-# LINE 125 "src\\PrettyInstr.ag" #-}
            u8 0xC7
            {-# LINE 2308 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Negate :: T_Instruction
sem_Instruction_Negate =
    (case (({-# LINE 126 "src\\PrettyInstr.ag" #-}
            u8 0x90
            {-# LINE 2316 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Negate_i :: T_Instruction
sem_Instruction_Negate_i =
    (case (({-# LINE 127 "src\\PrettyInstr.ag" #-}
            u8 0xC4
            {-# LINE 2324 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewActivation :: T_Instruction
sem_Instruction_NewActivation =
    (case (({-# LINE 128 "src\\PrettyInstr.ag" #-}
            u8 0x57
            {-# LINE 2332 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewArray :: Word32 ->
                            T_Instruction
sem_Instruction_NewArray argCount_ =
    (case (({-# LINE 129 "src\\PrettyInstr.ag" #-}
            u8 0x56 ## u30 argCount_
            {-# LINE 2341 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewCatch :: Word32 ->
                            T_Instruction
sem_Instruction_NewCatch exception_ =
    (case (({-# LINE 130 "src\\PrettyInstr.ag" #-}
            u8 0x5A ## u30 exception_
            {-# LINE 2350 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewClass :: Word32 ->
                            T_Instruction
sem_Instruction_NewClass class_ =
    (case (({-# LINE 131 "src\\PrettyInstr.ag" #-}
            u8 0x58 ## u30 class_
            {-# LINE 2359 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewFunction :: Word32 ->
                               T_Instruction
sem_Instruction_NewFunction method_ =
    (case (({-# LINE 132 "src\\PrettyInstr.ag" #-}
            u8 0x40 ## u30 method_
            {-# LINE 2368 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NewObject :: Word32 ->
                             T_Instruction
sem_Instruction_NewObject argCount_ =
    (case (({-# LINE 133 "src\\PrettyInstr.ag" #-}
            u8 0x55 ## u30 argCount_
            {-# LINE 2377 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NextName :: T_Instruction
sem_Instruction_NextName =
    (case (({-# LINE 134 "src\\PrettyInstr.ag" #-}
            u8 0x1E
            {-# LINE 2385 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_NextValue :: T_Instruction
sem_Instruction_NextValue =
    (case (({-# LINE 135 "src\\PrettyInstr.ag" #-}
            u8 0x23
            {-# LINE 2393 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Nop :: T_Instruction
sem_Instruction_Nop =
    (case (({-# LINE 136 "src\\PrettyInstr.ag" #-}
            u8 0x02
            {-# LINE 2401 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Not :: T_Instruction
sem_Instruction_Not =
    (case (({-# LINE 137 "src\\PrettyInstr.ag" #-}
            u8 0x96
            {-# LINE 2409 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Pop :: T_Instruction
sem_Instruction_Pop =
    (case (({-# LINE 138 "src\\PrettyInstr.ag" #-}
            u8 0x29
            {-# LINE 2417 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PopScope :: T_Instruction
sem_Instruction_PopScope =
    (case (({-# LINE 139 "src\\PrettyInstr.ag" #-}
            u8 0x1D
            {-# LINE 2425 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushByte :: Word8 ->
                            T_Instruction
sem_Instruction_PushByte val_ =
    (case (({-# LINE 140 "src\\PrettyInstr.ag" #-}
            u8 0x24 ## u8 val_
            {-# LINE 2434 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushDouble :: Word32 ->
                              T_Instruction
sem_Instruction_PushDouble name_ =
    (case (({-# LINE 141 "src\\PrettyInstr.ag" #-}
            u8 0x2F ## u30 name_
            {-# LINE 2443 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushFalse :: T_Instruction
sem_Instruction_PushFalse =
    (case (({-# LINE 142 "src\\PrettyInstr.ag" #-}
            u8 0x27
            {-# LINE 2451 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushInt :: Word32 ->
                           T_Instruction
sem_Instruction_PushInt name_ =
    (case (({-# LINE 143 "src\\PrettyInstr.ag" #-}
            u8 0x2D ## u30 name_
            {-# LINE 2460 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushNamespace :: Word32 ->
                                 T_Instruction
sem_Instruction_PushNamespace name_ =
    (case (({-# LINE 144 "src\\PrettyInstr.ag" #-}
            u8 0x31 ## u30 name_
            {-# LINE 2469 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushNaN :: T_Instruction
sem_Instruction_PushNaN =
    (case (({-# LINE 145 "src\\PrettyInstr.ag" #-}
            u8 0x28
            {-# LINE 2477 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushNull :: T_Instruction
sem_Instruction_PushNull =
    (case (({-# LINE 146 "src\\PrettyInstr.ag" #-}
            u8 0x20
            {-# LINE 2485 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushScope :: T_Instruction
sem_Instruction_PushScope =
    (case (({-# LINE 147 "src\\PrettyInstr.ag" #-}
            u8 0x30
            {-# LINE 2493 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushShort :: Word32 ->
                             T_Instruction
sem_Instruction_PushShort val_ =
    (case (({-# LINE 148 "src\\PrettyInstr.ag" #-}
            u8 0x25 ## u30 val_
            {-# LINE 2502 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushString :: Word32 ->
                              T_Instruction
sem_Instruction_PushString name_ =
    (case (({-# LINE 149 "src\\PrettyInstr.ag" #-}
            u8 0x2C ## u30 name_
            {-# LINE 2511 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushTrue :: T_Instruction
sem_Instruction_PushTrue =
    (case (({-# LINE 150 "src\\PrettyInstr.ag" #-}
            u8 0x26
            {-# LINE 2519 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushUInt :: Word32 ->
                            T_Instruction
sem_Instruction_PushUInt name_ =
    (case (({-# LINE 151 "src\\PrettyInstr.ag" #-}
            u8 0x2E ## u30 name_
            {-# LINE 2528 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushUndefined :: T_Instruction
sem_Instruction_PushUndefined =
    (case (({-# LINE 152 "src\\PrettyInstr.ag" #-}
            u8 0x21
            {-# LINE 2536 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_PushWith :: T_Instruction
sem_Instruction_PushWith =
    (case (({-# LINE 153 "src\\PrettyInstr.ag" #-}
            u8 0x1C
            {-# LINE 2544 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_ReturnValue :: T_Instruction
sem_Instruction_ReturnValue =
    (case (({-# LINE 154 "src\\PrettyInstr.ag" #-}
            u8 0x48
            {-# LINE 2552 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_ReturnVoid :: T_Instruction
sem_Instruction_ReturnVoid =
    (case (({-# LINE 155 "src\\PrettyInstr.ag" #-}
            u8 0x47
            {-# LINE 2560 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Rshift :: T_Instruction
sem_Instruction_Rshift =
    (case (({-# LINE 156 "src\\PrettyInstr.ag" #-}
            u8 0xA6
            {-# LINE 2568 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetLocal :: Word32 ->
                            T_Instruction
sem_Instruction_SetLocal reg_ =
    (case (({-# LINE 157 "src\\PrettyInstr.ag" #-}
            u8 0x63 ## u30 reg_
            {-# LINE 2577 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetLocal0 :: T_Instruction
sem_Instruction_SetLocal0 =
    (case (({-# LINE 158 "src\\PrettyInstr.ag" #-}
            u8 0xD4
            {-# LINE 2585 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetLocal1 :: T_Instruction
sem_Instruction_SetLocal1 =
    (case (({-# LINE 159 "src\\PrettyInstr.ag" #-}
            u8 0xD5
            {-# LINE 2593 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetLocal2 :: T_Instruction
sem_Instruction_SetLocal2 =
    (case (({-# LINE 160 "src\\PrettyInstr.ag" #-}
            u8 0xD6
            {-# LINE 2601 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetLocal3 :: T_Instruction
sem_Instruction_SetLocal3 =
    (case (({-# LINE 161 "src\\PrettyInstr.ag" #-}
            u8 0xD7
            {-# LINE 2609 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetGlobalSlot :: Word32 ->
                                 T_Instruction
sem_Instruction_SetGlobalSlot slot_ =
    (case (({-# LINE 162 "src\\PrettyInstr.ag" #-}
            u8 0x6F ## u30 slot_
            {-# LINE 2618 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetProperty :: Word32 ->
                               T_Instruction
sem_Instruction_SetProperty name_ =
    (case (({-# LINE 163 "src\\PrettyInstr.ag" #-}
            u8 0x61 ## u30 name_
            {-# LINE 2627 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetPropertyLate :: T_Instruction
sem_Instruction_SetPropertyLate =
    (case (({-# LINE 164 "src\\PrettyInstr.ag" #-}
            u8 0x69
            {-# LINE 2635 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetSlot :: Word32 ->
                           T_Instruction
sem_Instruction_SetSlot slot_ =
    (case (({-# LINE 165 "src\\PrettyInstr.ag" #-}
            u8 0x6D ## u30 slot_
            {-# LINE 2644 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SetSuper :: Word32 ->
                            T_Instruction
sem_Instruction_SetSuper name_ =
    (case (({-# LINE 166 "src\\PrettyInstr.ag" #-}
            u8 0x05 ## u30 name_
            {-# LINE 2653 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SignExtend1 :: T_Instruction
sem_Instruction_SignExtend1 =
    (case (({-# LINE 167 "src\\PrettyInstr.ag" #-}
            u8 0x50
            {-# LINE 2661 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SignExtend8 :: T_Instruction
sem_Instruction_SignExtend8 =
    (case (({-# LINE 168 "src\\PrettyInstr.ag" #-}
            u8 0x51
            {-# LINE 2669 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_SignExtend16 :: T_Instruction
sem_Instruction_SignExtend16 =
    (case (({-# LINE 169 "src\\PrettyInstr.ag" #-}
            u8 0x52
            {-# LINE 2677 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StoreFloat32 :: T_Instruction
sem_Instruction_StoreFloat32 =
    (case (({-# LINE 170 "src\\PrettyInstr.ag" #-}
            u8 0x3D
            {-# LINE 2685 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StoreFloat64 :: T_Instruction
sem_Instruction_StoreFloat64 =
    (case (({-# LINE 171 "src\\PrettyInstr.ag" #-}
            u8 0x3E
            {-# LINE 2693 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StoreIndirect32 :: T_Instruction
sem_Instruction_StoreIndirect32 =
    (case (({-# LINE 174 "src\\PrettyInstr.ag" #-}
            u8 0x3C
            {-# LINE 2701 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StoreIndirect16 :: T_Instruction
sem_Instruction_StoreIndirect16 =
    (case (({-# LINE 173 "src\\PrettyInstr.ag" #-}
            u8 0x3B
            {-# LINE 2709 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StoreIndirect8 :: T_Instruction
sem_Instruction_StoreIndirect8 =
    (case (({-# LINE 172 "src\\PrettyInstr.ag" #-}
            u8 0x3A
            {-# LINE 2717 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_StrictEquals :: T_Instruction
sem_Instruction_StrictEquals =
    (case (({-# LINE 175 "src\\PrettyInstr.ag" #-}
            u8 0xAC
            {-# LINE 2725 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Substract :: T_Instruction
sem_Instruction_Substract =
    (case (({-# LINE 176 "src\\PrettyInstr.ag" #-}
            u8 0xA1
            {-# LINE 2733 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Substract_i :: T_Instruction
sem_Instruction_Substract_i =
    (case (({-# LINE 177 "src\\PrettyInstr.ag" #-}
            u8 0xC6
            {-# LINE 2741 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Swap :: T_Instruction
sem_Instruction_Swap =
    (case (({-# LINE 178 "src\\PrettyInstr.ag" #-}
            u8 0x2B
            {-# LINE 2749 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Throw :: T_Instruction
sem_Instruction_Throw =
    (case (({-# LINE 179 "src\\PrettyInstr.ag" #-}
            u8 0x03
            {-# LINE 2757 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Timestamp :: T_Instruction
sem_Instruction_Timestamp =
    (case (({-# LINE 180 "src\\PrettyInstr.ag" #-}
            u8 0xF3
            {-# LINE 2765 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_TypeOf :: T_Instruction
sem_Instruction_TypeOf =
    (case (({-# LINE 181 "src\\PrettyInstr.ag" #-}
            u8 0x95
            {-# LINE 2773 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Urshift :: T_Instruction
sem_Instruction_Urshift =
    (case (({-# LINE 182 "src\\PrettyInstr.ag" #-}
            u8 0xA7
            {-# LINE 2781 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_Instruction_Location :: Int ->
                            T_Instruction
sem_Instruction_Location index_ =
    (case (({-# LINE 529 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 2790 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ( Builder)
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                      _hdIoutput ## _tlIoutput
                      {-# LINE 2812 "src/PrettyCode.hs" #-}
                      )) of
               { _lhsOoutput | _lhsOoutput `seq` (True) ->
               ( _lhsOoutput) }) }) })
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 2820 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- Interfaces --------------------------------------------------
-- cata
sem_Interfaces :: Interfaces ->
                  T_Interfaces
sem_Interfaces list =
    (Prelude.foldr sem_Interfaces_Cons sem_Interfaces_Nil list)
-- semantic domain
type T_Interfaces = ( Int,Builder)
sem_Interfaces_Cons :: Word32 ->
                       T_Interfaces ->
                       T_Interfaces
sem_Interfaces_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 331 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 2840 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 331 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 2845 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 331 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 2850 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 467 "src\\PrettyCode.ag" #-}
                    u30 hd_ ## _tlIoutput
                    {-# LINE 2855 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_Interfaces_Nil :: T_Interfaces
sem_Interfaces_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 2863 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 2868 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- MetaInfo ----------------------------------------------------
-- cata
sem_MetaInfo :: MetaInfo ->
                T_MetaInfo
sem_MetaInfo (MetaInfo_Info _name _items) =
    (sem_MetaInfo_Info _name (sem_MetaItems _items))
-- semantic domain
type T_MetaInfo = ( Builder)
sem_MetaInfo_Info :: Word32 ->
                     T_MetaItems ->
                     T_MetaInfo
sem_MetaInfo_Info name_ items_ =
    (case (items_) of
     { ( _itemsIcount,_itemsIoutput) | True ->
         (case (({-# LINE 441 "src\\PrettyCode.ag" #-}
                 u30 name_ ## u30size _itemsIcount ## _itemsIoutput
                 {-# LINE 2888 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- MetaInfos ---------------------------------------------------
-- cata
sem_MetaInfos :: MetaInfos ->
                 T_MetaInfos
sem_MetaInfos list =
    (Prelude.foldr sem_MetaInfos_Cons sem_MetaInfos_Nil (Prelude.map sem_MetaInfo list))
-- semantic domain
type T_MetaInfos = ( Int,Builder)
sem_MetaInfos_Cons :: T_MetaInfo ->
                      T_MetaInfos ->
                      T_MetaInfos
sem_MetaInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 313 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 2908 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 313 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 2913 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 313 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 2918 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 2925 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_MetaInfos_Nil :: T_MetaInfos
sem_MetaInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 2933 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 2938 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- MetaItem ----------------------------------------------------
-- cata
sem_MetaItem :: MetaItem ->
                T_MetaItem
sem_MetaItem (MetaItem_Item _key _value) =
    (sem_MetaItem_Item _key _value)
-- semantic domain
type T_MetaItem = ( Builder)
sem_MetaItem_Item :: Word32 ->
                     Word32 ->
                     T_MetaItem
sem_MetaItem_Item key_ value_ =
    (case (({-# LINE 444 "src\\PrettyCode.ag" #-}
            u30 key_ ## u30 value_
            {-# LINE 2956 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- MetaItems ---------------------------------------------------
-- cata
sem_MetaItems :: MetaItems ->
                 T_MetaItems
sem_MetaItems list =
    (Prelude.foldr sem_MetaItems_Cons sem_MetaItems_Nil (Prelude.map sem_MetaItem list))
-- semantic domain
type T_MetaItems = ( Int,Builder)
sem_MetaItems_Cons :: T_MetaItem ->
                      T_MetaItems ->
                      T_MetaItems
sem_MetaItems_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 329 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 2976 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 329 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 2981 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 329 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 2986 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 2993 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_MetaItems_Nil :: T_MetaItems
sem_MetaItems_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 3001 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 3006 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_MethodFlag = ( MethodFlag,Builder)
sem_MethodFlag_NeedArgs :: T_MethodFlag
sem_MethodFlag_NeedArgs =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_NeedArgs
            {-# LINE 3032 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3037 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3042 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_MethodFlag_NeedAct :: T_MethodFlag
sem_MethodFlag_NeedAct =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_NeedAct
            {-# LINE 3050 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3055 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3060 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_MethodFlag_NeedRest :: T_MethodFlag
sem_MethodFlag_NeedRest =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_NeedRest
            {-# LINE 3068 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3073 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3078 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_MethodFlag_HasOptionals :: T_MethodFlag
sem_MethodFlag_HasOptionals =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_HasOptionals
            {-# LINE 3086 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3091 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3096 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_MethodFlag_SetDXNS :: T_MethodFlag
sem_MethodFlag_SetDXNS =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_SetDXNS
            {-# LINE 3104 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3109 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3114 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_MethodFlag_HasParamNames :: T_MethodFlag
sem_MethodFlag_HasParamNames =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            MethodFlag_HasParamNames
            {-# LINE 3122 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3127 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3132 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
-- MethodFlags -------------------------------------------------
-- cata
sem_MethodFlags :: MethodFlags ->
                   T_MethodFlags
sem_MethodFlags list =
    (Prelude.foldr sem_MethodFlags_Cons sem_MethodFlags_Nil (Prelude.map sem_MethodFlag list))
-- semantic domain
type T_MethodFlags = ( MethodFlags,Builder)
sem_MethodFlags_Cons :: T_MethodFlag ->
                        T_MethodFlags ->
                        T_MethodFlags
sem_MethodFlags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIflags,_tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIflags,_hdIoutput) | True ->
              (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
                      (:) _hdIflags _tlIflags
                      {-# LINE 3154 "src/PrettyCode.hs" #-}
                      )) of
               { _flags | _flags `seq` (True) ->
               (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
                       _flags
                       {-# LINE 3159 "src/PrettyCode.hs" #-}
                       )) of
                { _lhsOflags | _lhsOflags `seq` (True) ->
                (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                        _hdIoutput ## _tlIoutput
                        {-# LINE 3164 "src/PrettyCode.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOflags,_lhsOoutput) }) }) }) }) })
sem_MethodFlags_Nil :: T_MethodFlags
sem_MethodFlags_Nil =
    (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
            []
            {-# LINE 3172 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 396 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 3177 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3182 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
-- MethodInfo --------------------------------------------------
-- cata
sem_MethodInfo :: MethodInfo ->
                  T_MethodInfo
sem_MethodInfo (MethodInfo_Info _return _params _name _flags _options _names) =
    (sem_MethodInfo_Info _return (sem_ParamTypes _params) _name (sem_MethodFlags _flags) (sem_Optionals _options) (sem_ParamNames _names))
-- semantic domain
type T_MethodInfo = ( Builder)
sem_MethodInfo_Info :: Word32 ->
                       T_ParamTypes ->
                       Word32 ->
                       T_MethodFlags ->
                       T_Optionals ->
                       T_ParamNames ->
                       T_MethodInfo
sem_MethodInfo_Info return_ params_ name_ flags_ options_ names_ =
    (case (names_) of
     { ( _namesIcount,_namesIoutput) | True ->
         (case (flags_) of
          { ( _flagsIflags,_flagsIoutput) | True ->
              (case (({-# LINE 402 "src\\PrettyCode.ag" #-}
                      if MethodFlag_HasParamNames `elem` _flagsIflags then _namesIoutput else empty
                      {-# LINE 3208 "src/PrettyCode.hs" #-}
                      )) of
               { _names | _names `seq` (True) ->
               (case (options_) of
                { ( _optionsIcount,_optionsIoutput) | True ->
                    (case (({-# LINE 401 "src\\PrettyCode.ag" #-}
                            if MethodFlag_HasOptionals  `elem` _flagsIflags then u30size _optionsIcount ## _optionsIoutput else empty
                            {-# LINE 3215 "src/PrettyCode.hs" #-}
                            )) of
                     { _options | _options `seq` (True) ->
                     (case (params_) of
                      { ( _paramsIcount,_paramsIoutput) | True ->
                          (case (({-# LINE 399 "src\\PrettyCode.ag" #-}
                                  u30size _paramsIcount ## u30 return_ ## _paramsIoutput ## u30 name_ ##
                                  u8 (mergeMethodFlags _flagsIflags) ## _options     ## _names
                                  {-# LINE 3223 "src/PrettyCode.hs" #-}
                                  )) of
                           { _lhsOoutput | _lhsOoutput `seq` (True) ->
                           ( _lhsOoutput) }) }) }) }) }) }) })
-- MethodInfos -------------------------------------------------
-- cata
sem_MethodInfos :: MethodInfos ->
                   T_MethodInfos
sem_MethodInfos list =
    (Prelude.foldr sem_MethodInfos_Cons sem_MethodInfos_Nil (Prelude.map sem_MethodInfo list))
-- semantic domain
type T_MethodInfos = ( Int,Builder)
sem_MethodInfos_Cons :: T_MethodInfo ->
                        T_MethodInfos ->
                        T_MethodInfos
sem_MethodInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 312 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 3243 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 312 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 3248 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 312 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 3253 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 3260 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_MethodInfos_Nil :: T_MethodInfos
sem_MethodInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 3268 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 3273 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_MultinameInfo = ( Builder)
sem_MultinameInfo_QName :: Word32 ->
                           Word32 ->
                           T_MultinameInfo
sem_MultinameInfo_QName namespace_ name_ =
    (case (({-# LINE 382 "src\\PrettyCode.ag" #-}
            u8 0x07 ## u30 namespace_ ## u30 name_
            {-# LINE 3311 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_QNameA :: Word32 ->
                            Word32 ->
                            T_MultinameInfo
sem_MultinameInfo_QNameA namespace_ name_ =
    (case (({-# LINE 383 "src\\PrettyCode.ag" #-}
            u8 0x0D ## u30 namespace_ ## u30 name_
            {-# LINE 3321 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_RTQName :: Word32 ->
                             T_MultinameInfo
sem_MultinameInfo_RTQName name_ =
    (case (({-# LINE 384 "src\\PrettyCode.ag" #-}
            u8 0x0F ## u30 name_
            {-# LINE 3330 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_RTQNameA :: Word32 ->
                              T_MultinameInfo
sem_MultinameInfo_RTQNameA name_ =
    (case (({-# LINE 385 "src\\PrettyCode.ag" #-}
            u8 0x10 ## u30 name_
            {-# LINE 3339 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_RTQNameL :: T_MultinameInfo
sem_MultinameInfo_RTQNameL =
    (case (({-# LINE 386 "src\\PrettyCode.ag" #-}
            u8 0x11
            {-# LINE 3347 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_RTQNameLA :: T_MultinameInfo
sem_MultinameInfo_RTQNameLA =
    (case (({-# LINE 387 "src\\PrettyCode.ag" #-}
            u8 0x12
            {-# LINE 3355 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_Multiname :: Word32 ->
                               Word32 ->
                               T_MultinameInfo
sem_MultinameInfo_Multiname name_ set_ =
    (case (({-# LINE 388 "src\\PrettyCode.ag" #-}
            u8 0x09 ## u30 name_ ## u30 set_
            {-# LINE 3365 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_MultinameA :: Word32 ->
                                Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameA name_ set_ =
    (case (({-# LINE 389 "src\\PrettyCode.ag" #-}
            u8 0x0E ## u30 name_ ## u30 set_
            {-# LINE 3375 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_MultinameL :: Word32 ->
                                T_MultinameInfo
sem_MultinameInfo_MultinameL set_ =
    (case (({-# LINE 390 "src\\PrettyCode.ag" #-}
            u8 0x1B ## u30 set_
            {-# LINE 3384 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_MultinameLA :: Word32 ->
                                 T_MultinameInfo
sem_MultinameInfo_MultinameLA set_ =
    (case (({-# LINE 391 "src\\PrettyCode.ag" #-}
            u8 0x1C ## u30 set_
            {-# LINE 3393 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_MultinameInfo_Generic :: Word32 ->
                             T_ParamNames ->
                             T_MultinameInfo
sem_MultinameInfo_Generic name_ params_ =
    (case (params_) of
     { ( _paramsIcount,_paramsIoutput) | True ->
         (case (({-# LINE 392 "src\\PrettyCode.ag" #-}
                 u8 0x1D ## u30 name_ ## u30size _paramsIcount ## _paramsIoutput
                 {-# LINE 3405 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- MultinameInfos ----------------------------------------------
-- cata
sem_MultinameInfos :: MultinameInfos ->
                      T_MultinameInfos
sem_MultinameInfos list =
    (Prelude.foldr sem_MultinameInfos_Cons sem_MultinameInfos_Nil (Prelude.map sem_MultinameInfo list))
-- semantic domain
type T_MultinameInfos = ( Int,Builder)
sem_MultinameInfos_Cons :: T_MultinameInfo ->
                           T_MultinameInfos ->
                           T_MultinameInfos
sem_MultinameInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 325 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 3425 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 325 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 3430 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 325 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 3435 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 3442 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_MultinameInfos_Nil :: T_MultinameInfos
sem_MultinameInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 3450 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 3455 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_MultinameKind = ( MultinameKind,Builder)
sem_MultinameKind_QName :: T_MultinameKind
sem_MultinameKind_QName =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_QName
            {-# LINE 3491 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3496 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3501 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_QNameA :: T_MultinameKind
sem_MultinameKind_QNameA =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_QNameA
            {-# LINE 3509 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3514 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3519 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_RTQName :: T_MultinameKind
sem_MultinameKind_RTQName =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_RTQName
            {-# LINE 3527 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3532 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3537 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_RTQNameA :: T_MultinameKind
sem_MultinameKind_RTQNameA =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_RTQNameA
            {-# LINE 3545 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3550 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3555 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_RTQNameL :: T_MultinameKind
sem_MultinameKind_RTQNameL =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_RTQNameL
            {-# LINE 3563 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3568 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3573 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_RTQNameLA :: T_MultinameKind
sem_MultinameKind_RTQNameLA =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_RTQNameLA
            {-# LINE 3581 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3586 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3591 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_Multiname :: T_MultinameKind
sem_MultinameKind_Multiname =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_Multiname
            {-# LINE 3599 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3604 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3609 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_MultinameA :: T_MultinameKind
sem_MultinameKind_MultinameA =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_MultinameA
            {-# LINE 3617 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3622 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3627 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_MultinameL :: T_MultinameKind
sem_MultinameKind_MultinameL =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_MultinameL
            {-# LINE 3635 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3640 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3645 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_MultinameLA :: T_MultinameKind
sem_MultinameKind_MultinameLA =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_MultinameLA
            {-# LINE 3653 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3658 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3663 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_MultinameKind_Generic :: T_MultinameKind
sem_MultinameKind_Generic =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            MultinameKind_Generic
            {-# LINE 3671 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3676 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3681 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
-- NamespaceInfo -----------------------------------------------
-- cata
sem_NamespaceInfo :: NamespaceInfo ->
                     T_NamespaceInfo
sem_NamespaceInfo (NamespaceInfo_Info _kind _name) =
    (sem_NamespaceInfo_Info (sem_NamespaceKind _kind) _name)
-- semantic domain
type T_NamespaceInfo = ( Builder)
sem_NamespaceInfo_Info :: T_NamespaceKind ->
                          Word32 ->
                          T_NamespaceInfo
sem_NamespaceInfo_Info kind_ name_ =
    (case (kind_) of
     { ( _kindIkind,_kindIoutput) | True ->
         (case (({-# LINE 363 "src\\PrettyCode.ag" #-}
                 u8 (fromNamespaceKind _kindIkind) ## u30 name_
                 {-# LINE 3701 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- NamespaceInfos ----------------------------------------------
-- cata
sem_NamespaceInfos :: NamespaceInfos ->
                      T_NamespaceInfos
sem_NamespaceInfos list =
    (Prelude.foldr sem_NamespaceInfos_Cons sem_NamespaceInfos_Nil (Prelude.map sem_NamespaceInfo list))
-- semantic domain
type T_NamespaceInfos = ( Int,Builder)
sem_NamespaceInfos_Cons :: T_NamespaceInfo ->
                           T_NamespaceInfos ->
                           T_NamespaceInfos
sem_NamespaceInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 322 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 3721 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 322 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 3726 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 322 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 3731 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 3738 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_NamespaceInfos_Nil :: T_NamespaceInfos
sem_NamespaceInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 3746 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 3751 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_NamespaceKind = ( NamespaceKind,Builder)
sem_NamespaceKind_General :: T_NamespaceKind
sem_NamespaceKind_General =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_General
            {-# LINE 3779 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3784 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3789 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Package :: T_NamespaceKind
sem_NamespaceKind_Package =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Package
            {-# LINE 3797 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3802 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3807 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Internal :: T_NamespaceKind
sem_NamespaceKind_Internal =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Internal
            {-# LINE 3815 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3820 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3825 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Protected :: T_NamespaceKind
sem_NamespaceKind_Protected =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Protected
            {-# LINE 3833 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3838 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3843 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Explicit :: T_NamespaceKind
sem_NamespaceKind_Explicit =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Explicit
            {-# LINE 3851 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3856 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3861 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Static :: T_NamespaceKind
sem_NamespaceKind_Static =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Static
            {-# LINE 3869 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3874 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3879 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_NamespaceKind_Private :: T_NamespaceKind
sem_NamespaceKind_Private =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            NamespaceKind_Private
            {-# LINE 3887 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 3892 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 3897 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
-- NamespaceNames ----------------------------------------------
-- cata
sem_NamespaceNames :: NamespaceNames ->
                      T_NamespaceNames
sem_NamespaceNames list =
    (Prelude.foldr sem_NamespaceNames_Cons sem_NamespaceNames_Nil list)
-- semantic domain
type T_NamespaceNames = ( Int,Builder)
sem_NamespaceNames_Cons :: Word32 ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 324 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 3917 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 324 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 3922 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 324 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 3927 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 379 "src\\PrettyCode.ag" #-}
                    u30 hd_ ## _tlIoutput
                    {-# LINE 3932 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 3940 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 3945 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- Optional ----------------------------------------------------
-- cata
sem_Optional :: Optional ->
                T_Optional
sem_Optional (Optional_Detail _val _kind) =
    (sem_Optional_Detail _val (sem_ValueKind _kind))
-- semantic domain
type T_Optional = ( Builder)
sem_Optional_Detail :: Word32 ->
                       T_ValueKind ->
                       T_Optional
sem_Optional_Detail val_ kind_ =
    (case (kind_) of
     { ( _kindIkind,_kindIoutput) | True ->
         (case (({-# LINE 418 "src\\PrettyCode.ag" #-}
                 u32 val_ ## u8 (fromValueKind _kindIkind)
                 {-# LINE 3965 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- Optionals ---------------------------------------------------
-- cata
sem_Optionals :: Optionals ->
                 T_Optionals
sem_Optionals list =
    (Prelude.foldr sem_Optionals_Cons sem_Optionals_Nil (Prelude.map sem_Optional list))
-- semantic domain
type T_Optionals = ( Int,Builder)
sem_Optionals_Cons :: T_Optional ->
                      T_Optionals ->
                      T_Optionals
sem_Optionals_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 326 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 3985 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 326 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 3990 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 326 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 3995 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 4002 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_Optionals_Nil :: T_Optionals
sem_Optionals_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4010 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4015 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- ParamNames --------------------------------------------------
-- cata
sem_ParamNames :: ParamNames ->
                  T_ParamNames
sem_ParamNames list =
    (Prelude.foldr sem_ParamNames_Cons sem_ParamNames_Nil list)
-- semantic domain
type T_ParamNames = ( Int,Builder)
sem_ParamNames_Cons :: Word32 ->
                       T_ParamNames ->
                       T_ParamNames
sem_ParamNames_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 327 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4035 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 327 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4040 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 327 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4045 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 394 "src\\PrettyCode.ag" #-}
                    u30 hd_ ## _tlIoutput
                    {-# LINE 4050 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_ParamNames_Nil :: T_ParamNames
sem_ParamNames_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4058 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4063 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- ParamTypes --------------------------------------------------
-- cata
sem_ParamTypes :: ParamTypes ->
                  T_ParamTypes
sem_ParamTypes list =
    (Prelude.foldr sem_ParamTypes_Cons sem_ParamTypes_Nil list)
-- semantic domain
type T_ParamTypes = ( Int,Builder)
sem_ParamTypes_Cons :: Word32 ->
                       T_ParamTypes ->
                       T_ParamTypes
sem_ParamTypes_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 328 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4083 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 328 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4088 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 328 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4093 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 416 "src\\PrettyCode.ag" #-}
                    u30 hd_ ## _tlIoutput
                    {-# LINE 4098 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_ParamTypes_Nil :: T_ParamTypes
sem_ParamTypes_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4106 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4111 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- PoolDoubles -------------------------------------------------
-- cata
sem_PoolDoubles :: PoolDoubles ->
                   T_PoolDoubles
sem_PoolDoubles list =
    (Prelude.foldr sem_PoolDoubles_Cons sem_PoolDoubles_Nil list)
-- semantic domain
type T_PoolDoubles = ( Int,Builder)
sem_PoolDoubles_Cons :: Double ->
                        T_PoolDoubles ->
                        T_PoolDoubles
sem_PoolDoubles_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 320 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4131 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 320 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4136 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 320 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4141 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 358 "src\\PrettyCode.ag" #-}
                    d64 hd_ ## _tlIoutput
                    {-# LINE 4146 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_PoolDoubles_Nil :: T_PoolDoubles
sem_PoolDoubles_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4154 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4159 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- PoolInfo ----------------------------------------------------
-- cata
sem_PoolInfo :: PoolInfo ->
                T_PoolInfo
sem_PoolInfo (PoolInfo_Info _integers _uintegers _doubles _strings _namespaces _namesets _multinames) =
    (sem_PoolInfo_Info (sem_PoolInts _integers) (sem_PoolUInts _uintegers) (sem_PoolDoubles _doubles) (sem_PoolStrings _strings) (sem_NamespaceInfos _namespaces) (sem_SetInfos _namesets) (sem_MultinameInfos _multinames))
-- semantic domain
type T_PoolInfo = ( Builder)
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
     { ( _multinamesIcount,_multinamesIoutput) | True ->
         (case (namesets_) of
          { ( _namesetsIcount,_namesetsIoutput) | True ->
              (case (namespaces_) of
               { ( _namespacesIcount,_namespacesIoutput) | True ->
                   (case (strings_) of
                    { ( _stringsIcount,_stringsIoutput) | True ->
                        (case (doubles_) of
                         { ( _doublesIcount,_doublesIoutput) | True ->
                             (case (uintegers_) of
                              { ( _uintegersIcount,_uintegersIoutput) | True ->
                                  (case (integers_) of
                                   { ( _integersIcount,_integersIoutput) | True ->
                                       (case (({-# LINE 344 "src\\PrettyCode.ag" #-}
                                               u30size1 _integersIcount   ## _integersIoutput   ##
                                               u30size1 _uintegersIcount  ## _uintegersIoutput  ##
                                               u30size1 _doublesIcount    ## _doublesIoutput    ##
                                               u30size1 _stringsIcount    ## _stringsIoutput    ##
                                               u30size1 _namespacesIcount ## _namespacesIoutput ##
                                               u30size1 _namesetsIcount   ## _namesetsIoutput   ##
                                               u30size1 _multinamesIcount ## _multinamesIoutput
                                               {-# LINE 4202 "src/PrettyCode.hs" #-}
                                               )) of
                                        { _lhsOoutput | _lhsOoutput `seq` (True) ->
                                        ( _lhsOoutput) }) }) }) }) }) }) }) })
-- PoolInts ----------------------------------------------------
-- cata
sem_PoolInts :: PoolInts ->
                T_PoolInts
sem_PoolInts list =
    (Prelude.foldr sem_PoolInts_Cons sem_PoolInts_Nil list)
-- semantic domain
type T_PoolInts = ( Int,Builder)
sem_PoolInts_Cons :: Word32 ->
                     T_PoolInts ->
                     T_PoolInts
sem_PoolInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 318 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4222 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 318 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4227 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 318 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4232 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 356 "src\\PrettyCode.ag" #-}
                    s32 hd_ ## _tlIoutput
                    {-# LINE 4237 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_PoolInts_Nil :: T_PoolInts
sem_PoolInts_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4245 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4250 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- PoolStrings -------------------------------------------------
-- cata
sem_PoolStrings :: PoolStrings ->
                   T_PoolStrings
sem_PoolStrings list =
    (Prelude.foldr sem_PoolStrings_Cons sem_PoolStrings_Nil list)
-- semantic domain
type T_PoolStrings = ( Int,Builder)
sem_PoolStrings_Cons :: ByteString ->
                        T_PoolStrings ->
                        T_PoolStrings
sem_PoolStrings_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 321 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4270 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 321 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4275 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 321 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4280 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 359 "src\\PrettyCode.ag" #-}
                    fromIntegral $ B.length hd_
                    {-# LINE 4285 "src/PrettyCode.hs" #-}
                    )) of
             { _blen | _blen `seq` (True) ->
             (case (({-# LINE 360 "src\\PrettyCode.ag" #-}
                     u30size _blen     ## hd_ ## _tlIoutput
                     {-# LINE 4290 "src/PrettyCode.hs" #-}
                     )) of
              { _lhsOoutput | _lhsOoutput `seq` (True) ->
              ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_PoolStrings_Nil :: T_PoolStrings
sem_PoolStrings_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4298 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4303 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- PoolUInts ---------------------------------------------------
-- cata
sem_PoolUInts :: PoolUInts ->
                 T_PoolUInts
sem_PoolUInts list =
    (Prelude.foldr sem_PoolUInts_Cons sem_PoolUInts_Nil list)
-- semantic domain
type T_PoolUInts = ( Int,Builder)
sem_PoolUInts_Cons :: Word32 ->
                      T_PoolUInts ->
                      T_PoolUInts
sem_PoolUInts_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 319 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4323 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 319 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4328 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 319 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4333 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 357 "src\\PrettyCode.ag" #-}
                    u32 hd_ ## _tlIoutput
                    {-# LINE 4338 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_PoolUInts_Nil :: T_PoolUInts
sem_PoolUInts_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4346 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4351 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- Rect --------------------------------------------------------
-- cata
sem_Rect :: Rect ->
            T_Rect
sem_Rect (Rect_Rect _bits _xMin _xMax _yMin _yMax) =
    (sem_Rect_Rect _bits _xMin _xMax _yMin _yMax)
-- semantic domain
type T_Rect = ( BitBuilder)
sem_Rect_Rect :: Int ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 Word32 ->
                 T_Rect
sem_Rect_Rect bits_ xMin_ xMax_ yMin_ yMax_ =
    (case (({-# LINE 204 "src\\PrettyCode.ag" #-}
            putWord (fromIntegral bits_) 5
              ### putWord xMin_ bits_ ### putWord yMin_ bits_
              ### putWord xMax_ bits_ ### putWord yMax_ bits_
            {-# LINE 4374 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- ScriptInfo --------------------------------------------------
-- cata
sem_ScriptInfo :: ScriptInfo ->
                  T_ScriptInfo
sem_ScriptInfo (ScriptInfo_Info _method _traits) =
    (sem_ScriptInfo_Info _method (sem_Traits _traits))
-- semantic domain
type T_ScriptInfo = ( Builder)
sem_ScriptInfo_Info :: Word32 ->
                       T_Traits ->
                       T_ScriptInfo
sem_ScriptInfo_Info method_ traits_ =
    (case (traits_) of
     { ( _traitsIcount,_traitsIoutput) | True ->
         (case (({-# LINE 517 "src\\PrettyCode.ag" #-}
                 u30 method_ ## u30size _traitsIcount ## _traitsIoutput
                 {-# LINE 4394 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- ScriptInfos -------------------------------------------------
-- cata
sem_ScriptInfos :: ScriptInfos ->
                   T_ScriptInfos
sem_ScriptInfos list =
    (Prelude.foldr sem_ScriptInfos_Cons sem_ScriptInfos_Nil (Prelude.map sem_ScriptInfo list))
-- semantic domain
type T_ScriptInfos = ( Int,Builder)
sem_ScriptInfos_Cons :: T_ScriptInfo ->
                        T_ScriptInfos ->
                        T_ScriptInfos
sem_ScriptInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 316 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4414 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 316 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4419 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 316 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4424 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 4431 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_ScriptInfos_Nil :: T_ScriptInfos
sem_ScriptInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4439 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4444 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- SetInfo -----------------------------------------------------
-- cata
sem_SetInfo :: SetInfo ->
               T_SetInfo
sem_SetInfo (SetInfo_Info _names) =
    (sem_SetInfo_Info (sem_NamespaceNames _names))
-- semantic domain
type T_SetInfo = ( Builder)
sem_SetInfo_Info :: T_NamespaceNames ->
                    T_SetInfo
sem_SetInfo_Info names_ =
    (case (names_) of
     { ( _namesIcount,_namesIoutput) | True ->
         (case (({-# LINE 377 "src\\PrettyCode.ag" #-}
                 u30size _namesIcount ## _namesIoutput
                 {-# LINE 4463 "src/PrettyCode.hs" #-}
                 )) of
          { _lhsOoutput | _lhsOoutput `seq` (True) ->
          ( _lhsOoutput) }) })
-- SetInfos ----------------------------------------------------
-- cata
sem_SetInfos :: SetInfos ->
                T_SetInfos
sem_SetInfos list =
    (Prelude.foldr sem_SetInfos_Cons sem_SetInfos_Nil (Prelude.map sem_SetInfo list))
-- semantic domain
type T_SetInfos = ( Int,Builder)
sem_SetInfos_Cons :: T_SetInfo ->
                     T_SetInfos ->
                     T_SetInfos
sem_SetInfos_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 323 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 4483 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 323 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 4488 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 323 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 4493 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 4500 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_SetInfos_Nil :: T_SetInfos
sem_SetInfos_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 4508 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 4513 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- SwfFile -----------------------------------------------------
-- cata
sem_SwfFile :: SwfFile ->
               T_SwfFile
sem_SwfFile (SwfFile_File _compressed _version _length _size _rate _count _tags) =
    (sem_SwfFile_File _compressed _version _length (sem_Rect _size) _rate _count (sem_Tags _tags))
-- semantic domain
type T_SwfFile = ( Builder)
data Inh_SwfFile = Inh_SwfFile {}
data Syn_SwfFile = Syn_SwfFile {output_Syn_SwfFile :: !(Builder)}
wrap_SwfFile :: T_SwfFile ->
                Inh_SwfFile ->
                Syn_SwfFile
wrap_SwfFile sem (Inh_SwfFile) =
    (let ( _lhsOoutput) | True = sem
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
    (case (tags_) of
     { ( _tagsIoutput) | True ->
         (case (size_) of
          { ( _sizeIoutput) | True ->
              (case (({-# LINE 198 "src\\PrettyCode.ag" #-}
                      padded _sizeIoutput
                        ## u16 rate_
                        ## u16 count_
                        ## _tagsIoutput
                      {-# LINE 4551 "src/PrettyCode.hs" #-}
                      )) of
               { _body | _body `seq` (True) ->
               (case (({-# LINE 197 "src\\PrettyCode.ag" #-}
                       toLazyByteString _body
                       {-# LINE 4556 "src/PrettyCode.hs" #-}
                       )) of
                { _bodyStr | _bodyStr `seq` (True) ->
                (case (({-# LINE 194 "src\\PrettyCode.ag" #-}
                        if compressed_
                        then compressWith defaultCompressParams { compressLevel = bestCompression } _bodyStr
                        else _bodyStr
                        {-# LINE 4563 "src/PrettyCode.hs" #-}
                        )) of
                 { _content | _content `seq` (True) ->
                 (case (({-# LINE 193 "src\\PrettyCode.ag" #-}
                         fromIntegral (8 + B.length _bodyStr    )
                         {-# LINE 4568 "src/PrettyCode.hs" #-}
                         )) of
                  { _length | _length `seq` (True) ->
                  (case (({-# LINE 192 "src\\PrettyCode.ag" #-}
                          u8 (if compressed_ then 0x43 else 0x46) ## u8 0x57 ## u8 0x53
                          {-# LINE 4573 "src/PrettyCode.hs" #-}
                          )) of
                   { _head | _head `seq` (True) ->
                   (case (({-# LINE 191 "src\\PrettyCode.ag" #-}
                           _head     ## u8 version_ ## w32 _length     ## _content
                           {-# LINE 4578 "src/PrettyCode.hs" #-}
                           )) of
                    { _lhsOoutput | _lhsOoutput `seq` (True) ->
                    ( _lhsOoutput) }) }) }) }) }) }) }) })
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
type T_Tag = ( Builder)
sem_Tag_Abc :: T_AbcFlags ->
               ByteString ->
               T_AbcFile ->
               T_Tag
sem_Tag_Abc flags_ name_ file_ =
    (case (file_) of
     { ( _fileIoutput) | True ->
         (case (flags_) of
          { ( _flagsIflags) | True ->
              (case (({-# LINE 211 "src\\PrettyCode.ag" #-}
                      w32 _flagsIflags ## nullString name_ ## _fileIoutput
                      {-# LINE 4607 "src/PrettyCode.hs" #-}
                      )) of
               { _file | _file `seq` (True) ->
               (case (({-# LINE 212 "src\\PrettyCode.ag" #-}
                       toLazyByteString _file
                       {-# LINE 4612 "src/PrettyCode.hs" #-}
                       )) of
                { _content | _content `seq` (True) ->
                (case (({-# LINE 213 "src\\PrettyCode.ag" #-}
                        fromIntegral $ B.length _content
                        {-# LINE 4617 "src/PrettyCode.hs" #-}
                        )) of
                 { _length | _length `seq` (True) ->
                 (case (({-# LINE 210 "src\\PrettyCode.ag" #-}
                         Tag_Opaque TagKind_DoABC _length     _content
                         {-# LINE 4622 "src/PrettyCode.hs" #-}
                         )) of
                  { body_val_ | body_val_ `seq` (True) ->
                  (case ((sem_Tag body_val_)) of
                   { body_inst_ | body_inst_ `seq` (True) ->
                   (case (body_inst_) of
                    { ( _bodyIoutput) | True ->
                        (case (({-# LINE 214 "src\\PrettyCode.ag" #-}
                                _bodyIoutput
                                {-# LINE 4631 "src/PrettyCode.hs" #-}
                                )) of
                         { _lhsOoutput | _lhsOoutput `seq` (True) ->
                         ( _lhsOoutput) }) }) }) }) }) }) }) }) })
sem_Tag_FileAttributes :: Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          Bool ->
                          T_Tag
sem_Tag_FileAttributes useDirectBlit_ useGPU_ hasMetaData_ hasAS3_ useNetwork_ =
    (case (({-# LINE 218 "src\\PrettyCode.ag" #-}
            padded (   putWord 0 1 ### putBit useDirectBlit_ ### putBit useGPU_ ### putBit hasMetaData_
                   ### putBit hasAS3_ ### putWord 0 2 ### putBit useNetwork_ ### putWord 0 24)
            {-# LINE 4645 "src/PrettyCode.hs" #-}
            )) of
     { _data | _data `seq` (True) ->
     (case (({-# LINE 220 "src\\PrettyCode.ag" #-}
             toLazyByteString _data
             {-# LINE 4650 "src/PrettyCode.hs" #-}
             )) of
      { _content | _content `seq` (True) ->
      (case (({-# LINE 221 "src\\PrettyCode.ag" #-}
              fromIntegral $ B.length _content
              {-# LINE 4655 "src/PrettyCode.hs" #-}
              )) of
       { _length | _length `seq` (True) ->
       (case (({-# LINE 217 "src\\PrettyCode.ag" #-}
               Tag_Opaque TagKind_FileAttributes _length     _content
               {-# LINE 4660 "src/PrettyCode.hs" #-}
               )) of
        { body_val_ | body_val_ `seq` (True) ->
        (case ((sem_Tag body_val_)) of
         { body_inst_ | body_inst_ `seq` (True) ->
         (case (body_inst_) of
          { ( _bodyIoutput) | True ->
              (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                      _bodyIoutput
                      {-# LINE 4669 "src/PrettyCode.hs" #-}
                      )) of
               { _lhsOoutput | _lhsOoutput `seq` (True) ->
               ( _lhsOoutput) }) }) }) }) }) }) })
sem_Tag_Opaque :: T_TagKind ->
                  Word32 ->
                  ByteString ->
                  T_Tag
sem_Tag_Opaque kind_ length_ body_ =
    (case (kind_) of
     { ( _kindIkind) | True ->
         (case (({-# LINE 223 "src\\PrettyCode.ag" #-}
                 fromTagKind _kindIkind
                 {-# LINE 4682 "src/PrettyCode.hs" #-}
                 )) of
          { _code | _code `seq` (True) ->
          (case (({-# LINE 222 "src\\PrettyCode.ag" #-}
                  min length_ 0x3f
                  {-# LINE 4687 "src/PrettyCode.hs" #-}
                  )) of
           { _len' | _len' `seq` (True) ->
           (case (({-# LINE 224 "src\\PrettyCode.ag" #-}
                   shiftL _code     6 .|. (fromIntegral _len'    )
                   {-# LINE 4692 "src/PrettyCode.hs" #-}
                   )) of
            { _key | _key `seq` (True) ->
            (case (({-# LINE 225 "src\\PrettyCode.ag" #-}
                    u16 _key     ##
                     if length_ >= 0x3F
                     then w32 length_
                     else empty
                    {-# LINE 4700 "src/PrettyCode.hs" #-}
                    )) of
             { _header | _header `seq` (True) ->
             (case (({-# LINE 229 "src\\PrettyCode.ag" #-}
                     _header     ## body_
                     {-# LINE 4705 "src/PrettyCode.hs" #-}
                     )) of
              { _lhsOoutput | _lhsOoutput `seq` (True) ->
              ( _lhsOoutput) }) }) }) }) }) })
sem_Tag_End :: T_Tag
sem_Tag_End =
    (case (({-# LINE 230 "src\\PrettyCode.ag" #-}
            u16 0
            {-# LINE 4713 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
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
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_End
            {-# LINE 4857 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4862 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_ShowFrame :: T_TagKind
sem_TagKind_ShowFrame =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_ShowFrame
            {-# LINE 4870 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4875 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineShape :: T_TagKind
sem_TagKind_DefineShape =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineShape
            {-# LINE 4883 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4888 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_PlaceObject :: T_TagKind
sem_TagKind_PlaceObject =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_PlaceObject
            {-# LINE 4896 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4901 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_RemoveObject :: T_TagKind
sem_TagKind_RemoveObject =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_RemoveObject
            {-# LINE 4909 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4914 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBits :: T_TagKind
sem_TagKind_DefineBits =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBits
            {-# LINE 4922 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4927 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineButton :: T_TagKind
sem_TagKind_DefineButton =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineButton
            {-# LINE 4935 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4940 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_JPEGTables :: T_TagKind
sem_TagKind_JPEGTables =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_JPEGTables
            {-# LINE 4948 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4953 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SetBackgroundColor :: T_TagKind
sem_TagKind_SetBackgroundColor =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SetBackgroundColor
            {-# LINE 4961 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4966 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFont :: T_TagKind
sem_TagKind_DefineFont =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFont
            {-# LINE 4974 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4979 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineText :: T_TagKind
sem_TagKind_DefineText =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineText
            {-# LINE 4987 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 4992 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DoAction :: T_TagKind
sem_TagKind_DoAction =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DoAction
            {-# LINE 5000 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5005 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFontInfo :: T_TagKind
sem_TagKind_DefineFontInfo =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFontInfo
            {-# LINE 5013 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5018 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineSound :: T_TagKind
sem_TagKind_DefineSound =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineSound
            {-# LINE 5026 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5031 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_StartSound :: T_TagKind
sem_TagKind_StartSound =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_StartSound
            {-# LINE 5039 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5044 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineButtonSound :: T_TagKind
sem_TagKind_DefineButtonSound =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineButtonSound
            {-# LINE 5052 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5057 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SoundStreamHead :: T_TagKind
sem_TagKind_SoundStreamHead =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SoundStreamHead
            {-# LINE 5065 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5070 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SoundStreamBlock :: T_TagKind
sem_TagKind_SoundStreamBlock =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SoundStreamBlock
            {-# LINE 5078 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5083 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBitsLossless :: T_TagKind
sem_TagKind_DefineBitsLossless =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBitsLossless
            {-# LINE 5091 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5096 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBitsJPEG2 :: T_TagKind
sem_TagKind_DefineBitsJPEG2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBitsJPEG2
            {-# LINE 5104 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5109 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineShape2 :: T_TagKind
sem_TagKind_DefineShape2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineShape2
            {-# LINE 5117 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5122 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineButtonCxform :: T_TagKind
sem_TagKind_DefineButtonCxform =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineButtonCxform
            {-# LINE 5130 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5135 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_Protect :: T_TagKind
sem_TagKind_Protect =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_Protect
            {-# LINE 5143 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5148 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_PlaceObject2 :: T_TagKind
sem_TagKind_PlaceObject2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_PlaceObject2
            {-# LINE 5156 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5161 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_RemoveObject2 :: T_TagKind
sem_TagKind_RemoveObject2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_RemoveObject2
            {-# LINE 5169 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5174 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineShape3 :: T_TagKind
sem_TagKind_DefineShape3 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineShape3
            {-# LINE 5182 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5187 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineText2 :: T_TagKind
sem_TagKind_DefineText2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineText2
            {-# LINE 5195 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5200 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineButton2 :: T_TagKind
sem_TagKind_DefineButton2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineButton2
            {-# LINE 5208 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5213 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBitsJPEG3 :: T_TagKind
sem_TagKind_DefineBitsJPEG3 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBitsJPEG3
            {-# LINE 5221 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5226 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBitsLossless2 :: T_TagKind
sem_TagKind_DefineBitsLossless2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBitsLossless2
            {-# LINE 5234 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5239 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineEditText :: T_TagKind
sem_TagKind_DefineEditText =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineEditText
            {-# LINE 5247 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5252 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineSprite :: T_TagKind
sem_TagKind_DefineSprite =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineSprite
            {-# LINE 5260 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5265 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_FrameLabel :: T_TagKind
sem_TagKind_FrameLabel =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_FrameLabel
            {-# LINE 5273 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5278 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SoundStreamHead2 :: T_TagKind
sem_TagKind_SoundStreamHead2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SoundStreamHead2
            {-# LINE 5286 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5291 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineMorphShape :: T_TagKind
sem_TagKind_DefineMorphShape =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineMorphShape
            {-# LINE 5299 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5304 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFont2 :: T_TagKind
sem_TagKind_DefineFont2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFont2
            {-# LINE 5312 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5317 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_ExportAssets :: T_TagKind
sem_TagKind_ExportAssets =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_ExportAssets
            {-# LINE 5325 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5330 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_ImportAssets :: T_TagKind
sem_TagKind_ImportAssets =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_ImportAssets
            {-# LINE 5338 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5343 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_EnableDebugger :: T_TagKind
sem_TagKind_EnableDebugger =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_EnableDebugger
            {-# LINE 5351 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5356 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DoInitAction :: T_TagKind
sem_TagKind_DoInitAction =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DoInitAction
            {-# LINE 5364 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5369 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineVideoStream :: T_TagKind
sem_TagKind_DefineVideoStream =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineVideoStream
            {-# LINE 5377 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5382 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_VideoFrame :: T_TagKind
sem_TagKind_VideoFrame =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_VideoFrame
            {-# LINE 5390 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5395 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFontInfo2 :: T_TagKind
sem_TagKind_DefineFontInfo2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFontInfo2
            {-# LINE 5403 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5408 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_EnableDebugger2 :: T_TagKind
sem_TagKind_EnableDebugger2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_EnableDebugger2
            {-# LINE 5416 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5421 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_ScriptLimits :: T_TagKind
sem_TagKind_ScriptLimits =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_ScriptLimits
            {-# LINE 5429 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5434 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SetTabIndex :: T_TagKind
sem_TagKind_SetTabIndex =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SetTabIndex
            {-# LINE 5442 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5447 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_FileAttributes :: T_TagKind
sem_TagKind_FileAttributes =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_FileAttributes
            {-# LINE 5455 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5460 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_PlaceObject3 :: T_TagKind
sem_TagKind_PlaceObject3 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_PlaceObject3
            {-# LINE 5468 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5473 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_ImportAssets2 :: T_TagKind
sem_TagKind_ImportAssets2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_ImportAssets2
            {-# LINE 5481 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5486 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFontAlignZones :: T_TagKind
sem_TagKind_DefineFontAlignZones =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFontAlignZones
            {-# LINE 5494 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5499 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_CSMTextSettings :: T_TagKind
sem_TagKind_CSMTextSettings =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_CSMTextSettings
            {-# LINE 5507 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5512 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFont3 :: T_TagKind
sem_TagKind_DefineFont3 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFont3
            {-# LINE 5520 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5525 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_SymbolClass :: T_TagKind
sem_TagKind_SymbolClass =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_SymbolClass
            {-# LINE 5533 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5538 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_Metadata :: T_TagKind
sem_TagKind_Metadata =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_Metadata
            {-# LINE 5546 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5551 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineScalingGrid :: T_TagKind
sem_TagKind_DefineScalingGrid =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineScalingGrid
            {-# LINE 5559 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5564 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DoABC :: T_TagKind
sem_TagKind_DoABC =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DoABC
            {-# LINE 5572 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5577 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineShape4 :: T_TagKind
sem_TagKind_DefineShape4 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineShape4
            {-# LINE 5585 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5590 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineMorphShape2 :: T_TagKind
sem_TagKind_DefineMorphShape2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineMorphShape2
            {-# LINE 5598 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5603 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineSceneAndFrameLabelData :: T_TagKind
sem_TagKind_DefineSceneAndFrameLabelData =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineSceneAndFrameLabelData
            {-# LINE 5611 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5616 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBinaryData :: T_TagKind
sem_TagKind_DefineBinaryData =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBinaryData
            {-# LINE 5624 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5629 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFontName :: T_TagKind
sem_TagKind_DefineFontName =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFontName
            {-# LINE 5637 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5642 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_StartSound2 :: T_TagKind
sem_TagKind_StartSound2 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_StartSound2
            {-# LINE 5650 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5655 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineBitsJPEG4 :: T_TagKind
sem_TagKind_DefineBitsJPEG4 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineBitsJPEG4
            {-# LINE 5663 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5668 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_DefineFont4 :: T_TagKind
sem_TagKind_DefineFont4 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_DefineFont4
            {-# LINE 5676 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5681 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
sem_TagKind_Other :: Word16 ->
                     T_TagKind
sem_TagKind_Other code_ =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            TagKind_Other code_
            {-# LINE 5690 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 5695 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      ( _lhsOkind) }) })
-- Tags --------------------------------------------------------
-- cata
sem_Tags :: Tags ->
            T_Tags
sem_Tags list =
    (Prelude.foldr sem_Tags_Cons sem_Tags_Nil (Prelude.map sem_Tag list))
-- semantic domain
type T_Tags = ( Builder)
sem_Tags_Cons :: T_Tag ->
                 T_Tags ->
                 T_Tags
sem_Tags_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIoutput) | True ->
              (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                      _hdIoutput ## _tlIoutput
                      {-# LINE 5717 "src/PrettyCode.hs" #-}
                      )) of
               { _lhsOoutput | _lhsOoutput `seq` (True) ->
               ( _lhsOoutput) }) }) })
sem_Tags_Nil :: T_Tags
sem_Tags_Nil =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 5725 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- Trait -------------------------------------------------------
-- cata
sem_Trait :: Trait ->
             T_Trait
sem_Trait (Trait_Trait _name _data _attrs _meta) =
    (sem_Trait_Trait _name (sem_TraitData _data) (sem_TraitAttrs _attrs) (sem_TraitMeta _meta))
-- semantic domain
type T_Trait = ( Builder)
sem_Trait_Trait :: Word32 ->
                   T_TraitData ->
                   T_TraitAttrs ->
                   T_TraitMeta ->
                   T_Trait
sem_Trait_Trait name_ data_ attrs_ meta_ =
    (case (attrs_) of
     { ( _attrsIflags,_attrsIoutput) | True ->
         (case (({-# LINE 477 "src\\PrettyCode.ag" #-}
                 mergeTraitFlags _attrsIflags
                 {-# LINE 5747 "src/PrettyCode.hs" #-}
                 )) of
          { _dataOflags | _dataOflags `seq` (True) ->
          (case (meta_) of
           { ( _metaIcount,_metaIoutput) | True ->
               (case (({-# LINE 474 "src\\PrettyCode.ag" #-}
                       if TraitAttr_Metadata `elem` _attrsIflags
                       then u30size _metaIcount ## _metaIoutput
                       else empty
                       {-# LINE 5756 "src/PrettyCode.hs" #-}
                       )) of
                { _optMet | _optMet `seq` (True) ->
                (case (data_ _dataOflags) of
                 { ( _dataIoutput) | True ->
                     (case (({-# LINE 473 "src\\PrettyCode.ag" #-}
                             u30 name_ ## _dataIoutput ## _optMet
                             {-# LINE 5763 "src/PrettyCode.hs" #-}
                             )) of
                      { _lhsOoutput | _lhsOoutput `seq` (True) ->
                      ( _lhsOoutput) }) }) }) }) }) })
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
type T_TraitAttr = ( TraitAttr,Builder)
sem_TraitAttr_Final :: T_TraitAttr
sem_TraitAttr_Final =
    (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
            TraitAttr_Final
            {-# LINE 5783 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 5788 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 5793 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_TraitAttr_Override :: T_TraitAttr
sem_TraitAttr_Override =
    (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
            TraitAttr_Override
            {-# LINE 5801 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 5806 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 5811 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
sem_TraitAttr_Metadata :: T_TraitAttr
sem_TraitAttr_Metadata =
    (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
            TraitAttr_Metadata
            {-# LINE 5819 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 5824 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 5829 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
-- TraitAttrs --------------------------------------------------
-- cata
sem_TraitAttrs :: TraitAttrs ->
                  T_TraitAttrs
sem_TraitAttrs list =
    (Prelude.foldr sem_TraitAttrs_Cons sem_TraitAttrs_Nil (Prelude.map sem_TraitAttr list))
-- semantic domain
type T_TraitAttrs = ( TraitAttrs,Builder)
sem_TraitAttrs_Cons :: T_TraitAttr ->
                       T_TraitAttrs ->
                       T_TraitAttrs
sem_TraitAttrs_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIflags,_tlIoutput) | True ->
         (case (hd_) of
          { ( _hdIflags,_hdIoutput) | True ->
              (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
                      (:) _hdIflags _tlIflags
                      {-# LINE 5851 "src/PrettyCode.hs" #-}
                      )) of
               { _flags | _flags `seq` (True) ->
               (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
                       _flags
                       {-# LINE 5856 "src/PrettyCode.hs" #-}
                       )) of
                { _lhsOflags | _lhsOflags `seq` (True) ->
                (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                        _hdIoutput ## _tlIoutput
                        {-# LINE 5861 "src/PrettyCode.hs" #-}
                        )) of
                 { _lhsOoutput | _lhsOoutput `seq` (True) ->
                 ( _lhsOflags,_lhsOoutput) }) }) }) }) })
sem_TraitAttrs_Nil :: T_TraitAttrs
sem_TraitAttrs_Nil =
    (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
            []
            {-# LINE 5869 "src/PrettyCode.hs" #-}
            )) of
     { _flags | _flags `seq` (True) ->
     (case (({-# LINE 479 "src\\PrettyCode.ag" #-}
             _flags
             {-# LINE 5874 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOflags | _lhsOflags `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 5879 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOflags,_lhsOoutput) }) }) })
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
type T_TraitData = Word8 ->
                   ( Builder)
sem_TraitData_Slot :: Word32 ->
                      Word32 ->
                      Word32 ->
                      T_ValueKind ->
                      T_TraitData
sem_TraitData_Slot slotId_ tp_ vindex_ vkind_ =
    (\ _lhsIflags ->
         (case (vkind_) of
          { ( _vkindIkind,_vkindIoutput) | True ->
              (case (({-# LINE 507 "src\\PrettyCode.ag" #-}
                      if vindex_ > 0 then u8 (fromValueKind _vkindIkind) else empty
                      {-# LINE 5915 "src/PrettyCode.hs" #-}
                      )) of
               { _kind | _kind `seq` (True) ->
               (case (({-# LINE 493 "src\\PrettyCode.ag" #-}
                       0
                       {-# LINE 5920 "src/PrettyCode.hs" #-}
                       )) of
                { _tp | _tp `seq` (True) ->
                (case (({-# LINE 506 "src\\PrettyCode.ag" #-}
                        u30 slotId_ ## u30 tp_ ## u30 vindex_ ## _kind
                        {-# LINE 5925 "src/PrettyCode.hs" #-}
                        )) of
                 { _body | _body `seq` (True) ->
                 (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                         u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                         {-# LINE 5930 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_TraitData_Const :: Word32 ->
                       Word32 ->
                       Word32 ->
                       T_ValueKind ->
                       T_TraitData
sem_TraitData_Const slotId_ tp_ vindex_ vkind_ =
    (\ _lhsIflags ->
         (case (vkind_) of
          { ( _vkindIkind,_vkindIoutput) | True ->
              (case (({-# LINE 507 "src\\PrettyCode.ag" #-}
                      if vindex_ > 0 then u8 (fromValueKind _vkindIkind) else empty
                      {-# LINE 5945 "src/PrettyCode.hs" #-}
                      )) of
               { _kind | _kind `seq` (True) ->
               (case (({-# LINE 499 "src\\PrettyCode.ag" #-}
                       6
                       {-# LINE 5950 "src/PrettyCode.hs" #-}
                       )) of
                { _tp | _tp `seq` (True) ->
                (case (({-# LINE 506 "src\\PrettyCode.ag" #-}
                        u30 slotId_ ## u30 tp_ ## u30 vindex_ ## _kind
                        {-# LINE 5955 "src/PrettyCode.hs" #-}
                        )) of
                 { _body | _body `seq` (True) ->
                 (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                         u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                         {-# LINE 5960 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOoutput) }) }) }) }) }))
sem_TraitData_Method :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Method dispId_ method_ =
    (\ _lhsIflags ->
         (case (({-# LINE 509 "src\\PrettyCode.ag" #-}
                 u30 dispId_ ## u30 method_
                 {-# LINE 5971 "src/PrettyCode.hs" #-}
                 )) of
          { _body | _body `seq` (True) ->
          (case (({-# LINE 494 "src\\PrettyCode.ag" #-}
                  1
                  {-# LINE 5976 "src/PrettyCode.hs" #-}
                  )) of
           { _tp | _tp `seq` (True) ->
           (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                   u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                   {-# LINE 5981 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOoutput) }) }) }))
sem_TraitData_Getter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Getter dispId_ method_ =
    (\ _lhsIflags ->
         (case (({-# LINE 509 "src\\PrettyCode.ag" #-}
                 u30 dispId_ ## u30 method_
                 {-# LINE 5992 "src/PrettyCode.hs" #-}
                 )) of
          { _body | _body `seq` (True) ->
          (case (({-# LINE 495 "src\\PrettyCode.ag" #-}
                  2
                  {-# LINE 5997 "src/PrettyCode.hs" #-}
                  )) of
           { _tp | _tp `seq` (True) ->
           (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                   u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                   {-# LINE 6002 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOoutput) }) }) }))
sem_TraitData_Setter :: Word32 ->
                        Word32 ->
                        T_TraitData
sem_TraitData_Setter dispId_ method_ =
    (\ _lhsIflags ->
         (case (({-# LINE 509 "src\\PrettyCode.ag" #-}
                 u30 dispId_ ## u30 method_
                 {-# LINE 6013 "src/PrettyCode.hs" #-}
                 )) of
          { _body | _body `seq` (True) ->
          (case (({-# LINE 496 "src\\PrettyCode.ag" #-}
                  3
                  {-# LINE 6018 "src/PrettyCode.hs" #-}
                  )) of
           { _tp | _tp `seq` (True) ->
           (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                   u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                   {-# LINE 6023 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOoutput) }) }) }))
sem_TraitData_Function :: Word32 ->
                          Word32 ->
                          T_TraitData
sem_TraitData_Function dispId_ method_ =
    (\ _lhsIflags ->
         (case (({-# LINE 509 "src\\PrettyCode.ag" #-}
                 u30 dispId_ ## u30 method_
                 {-# LINE 6034 "src/PrettyCode.hs" #-}
                 )) of
          { _body | _body `seq` (True) ->
          (case (({-# LINE 498 "src\\PrettyCode.ag" #-}
                  5
                  {-# LINE 6039 "src/PrettyCode.hs" #-}
                  )) of
           { _tp | _tp `seq` (True) ->
           (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                   u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                   {-# LINE 6044 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOoutput) }) }) }))
sem_TraitData_Class :: Word32 ->
                       Word32 ->
                       T_TraitData
sem_TraitData_Class slotId_ class_ =
    (\ _lhsIflags ->
         (case (({-# LINE 510 "src\\PrettyCode.ag" #-}
                 u30 slotId_ ## u30 class_
                 {-# LINE 6055 "src/PrettyCode.hs" #-}
                 )) of
          { _body | _body `seq` (True) ->
          (case (({-# LINE 497 "src\\PrettyCode.ag" #-}
                  4
                  {-# LINE 6060 "src/PrettyCode.hs" #-}
                  )) of
           { _tp | _tp `seq` (True) ->
           (case (({-# LINE 502 "src\\PrettyCode.ag" #-}
                   u8 (shiftL _lhsIflags 4 .|. _tp    ) ## _body
                   {-# LINE 6065 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOoutput | _lhsOoutput `seq` (True) ->
            ( _lhsOoutput) }) }) }))
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
type T_TraitKind = ( Builder)
sem_TraitKind_Slot :: T_TraitKind
sem_TraitKind_Slot =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6093 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Method :: T_TraitKind
sem_TraitKind_Method =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6101 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Getter :: T_TraitKind
sem_TraitKind_Getter =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6109 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Setter :: T_TraitKind
sem_TraitKind_Setter =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6117 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Class :: T_TraitKind
sem_TraitKind_Class =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6125 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Function :: T_TraitKind
sem_TraitKind_Function =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6133 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
sem_TraitKind_Const :: T_TraitKind
sem_TraitKind_Const =
    (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
            mempty
            {-# LINE 6141 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOoutput | _lhsOoutput `seq` (True) ->
     ( _lhsOoutput) })
-- TraitMeta ---------------------------------------------------
-- cata
sem_TraitMeta :: TraitMeta ->
                 T_TraitMeta
sem_TraitMeta list =
    (Prelude.foldr sem_TraitMeta_Cons sem_TraitMeta_Nil list)
-- semantic domain
type T_TraitMeta = ( Int,Builder)
sem_TraitMeta_Cons :: Word32 ->
                      T_TraitMeta ->
                      T_TraitMeta
sem_TraitMeta_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 332 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 6161 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 332 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 6166 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 332 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 6171 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (({-# LINE 470 "src\\PrettyCode.ag" #-}
                    u30 hd_ ## _tlIoutput
                    {-# LINE 6176 "src/PrettyCode.hs" #-}
                    )) of
             { _lhsOoutput | _lhsOoutput `seq` (True) ->
             ( _lhsOcount,_lhsOoutput) }) }) }) }) })
sem_TraitMeta_Nil :: T_TraitMeta
sem_TraitMeta_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 6184 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 6189 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
-- Traits ------------------------------------------------------
-- cata
sem_Traits :: Traits ->
              T_Traits
sem_Traits list =
    (Prelude.foldr sem_Traits_Cons sem_Traits_Nil (Prelude.map sem_Trait list))
-- semantic domain
type T_Traits = ( Int,Builder)
sem_Traits_Cons :: T_Trait ->
                   T_Traits ->
                   T_Traits
sem_Traits_Cons hd_ tl_ =
    (case (tl_) of
     { ( _tlIcount,_tlIoutput) | True ->
         (case (({-# LINE 330 "src\\PrettyCode.ag" #-}
                 _tlIcount
                 {-# LINE 6209 "src/PrettyCode.hs" #-}
                 )) of
          { _count_augmented_syn | _count_augmented_syn `seq` (True) ->
          (case (({-# LINE 330 "src\\PrettyCode.ag" #-}
                  (+1)
                  {-# LINE 6214 "src/PrettyCode.hs" #-}
                  )) of
           { _count_augmented_f1 | _count_augmented_f1 `seq` (True) ->
           (case (({-# LINE 330 "src\\PrettyCode.ag" #-}
                   foldr ($) _count_augmented_syn [_count_augmented_f1]
                   {-# LINE 6219 "src/PrettyCode.hs" #-}
                   )) of
            { _lhsOcount | _lhsOcount `seq` (True) ->
            (case (hd_) of
             { ( _hdIoutput) | True ->
                 (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
                         _hdIoutput ## _tlIoutput
                         {-# LINE 6226 "src/PrettyCode.hs" #-}
                         )) of
                  { _lhsOoutput | _lhsOoutput `seq` (True) ->
                  ( _lhsOcount,_lhsOoutput) }) }) }) }) }) })
sem_Traits_Nil :: T_Traits
sem_Traits_Nil =
    (case (({-# LINE 310 "src\\PrettyCode.ag" #-}
            0
            {-# LINE 6234 "src/PrettyCode.hs" #-}
            )) of
     { _lhsOcount | _lhsOcount `seq` (True) ->
     (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
             mempty
             {-# LINE 6239 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOoutput | _lhsOoutput `seq` (True) ->
      ( _lhsOcount,_lhsOoutput) }) })
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
type T_ValueKind = ( ValueKind,Builder)
sem_ValueKind_Int :: T_ValueKind
sem_ValueKind_Int =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Int
            {-# LINE 6283 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6288 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6293 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_UInt :: T_ValueKind
sem_ValueKind_UInt =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_UInt
            {-# LINE 6301 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6306 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6311 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Double :: T_ValueKind
sem_ValueKind_Double =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Double
            {-# LINE 6319 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6324 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6329 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Utf8 :: T_ValueKind
sem_ValueKind_Utf8 =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Utf8
            {-# LINE 6337 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6342 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6347 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_True :: T_ValueKind
sem_ValueKind_True =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_True
            {-# LINE 6355 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6360 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6365 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_False :: T_ValueKind
sem_ValueKind_False =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_False
            {-# LINE 6373 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6378 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6383 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Null :: T_ValueKind
sem_ValueKind_Null =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Null
            {-# LINE 6391 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6396 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6401 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Undefined :: T_ValueKind
sem_ValueKind_Undefined =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Undefined
            {-# LINE 6409 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6414 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6419 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Namespace :: T_ValueKind
sem_ValueKind_Namespace =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Namespace
            {-# LINE 6427 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6432 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6437 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Package :: T_ValueKind
sem_ValueKind_Package =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Package
            {-# LINE 6445 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6450 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6455 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Internal :: T_ValueKind
sem_ValueKind_Internal =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Internal
            {-# LINE 6463 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6468 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6473 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Protected :: T_ValueKind
sem_ValueKind_Protected =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Protected
            {-# LINE 6481 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6486 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6491 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Explicit :: T_ValueKind
sem_ValueKind_Explicit =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Explicit
            {-# LINE 6499 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6504 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6509 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Static :: T_ValueKind
sem_ValueKind_Static =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Static
            {-# LINE 6517 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6522 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6527 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })
sem_ValueKind_Private :: T_ValueKind
sem_ValueKind_Private =
    (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
            ValueKind_Private
            {-# LINE 6535 "src/PrettyCode.hs" #-}
            )) of
     { _kind | _kind `seq` (True) ->
     (case (({-# LINE 184 "src\\PrettyCode.ag" #-}
             _kind
             {-# LINE 6540 "src/PrettyCode.hs" #-}
             )) of
      { _lhsOkind | _lhsOkind `seq` (True) ->
      (case (({-# LINE 181 "src\\PrettyCode.ag" #-}
              mempty
              {-# LINE 6545 "src/PrettyCode.hs" #-}
              )) of
       { _lhsOoutput | _lhsOoutput `seq` (True) ->
       ( _lhsOkind,_lhsOoutput) }) }) })