-- | Exposes one of the pretty print libraries and some additional utility operations
module PrettyUtil
  ( module Text.PrettyPrint.HughesPJ
  , vert
  , renderBytes, embedAsString
  ) where

import Text.PrettyPrint.HughesPJ
import Codec.Binary.UTF8.String
import qualified Data.ByteString.Lazy as B

vert :: [Doc] -> Doc
vert = foldr ($+$) empty

renderBytes :: Doc -> B.ByteString
renderBytes = B.pack . encode . show

embedAsString :: Doc -> Doc
embedAsString = text . show . render
