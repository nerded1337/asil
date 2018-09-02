module ProgInfo
  ( StringPool,IntPool,UIntPool,DoublePool,NamePool,NamespacePool,NamesetsPool
  , Name(..),Qual(..),Namespace(..),Nameset(..)
  , Sigs,Sig(..),SigParams,SigParam(..)
  , ClassDescr(..), ClassDescrs, TraitDescrs, TraitDescr(..), TraitBody(..)
  , ExceptionDescrs, ExceptionDescr(..), lookupException, ExceptionRef
  , SymbolTables(..)
  , Ref(Ref), StringRef, IntRef, UIntRef, DoubleRef, NameRef, NamesetRef, NamespaceRef, MethodRef, ClassRef, refVal, refNull
  , lookupString, lookupInt, lookupUInt, lookupDouble, lookupName, lookupNameset, lookupNamespace, lookupMethod, lookupClass
  , module Env
  ) where

import Data.Word
import Env


-- Symbol tables
data SymbolTables = SymbolTables
  { tableInts    :: IntPool
  , tableUInts   :: UIntPool
  , tableDoubles :: DoublePool
  , tableStrings :: StringPool
  , tableNames   :: NamePool
  , tableSpaces  :: NamespacePool
  , tableSets    :: NamesetsPool
  , tableSigs    :: Sigs
  , tableClasses :: ClassDescrs
  }
  deriving Show

type StringPool      = StaticEnv String
type IntPool         = StaticEnv Word32
type UIntPool        = StaticEnv Word32
type DoublePool      = StaticEnv Double
type NamePool        = StaticEnv Name
type NamesetsPool    = StaticEnv Nameset
type NamespacePool   = StaticEnv Namespace
type Sigs            = StaticEnv Sig
type ClassDescrs     = StaticEnv ClassDescr
type ExceptionDescrs = StaticEnv ExceptionDescr


-- | Typed reference to indices in a table
newtype Ref a = Ref Word32
  deriving (Eq,Ord,Show)

refVal :: Ref a -> Word32
refVal (Ref v) = v

refNull :: Ref a -> Bool
refNull (Ref v) = v == 0

type StringRef    = Ref String
type IntRef       = Ref Int
type UIntRef      = Ref Word32
type DoubleRef    = Ref Double
type NameRef      = Ref Name
type NamesetRef   = Ref Nameset
type NamespaceRef = Ref Namespace
type MethodRef    = Ref Sig
type ClassRef     = Ref ClassDescr
type ExceptionRef = Ref ExceptionDescr


-- | Names of identifiers (rather complex structure)
data Name = Name { nmQual :: !Qual, nmStr :: !(Maybe StringRef) }
  deriving (Eq,Ord,Show)

-- | Qualified prefix of a name.
data Qual
  = QualLate                -- Not available statically
  | QualNs !NamespaceRef    -- Prefixed with a namespace
  | QualNss !NamesetRef     -- Prefixed with namespaces
  | QualOther               -- Unknown qualifier
  deriving (Eq,Ord,Show)

-- | A namespace
data Namespace = Namespace { nsName :: !StringRef }
  deriving (Eq,Ord,Show)

-- | A set of namespaces
data Nameset = Nameset { nsSpaces :: ![NamespaceRef] }
  deriving (Eq,Ord,Show)

-- | Method signature
data Sig = Sig { sigName   :: !(Maybe StringRef)  -- may be nameless
               , sigReturn :: !NameRef            -- a Name
               , sigParams :: SigParams }         -- list of Names
  deriving (Eq,Ord,Show)

-- | Parameters of a mehod.
type SigParams = [SigParam]

-- | Signature of a parameter of a method. A parameter might be nameless.
data SigParam = SigParam { spName :: !(Maybe StringRef), spType :: !NameRef }
  deriving (Eq,Ord,Show)

-- | Class information
data ClassDescr = ClassDescr
  { clName       :: !NameRef            -- name of class
  , clSuper      :: !(Maybe NameRef)    -- name of superclass (if any)
  , clInterfaces :: [NameRef]           -- interfaces implemented by class
  , clDynTraits  :: TraitDescrs         -- dynamic traits of a class
  , clStaTraits  :: TraitDescrs         -- static traits of a class
  }
  deriving (Eq,Ord,Show)

-- | List of traits
type TraitDescrs = [TraitDescr]

-- | Information about a trait
data TraitDescr = TraitDescr { trName :: !NameRef, trData :: !TraitBody }
  deriving (Eq,Ord,Show)

-- | Data of a trait
data TraitBody
  = TraitMethod  { trMethod :: !MethodRef }
  | TraitField   { trType :: !NameRef }
  | TraitClass   { trClass :: !ClassRef }
  deriving (Eq,Ord,Show)

-- | Exception information
data ExceptionDescr = ExceptionDescr
  { expFrom    :: !Word32
  , expTo      :: !Word32
  , expTarget  :: !Word32
  , expTp      :: !NameRef
  , expName    :: !NameRef
  } deriving (Eq,Ord,Show)


--
-- Lookup operations
--

lookupString :: StringRef -> SymbolTables -> String
lookupString (Ref ref) tbls = lookupEnv ref (tableStrings tbls)

lookupInt :: IntRef -> SymbolTables -> Word32
lookupInt (Ref ref) tbls = lookupEnv ref (tableInts tbls)

lookupUInt :: UIntRef -> SymbolTables -> Word32
lookupUInt (Ref ref) tbls = lookupEnv ref (tableUInts tbls)

lookupDouble :: DoubleRef -> SymbolTables -> Double
lookupDouble (Ref ref) tbls = lookupEnv ref (tableDoubles tbls)

lookupName :: NameRef -> SymbolTables -> Name
lookupName (Ref ref) tbls = lookupEnv ref (tableNames tbls)

lookupNameset :: NamesetRef -> SymbolTables -> Nameset
lookupNameset (Ref ref) tbls = lookupEnv ref (tableSets tbls)

lookupNamespace :: NamespaceRef -> SymbolTables -> Namespace
lookupNamespace (Ref ref) tbls = lookupEnv ref (tableSpaces tbls)

lookupMethod :: MethodRef -> SymbolTables -> Sig
lookupMethod (Ref ref) tbls = lookupEnv ref (tableSigs tbls)

lookupClass :: ClassRef -> SymbolTables -> ClassDescr
lookupClass (Ref ref) tbls = lookupEnv ref (tableClasses tbls)

lookupException :: ExceptionRef -> ExceptionDescrs -> ExceptionDescr
lookupException (Ref ref) = lookupEnv ref
