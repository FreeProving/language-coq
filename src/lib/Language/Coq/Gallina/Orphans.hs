{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains orphan instances to use string literals to refer to
--   variables in terms, which is convenient when building expressions in the
--   Haskell code.
--
--   Orphans are frowned upon in libraries, but not quite as bad in programs.
--   We still put them in a separate module, to make it (somewhat) clear where
--   they are used.
module Language.Coq.Gallina.Orphans () where

import           Data.String
import qualified Data.Text as Text

import           Language.Coq.Gallina
import           Language.Coq.Gallina.Util

-- For internal use only (e.g. hardcoded names)
instance IsString Term where
  fromString x = Qualid (unsafeIdentToQualid (Text.pack x))

instance IsString Qualid where
  fromString x = unsafeIdentToQualid (Text.pack x)

instance IsString Binder where
  fromString x = Inferred Explicit (Ident (unsafeIdentToQualid (Text.pack x)))
