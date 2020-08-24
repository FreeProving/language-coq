{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Coq.Util.InfixNames
  ( identIsVariable
  , infixToPrefix
  , toPrefix
  , toLocalPrefix
  , prefixOpToInfix
  , infixToCoq
  , identIsOp
  , identToOp
  , splitModule -- a bit out of place here. oh well.
  ) where

import           Control.Applicative
import           Control.Lens        hiding ( op )
import           Control.Monad
import           Data.Char
import           Data.Semigroup      ( Semigroup(..) )
import           Data.Text           ( Text )
import qualified Data.Text           as Text
import           Encoding            ( zDecodeString, zEncodeString )
import           GHC.Stack
import           Text.Parsec         hiding ( (<|>), many )

-- Lets keep this module self-contained (but use the same type synonyms)
type Op = Text

type Ident = Text

type ModuleIdent = Text

type AccessIdent = Text

identIsVariable_ :: Text -> Bool
identIsVariable_ = Text.uncons <&> \case
  Just (h, t) -> (isAlpha h || h == '_')
    && Text.all (\c -> isAlphaNum c || c == '_' || c == '\'') t
  Nothing     -> False

identIsVariable :: Text -> Bool
identIsVariable = all identIsVariable_ . Text.splitOn "."

-- An operator's user-facing name in Coq (a notation)
infixToPrefix :: Op -> Ident
infixToPrefix = ("_" <>) . (<> "_")

toPrefix :: Ident -> Ident
toPrefix x | identIsVariable x = x
           | otherwise = infixToCoq x

toLocalPrefix :: Ident -> Ident
toLocalPrefix x | identIsVariable x = x
                | otherwise = "l" <> infixToCoq x

prefixOpToInfix :: Ident -> Maybe Op
prefixOpToInfix px = do
  x <- Text.stripSuffix "_" =<< Text.stripPrefix "_" px
  guard . not $ identIsVariable x
  pure x

-- An operator's defined name in Coq (hidden by a notation)
infixToCoq_ :: Op -> Ident
infixToCoq_ name
  = "op_" <> Text.pack (zEncodeString $ Text.unpack name) <> "__"

-- This is code smell: Why do we return an unstructured Ident, and not
-- a QualId?
infixToCoq :: HasCallStack => Op -> Ident
infixToCoq op = case splitModule op of
  Just (m, op') -> m <> "." <> infixToCoq_ op'
  Nothing       -> infixToCoq_ op

splitModule :: Ident -> Maybe (ModuleIdent, AccessIdent)
splitModule = fmap fixup . either (const Nothing) Just . parse qualid ""
 where
  qualid                 = do
    let modFrag = Text.cons <$> upper
          <*> (Text.pack <$> many (alphaNum <|> char '_' <|> char '\''))
    modIdent <- Text.intercalate "." <$> many1 (try (modFrag <* char '.'))
    -- since we're assuming we get a valid name
    base <- Text.pack <$> some anyChar
    pure (modIdent, base)

  -- When we have a module name that ends in .Z or .N then that should be
  -- considered part of the name of the function. This is a hack to make the
  -- common case of working with names like Coq.ZArith.BinInt.Z.eqb more
  -- convenient, without solving the problem of handling non-filesystem-modules
  -- in general
  fixup (modIdent, name)
    | ".Z" `Text.isSuffixOf` modIdent
      = (Text.take (Text.length modIdent - 2) modIdent, "Z." <> name)
    | ".N" `Text.isSuffixOf` modIdent
      = (Text.take (Text.length modIdent - 2) modIdent, "N." <> name)
    | otherwise = (modIdent, name)

identIsOp :: Ident -> Bool
identIsOp t = "op_" `Text.isPrefixOf` t
  && "__" `Text.isSuffixOf` t
  -- the next clause is a work-around as long as the dict accessors are named
  -- op_...____ – these do not have notations
  && not ("____" `Text.isSuffixOf` t)
  && Text.length t > 5

identToOp :: Ident -> Maybe Op
identToOp t
  | identIsOp t = Just
    $ Text.pack (zDecodeString (Text.unpack (Text.drop 3 (Text.dropEnd 2 t))))
  | otherwise = Nothing
