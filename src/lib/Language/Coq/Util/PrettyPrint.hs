module Language.Coq.Util.PrettyPrint
  ( -- * The base module
    module Text.PrettyPrint.Leijen.Text
    -- * Don't use operators with existing meanings
  , (<>)
  , (<!>)
    -- * Lazy 'LazyText.Text' to strict 'Text'
  , text
  , lazyText
  , string
  , lazyString
    -- * '[]' -> 'Foldable'
  , encloseSep
  , list
  , tupled
  , semiBraces
  , hsep
  , vsep
  , fillSep
  , sep
  , hcat
  , vcat
  , fillCat
  , cat
  , punctuate
  , fill1Sep
  , commaList
    -- * Utility functions
    -- ** Nicely smushing lists together
  , sepWith
  , spacedSepPre
  , spacedSepPost
    -- ** Dealing with possibly-empty documents
  , (</?>)
  , docIf
  , spaceIf
  , lineIf
  , softlineIf
    -- * Rendering
  , renderOneLineT
  ) where

import           Data.Foldable
import           Data.Semigroup ( Semigroup(..) )
import           Data.Text ( Text )
import qualified Data.Text.Lazy as LazyText
import           Text.PrettyPrint.Leijen.Text
  hiding ( (<$>), cat, encloseSep, fillCat, fillSep, hcat, hsep, list, punctuate
         , semiBraces, sep, string, text, tupled, vcat, vsep )
import qualified Text.PrettyPrint.Leijen.Text as Leijen

{-# INLINABLE (<!>) #-}
(<!>) :: Doc -> Doc -> Doc
(<!>) = (Leijen.<$>)

infixr 5 <!>

{-# INLINABLE text #-}
text :: Text -> Doc
text = Leijen.text . LazyText.fromStrict

{-# INLINABLE lazyText #-}
lazyText :: LazyText.Text -> Doc
lazyText = Leijen.text

{-# INLINABLE string #-}
string :: Text -> Doc
string = Leijen.string . LazyText.fromStrict

{-# INLINABLE lazyString #-}
lazyString :: LazyText.Text -> Doc
lazyString = Leijen.string

{-# INLINABLE encloseSep #-}
encloseSep :: Foldable f => Doc -> Doc -> Doc -> f Doc -> Doc
encloseSep left right seperator
  = Leijen.encloseSep left right seperator . toList

{-# INLINABLE list #-}
list :: Foldable f => f Doc -> Doc
list = Leijen.list . toList

{-# INLINABLE tupled #-}
tupled :: Foldable f => f Doc -> Doc
tupled = Leijen.tupled . toList

{-# INLINABLE semiBraces #-}
semiBraces :: Foldable f => f Doc -> Doc
semiBraces = Leijen.semiBraces . toList

{-# INLINABLE hsep #-}
hsep :: Foldable f => f Doc -> Doc
hsep = Leijen.hsep . toList

{-# INLINABLE vsep #-}
vsep :: Foldable f => f Doc -> Doc
vsep = Leijen.vsep . toList

{-# INLINABLE fillSep #-}
fillSep :: Foldable f => f Doc -> Doc
fillSep = Leijen.fillSep . toList

{-# INLINABLE sep #-}
sep :: Foldable f => f Doc -> Doc
sep = Leijen.sep . toList

{-# INLINABLE hcat #-}
hcat :: Foldable f => f Doc -> Doc
hcat = Leijen.hcat . toList

{-# INLINABLE vcat #-}
vcat :: Foldable f => f Doc -> Doc
vcat = Leijen.vcat . toList

{-# INLINABLE fillCat #-}
fillCat :: Foldable f => f Doc -> Doc
fillCat = Leijen.fillCat . toList

{-# INLINABLE cat #-}
cat :: Foldable f => f Doc -> Doc
cat = Leijen.cat . toList

{-# INLINABLE punctuate #-}
punctuate :: Foldable f => Doc -> f Doc -> [Doc]
punctuate p = Leijen.punctuate p . toList

{-# INLINABLE sepWith #-}
sepWith :: Foldable f => (Doc -> Doc -> Doc) -> (Doc -> Doc -> Doc)
        -> Doc -> f Doc -> Doc
sepWith concatLeft concatRight separator docs
  | null docs = mempty
  | otherwise = foldr1
    (\doc result -> doc `concatLeft` separator `concatRight` result) docs

{-# INLINABLE spacedSepPre #-}
spacedSepPre :: Foldable f => Doc -> f Doc -> Doc
spacedSepPre = sepWith (</>) (<+>)

{-# INLINABLE spacedSepPost #-}
spacedSepPost :: Foldable f => Doc -> f Doc -> Doc
spacedSepPost = sepWith (<+>) (</>)

(</?>) :: Doc -> Maybe Doc -> Doc
d1 </?> Nothing = d1
d1 </?> Just d2 = d1 </> d2

infixl 5 </?>

{-# INLINABLE docIf #-}
docIf :: Foldable f => Doc -> f a -> Doc
docIf d x
  | null x = empty
  | otherwise = d

{-# INLINABLE spaceIf #-}
spaceIf :: Foldable f => f a -> Doc
spaceIf = docIf space

{-# INLINABLE lineIf #-}
lineIf :: Foldable f => f a -> Doc
lineIf = docIf line

{-# INLINABLE softlineIf #-}
softlineIf :: Foldable f => f a -> Doc
softlineIf = docIf softline

{-# INLINABLE renderOneLineT #-}
renderOneLineT :: Doc -> Text
renderOneLineT = LazyText.toStrict . displayT . renderOneLine

-- Puts everything on a line as long as it fits, and the
-- rest on individual lines
fill1Sep :: Foldable f => f Doc -> Doc
fill1Sep xs = go (reverse (toList xs))
 where
   go []        = empty
   go (x : xs') = group (go xs' <!> x)

commaList :: Foldable f => f Doc -> Doc
commaList xs = group (align . nest (-2) $ sepWith (<$$>) (<+>) Leijen.comma xs)
