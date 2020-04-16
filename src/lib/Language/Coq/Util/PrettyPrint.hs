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
  )
where

import           Text.PrettyPrint.Leijen.Text
                                         hiding ( (<$>)
                                                , text
                                                , string
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
                                                )
import qualified Text.PrettyPrint.Leijen.Text  as Leijen
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LazyText
import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Foldable

(<!>) :: Doc -> Doc -> Doc
(<!>) = (Leijen.<$>)
infixr 5 <!>
{-# INLINABLE (<!>) #-}

text :: Text -> Doc
text = Leijen.text . LazyText.fromStrict
{-# INLINABLE text #-}

lazyText :: LazyText.Text -> Doc
lazyText = Leijen.text
{-# INLINABLE lazyText #-}

string :: Text -> Doc
string = Leijen.string . LazyText.fromStrict
{-# INLINABLE string #-}

lazyString :: LazyText.Text -> Doc
lazyString = Leijen.string
{-# INLINABLE lazyString #-}

encloseSep :: Foldable f => Doc -> Doc -> Doc -> f Doc -> Doc
encloseSep left right seperator =
  Leijen.encloseSep left right seperator . toList
{-# INLINABLE encloseSep #-}

list :: Foldable f => f Doc -> Doc
list = Leijen.list . toList
{-# INLINABLE list #-}

tupled :: Foldable f => f Doc -> Doc
tupled = Leijen.tupled . toList
{-# INLINABLE tupled #-}

semiBraces :: Foldable f => f Doc -> Doc
semiBraces = Leijen.semiBraces . toList
{-# INLINABLE semiBraces #-}

hsep :: Foldable f => f Doc -> Doc
hsep = Leijen.hsep . toList
{-# INLINABLE hsep #-}

vsep :: Foldable f => f Doc -> Doc
vsep = Leijen.vsep . toList
{-# INLINABLE vsep #-}

fillSep :: Foldable f => f Doc -> Doc
fillSep = Leijen.fillSep . toList
{-# INLINABLE fillSep #-}

sep :: Foldable f => f Doc -> Doc
sep = Leijen.sep . toList
{-# INLINABLE sep #-}

hcat :: Foldable f => f Doc -> Doc
hcat = Leijen.hcat . toList
{-# INLINABLE hcat #-}

vcat :: Foldable f => f Doc -> Doc
vcat = Leijen.vcat . toList
{-# INLINABLE vcat #-}

fillCat :: Foldable f => f Doc -> Doc
fillCat = Leijen.fillCat . toList
{-# INLINABLE fillCat #-}

cat :: Foldable f => f Doc -> Doc
cat = Leijen.cat . toList
{-# INLINABLE cat #-}

punctuate :: Foldable f => Doc -> f Doc -> [Doc]
punctuate p = Leijen.punctuate p . toList
{-# INLINABLE punctuate #-}

sepWith
  :: Foldable f
  => (Doc -> Doc -> Doc)
  -> (Doc -> Doc -> Doc)
  -> Doc
  -> f Doc
  -> Doc
sepWith concatLeft concatRight separator docs
  | null docs = mempty
  | otherwise = foldr1
    (\doc result -> doc `concatLeft` separator `concatRight` result)
    docs
{-# INLINABLE sepWith #-}

spacedSepPre :: Foldable f => Doc -> f Doc -> Doc
spacedSepPre = sepWith (</>) (<+>)
{-# INLINABLE spacedSepPre #-}

spacedSepPost :: Foldable f => Doc -> f Doc -> Doc
spacedSepPost = sepWith (<+>) (</>)
{-# INLINABLE spacedSepPost #-}

(</?>) :: Doc -> Maybe Doc -> Doc
d1 </?> Nothing = d1
d1 </?> Just d2 = d1 </> d2
infixl 5 </?>

docIf :: Foldable f => Doc -> f a -> Doc
docIf d x | null x    = empty
          | otherwise = d
{-# INLINABLE docIf #-}

spaceIf :: Foldable f => f a -> Doc
spaceIf = docIf space
{-# INLINABLE spaceIf #-}

lineIf :: Foldable f => f a -> Doc
lineIf = docIf line
{-# INLINABLE lineIf #-}

softlineIf :: Foldable f => f a -> Doc
softlineIf = docIf softline
{-# INLINABLE softlineIf #-}

renderOneLineT :: Doc -> Text
renderOneLineT = LazyText.toStrict . displayT . renderOneLine
{-# INLINABLE renderOneLineT #-}

-- Puts everything on a line as long as it fits, and the
-- rest on individual lines
fill1Sep :: Foldable f => f Doc -> Doc
fill1Sep xs = go (reverse (toList xs))
 where
  go []        = empty
  go (x : xs') = group (go xs' <!> x)

commaList :: Foldable f => f Doc -> Doc
commaList xs = group (align . nest (-2) $ sepWith (<$$>) (<+>) Leijen.comma xs)
