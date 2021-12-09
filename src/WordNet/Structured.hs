{-# LANGUAGE NamedFieldPuns #-}
module WordNet.Structured 
  (module WordNet.Structured
  , Synset (..)
  , SynsetLink (..)
  , PtrOffset
  , findTheInfo 
  , readSynset
  ) where
import WordNet.Internal.C
import WordNet.DB
import Control.Monad (forM)


-- | All links to derivationally related words
derivationLinks :: Synset -> [SynsetLink]
derivationLinks Synset{links, whichword} = filter (\SynsetLink{ltyp, lfrm} -> ltyp == DERIVATION && lfrm == whichword) links

relatedToIO :: Synset -> IO [(POS,[Synset])]
relatedToIO synset = do
  let related = derivationLinks synset
  forM related $ \SynsetLink {lpos, loff} -> do
    innerSynset <- readSynset lpos loff
    return (lpos,innerSynset)
