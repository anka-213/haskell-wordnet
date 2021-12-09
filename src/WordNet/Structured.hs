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
import Control.Monad (forM, forM_, zipWithM_)


-- | All links to derivationally related words
derivationLinks :: Synset -> [SynsetLink]
derivationLinks Synset{links, whichword} = filter (\SynsetLink{ltyp, lfrm} -> ltyp == DERIVATION && lfrm == whichword) links

followLink :: SynsetLink -> IO [Synset]
followLink SynsetLink{lpos, loff} = readSynset lpos loff

relatedToIO :: Synset -> IO [(POS,[Synset])]
relatedToIO synset = do
  let related = derivationLinks synset
  forM related $ \lnk@SynsetLink {lpos} -> do
    innerSynset <- followLink lnk
    return (lpos,innerSynset)

-- | Simulate "wn -derv" in haskell
printDerivations :: String -> IO ()
printDerivations word = printAllSenses =<< findTheInfo word Verb DERIVATION

printAllSenses :: [Synset] -> IO ()
printAllSenses = zipWithM_ printRelated [1..]

printRelated :: Int -> Synset -> IO ()
printRelated senseNr synset@Synset{sWords, defn} = do
  let related = derivationLinks synset
  putStrLn $ "Sense " ++ show senseNr
  putStrLn $ "" ++ show sWords ++ " -- " ++ defn
  forM_ related $ \lnk@SynsetLink {lpos,lto} -> do
    [innerSynset] <- followLink lnk
    let Synset{sWords, wnsns, defn} = innerSynset
    putStrLn $ "    RELATED TO->(" ++ show lpos ++ ") " ++ getWord sWords lto ++ "#" ++ show (getSense wnsns lto)
    putStrLn $ "        => " ++ show sWords ++ " -- " ++ defn
  putStrLn ""

getWord :: [String] -> Int -> String
getWord wordList nr = wordList !! (nr - 1)

getSense :: [Int] -> Int -> Int
getSense senseNrs index = senseNrs !! (index - 1) 
