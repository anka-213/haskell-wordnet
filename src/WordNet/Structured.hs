{-# LANGUAGE NamedFieldPuns #-}
module WordNet.Structured
  (module WordNet.Structured
  , Synset (..)
  , SynsetLink (..)
  , PtrOffset
  , WordNumber(..)
  , SenseNumber(..)
  , findTheInfo
  , readSynset
  ) where
import WordNet.Internal.C
import WordNet.DB
import Control.Monad (forM, forM_, zipWithM_)
import Data.List (intercalate)


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

-- | Get a list of all derivationally related words for each synset of a word
getDerivations' :: String -> IO [(Synset,[(SynsetLink,Synset)])]
getDerivations' word = do
  synsets <- findTheInfo word Verb DERIVATION
  forM synsets $ \synset -> do
    let derivLinks = filter ((==) DERIVATION . ltyp) $ links synset
    derivs <- forM derivLinks $ \lnk -> do
      [innerSynset] <- followLink lnk
      return (lnk,innerSynset)
    return (synset,derivs)

printAllSenses :: [Synset] -> IO ()
printAllSenses = zipWithM_ printRelated [1..]

printRelated :: Int -> Synset -> IO ()
printRelated senseNr synset = do
  let related = derivationLinks synset
  putStrLn $ "Sense " ++ show senseNr
  putStrLn $ showSynset synset
  forM_ related $ \lnk@SynsetLink {lpos,lto} -> do
    [innerSynset] <- followLink lnk
    putStrLn $ "    RELATED TO->(" ++ show lpos ++ ") " ++ getWord innerSynset lto ++ "#" ++ show (getSense innerSynset lto)
    putStrLn $ "        => " ++ showSynset innerSynset
  putStrLn ""

  where showSynset Synset{sWords, defn} = intercalate ", " sWords ++ " -- " ++ defn

printAllRelated :: Int -> Synset -> IO ()
printAllRelated senseNr synset = do
  -- let related = links synset
  let related = filter (\x -> ltyp x == DERIVATION) $ links synset
  putStrLn $ "Sense " ++ show senseNr
  putStrLn $ showSynset synset
  forM_ related $ \lnk@SynsetLink {ltyp, lpos,lto, lfrm} -> do
    [innerSynset] <- followLink lnk
    putStrLn $ "    RELATED " <> show ltyp <> " TO "++ getWord synset lfrm ++ "->(" ++ show lpos ++ ") " ++ getWord innerSynset lto ++ "#" ++ show (getSense innerSynset lto)
    putStrLn $ "        => " ++ showSynset innerSynset
  putStrLn ""

  where showSynset Synset{sWords, defn} = intercalate ", " sWords ++ " -- " ++ defn

getWord :: Synset -> WordNumber  -> String
getWord Synset{sWords} (WordNumber nr)
  | nr > 0 = sWords !! (nr - 1)
  | otherwise = "Unknown"
  --- | otherwise = show sWords ++ " !! " ++ show nr

-- | Get the sense number of a word in a synset
getSense :: Synset -> WordNumber  -> SenseNumber
getSense Synset{wnsns} (WordNumber index) 
  | index > 0 = wnsns !! (index - 1)
  | otherwise = SenseNumber (index - 1)
