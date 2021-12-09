{-# language CPP #-}
{-# language TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#include "wn.h"

module WordNet.C where
-- import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (forM)
import Foreign.C.Types
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (nullPtr, Ptr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array
-- import Data.Text (Text, pack, unpack)
-- import Control.Exception (Exception, throw)
-- import Data.Dynamic (Typeable)
import qualified WordNet.DB as DB
import Data.List (zipWith4, zipWith5)

newtype SearchOpts = SearchOpts CInt

-- foreign import ccall "findtheinfo"     findtheinfo_ :: CString -> CInt -> CInt -> CInt -> IO SynsetPtr
foreign import ccall "findtheinfo_ds"     findtheinfo_ds :: CString -> POS -> SearchOpts -> CInt -> IO SynsetPtr
foreign import ccall "free_syns"          free_syns :: SynsetPtr -> IO ()
foreign import ccall "read_synset"        read_synset :: CInt -> CLong -> CString -> IO SynsetPtr

type PtrType = DB.Search
-- type PtrType = CInt

type SynsetPtr = Ptr Synset

-- | A relative offset to a synset in a database.
newtype PtrOffset = PtrOffset CLong
  deriving newtype (Show, Storable)

data Synset = Synset
  { pos :: String
  , ptrcount :: Int
  , sWords :: [String]
--   , wcount :: Int
  , defn :: String
  , whichword :: Int
--   , nextform :: [Synset]
--   Indexed by ptrcount
  , ppos :: [DB.POS] -- Pointer to int symbolizing the part of speech (as index to the array { "", "noun", "verb", "adj", "adv", NULL }; )
  , ptroff :: [PtrOffset] -- Pointer offset
  , ptrtyp :: [PtrType]
  , pfrm :: [Int] -- 'from' fields
  , links :: [SynsetLink]
  }
  deriving (Show)

fromCInt :: CInt -> Int
fromCInt = fromIntegral

peekEnumArray :: (Enum a) => Int -> Ptr CInt -> IO [a]
peekEnumArray n = (fmap . fmap) (toEnum . fromCInt) . peekArray n

instance Storable Synset where
  sizeOf _ = #size Synset
  alignment _ = #alignment Synset
  peek ptr = do
    pos <- peekCString =<< (#peek Synset, pos) ptr
    ptrcount <- fromCInt <$> (#peek Synset, ptrcount) ptr
    wcount <- fromCInt <$> (#peek Synset, wcount) ptr
    sWords <- mapM peekCString =<< peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- (#peek Synset, words) ptr
    defn <- peekCString =<< (#peek Synset, defn) ptr
    whichword <- fromCInt <$> (#peek Synset, whichword) ptr
    -- nextform <- peekSynsetList =<< (#peek Synset, nextform) ptr
    ppos <- (fmap . fmap) (toEnum . fromCInt) . peekArray ptrcount =<< (#peek Synset, ppos) ptr
    ptroff <- peekArray ptrcount =<< (#peek Synset, ptroff) ptr
    pfrm <- (fmap . fmap) fromCInt . peekArray ptrcount =<< (#peek Synset, pfrm) ptr
    pto <- (fmap . fmap) fromCInt . peekArray ptrcount =<< (#peek Synset, pto) ptr
    ptrtyp <- (fmap . fmap) (toEnum . fromCInt) $ peekArray ptrcount =<< (#peek Synset, ptrtyp) ptr
    let links = zipWith5 SynsetLink ppos ptroff ptrtyp pfrm pto
    return $ Synset
        { pos
        , ptrcount
        -- , wcount
        , sWords
        , defn
        , whichword
        , ppos
        , ptroff
        , ptrtyp
        , pfrm
        , links
        -- , nextform
        }
  poke _ _ = error "Synset: poke not implemented"

data SynsetLink = SynsetLink
  { lpos :: DB.POS -- ^ Part of speech for target
  , loff :: PtrOffset -- ^ Offset in DB
  , ltyp :: PtrType -- ^ Type of link
  , lfrm :: Int -- ^ 'from' fields
  , lto  :: Int -- ^ 'to' fields
  }
  deriving (Show)

derivationLinks :: Synset -> [SynsetLink]
derivationLinks Synset{links, whichword} = filter (\SynsetLink{ltyp, lfrm} -> ltyp == DB.DERIVATION && lfrm == whichword) links

relatedTo :: Synset -> [(DB.POS,SynsetDBPos)]
relatedTo synset = concat $ zipWith4 relatedPtr (ptrtyp synset) (ptroff synset) (ppos synset) (pfrm synset)
  where
    relatedPtr :: PtrType -> PtrOffset -> DB.POS -> Int -> [(DB.POS,SynsetDBPos)]
    relatedPtr ptrtyp ptroff pos frm 
      | ptrtyp == DB.DERIVATION, frm == whichword synset = [(pos, SynsetDBPos pos ptroff)]
      | otherwise = []

data SynsetDBPos = SynsetDBPos
  { dbPos :: DB.POS
  , dbOff :: PtrOffset
  }
  deriving (Show)

relatedToIO :: Synset -> IO [(DB.POS,[Synset])]
relatedToIO synset = do
  let related = relatedTo synset
  forM related $ \(pos, SynsetDBPos {dbPos, dbOff}) -> do
    innerSynset <- readSynset dbPos dbOff
    return (pos,innerSynset)

readSynset :: DB.POS -> PtrOffset -> IO [Synset]
readSynset pos (PtrOffset off) = do
  ptr <- read_synset (fromIntegral $ fromEnum pos) off =<< newCString ""
  synset <- peekSynsetList ptr
  free_syns ptr
  return synset


peekSynsetList :: Ptr Synset -> IO [Synset]
peekSynsetList ptr | ptr == nullPtr = return []
peekSynsetList ptr = do
  next <- (#peek Synset, nextss) ptr
  this <- peek ptr
  rest <- peekSynsetList next
  return $ this : rest


-- ForeignPtr ... free_syns

foo :: Ptr Synset -> Ptr CString
foo = #ptr Synset, pos

findTheInfo :: String -> POS -> SearchOpts -> IO [Synset]
findTheInfo s p opts = do
  DB.ensureInit
  cstr <- newCString s
  ptr <- findtheinfo_ds cstr p opts 0 -- Always return all senses
  peekSynsetList ptr
--   peekSynsetList ptr <* free_syns ptr
--   if ptr == nullPtr
--     then error "findTheInfo: an error occurred"
--     else peek ptr


data Foo = Foo | Bar
 deriving (Show, Enum)

instance Storable Foo where
  sizeOf _ = 1
  alignment _ = 1
  peek x = toEnum . fromIntegral <$> peek @CInt (castPtr x)
  poke ptr x = poke @CInt (castPtr ptr) (fromIntegral $ fromEnum x)

#{enum SearchOpts, SearchOpts
  , derivation             = DERIVATION
  }

newtype POS = POS CInt
  deriving (Show)

#{enum POS, POS
 ,any = ALL_POS
 ,noun = NOUN
 ,verb = VERB
 ,adj = ADJ
 ,adv = ADV
}


-- {#enum Abc_VerbLevel {underscoreToCase} deriving (Show, Eq) #}