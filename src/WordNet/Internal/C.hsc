{-# language CPP #-}
{-# language TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
#include "wn.h"

module WordNet.Internal.C where
-- import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (forM)
import Foreign.C.Types
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (nullPtr, Ptr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array
-- import Data.Text (Text, pack, unpack)
-- import Control.Exception (Exception, throw)
-- import Data.Dynamic (Typeable)
import qualified WordNet.DB as DB
import Data.List (zipWith5)

newtype SearchOpts = SearchOpts CInt

-- foreign import ccall "findtheinfo"     findtheinfo_ :: CString -> CInt -> CInt -> CInt -> IO SynsetPtr
foreign import ccall "findtheinfo_ds"     findtheinfo_ds :: CString -> POS -> SearchOpts -> CInt -> IO SynsetPtr
foreign import ccall "free_syns"          free_syns :: SynsetPtr -> IO ()
foreign import ccall "read_synset"        read_synset :: CInt -> PtrOffset -> CString -> IO SynsetPtr

type PtrType = DB.Search
-- type PtrType = CInt

newtype CEnum a = CEnum CInt

type SynsetPtr = Ptr Synset

-- | A relative offset to a synset in a database.
newtype PtrOffset = PtrOffset CLong
  deriving newtype (Show, Storable)

data Synset = Synset
  { pos :: String
  , sWords :: [String]
--   , wcount :: Int
  , defn :: String
  , whichword :: Int
--   , nextform :: [Synset]
  , ptrcount :: Int
  , links :: [SynsetLink]
  }
  deriving (Show)

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromIntegral

toCInt :: Enum a => a -> CInt
toCInt = fromIntegral . fromEnum

peekEnumArray :: (Enum a) => Int -> Ptr CInt -> IO [a]
peekEnumArray n = (fmap . fmap) fromCInt . peekArray n

instance Storable Synset where
  sizeOf _ = #size Synset
  alignment _ = #alignment Synset
  peek ptr = do
    pos <- peekCString =<< (#peek Synset, pos) ptr
    wcount <- fromCInt <$> (#peek Synset, wcount) ptr
    sWords <- mapM peekCString =<< peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- (#peek Synset, words) ptr
    defn <- peekCString =<< (#peek Synset, defn) ptr
    whichword <- fromCInt <$> (#peek Synset, whichword) ptr
    -- nextform <- peekSynsetList =<< (#peek Synset, nextform) ptr

    -- Pointers/Links
    ptrcount <- fromCInt <$> (#peek Synset, ptrcount) ptr
    ppos <- peekEnumArray ptrcount =<< (#peek Synset, ppos) ptr
    ptroff <- peekArray ptrcount =<< (#peek Synset, ptroff) ptr
    pfrm <- peekEnumArray ptrcount =<< (#peek Synset, pfrm) ptr
    pto <- peekEnumArray ptrcount =<< (#peek Synset, pto) ptr
    ptrtyp <-  peekEnumArray ptrcount =<< (#peek Synset, ptrtyp) ptr
    let links = zipWith5 SynsetLink ptrtyp ptroff ppos pto pfrm
    return $ Synset
        { pos
        -- , wcount
        , sWords
        , defn
        , whichword
        , ptrcount
        , links
        -- , nextform
        }
  poke _ _ = error "Synset: poke not implemented"

data SynsetLink = SynsetLink
  { ltyp :: PtrType -- ^ Type of link
  , loff :: PtrOffset -- ^ Offset in DB
  , lpos :: DB.POS -- ^ Part of speech for target
  , lto  :: Int -- ^ 'to' fields
  , lfrm :: Int -- ^ 'from' fields
  }
  deriving (Show)

derivationLinks :: Synset -> [SynsetLink]
derivationLinks Synset{links, whichword} = filter (\SynsetLink{ltyp, lfrm} -> ltyp == DB.DERIVATION && lfrm == whichword) links

relatedToIO :: Synset -> IO [(DB.POS,[Synset])]
relatedToIO synset = do
  let related = derivationLinks synset
  forM related $ \(SynsetLink {lpos, loff}) -> do
    innerSynset <- readSynset lpos loff
    return (lpos,innerSynset)

readSynset :: DB.POS -> PtrOffset -> IO [Synset]
readSynset pos off = do
  DB.ensureInit
  ptr <- read_synset (toCInt pos) off =<< newCString ""
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


findTheInfo :: String -> DB.POS -> SearchOpts -> IO [Synset]
findTheInfo s p opts = do
  DB.ensureInit
  cstr <- newCString s
  ptr <- findtheinfo_ds cstr (POS $ toCInt p) opts 0 -- Always return all senses
  -- peekSynsetList ptr
  peekSynsetList ptr <* free_syns ptr


data Foo = Foo | Bar
 deriving (Show, Enum)
 deriving Storable via (EnumCInt Foo)

newtype EnumCInt a = EnumCInt a
  deriving newtype (Enum)

instance Enum a => Storable (EnumCInt a) where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek x = fromCInt <$> peek @CInt (castPtr x)
  poke ptr x = poke @CInt (castPtr ptr) (toCInt x)

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