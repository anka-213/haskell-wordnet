{-# language CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
#include "wn.h"

module WordNet.Internal.C where
import Control.Exception (bracket)
import Foreign.C.Types
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array
-- import Data.Text (Text, pack, unpack)
-- import Control.Exception (Exception, throw)
-- import Data.Dynamic (Typeable)
import WordNet.DB as DB
import Data.List (zipWith5)

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda using `infix`" #-}

newtype SearchOpts = SearchOpts CInt

-- foreign import ccall "findtheinfo"     findtheinfo_ :: CString -> CInt -> CInt -> CInt -> IO SynsetPtr
foreign import ccall "findtheinfo_ds"     findtheinfo_ds :: CString -> CEnum DB.POS -> CEnum DB.Search -> CInt -> IO SynsetPtr
foreign import ccall "free_syns"          free_syns :: SynsetPtr -> IO ()
foreign import ccall "read_synset"        read_synset :: CEnum DB.POS -> PtrOffset -> CString -> IO SynsetPtr

type PtrType = DB.Search
-- type PtrType = CInt

-- | An enum represented as a C int
newtype CEnum a = CEnum CInt

cEnum :: (Enum a) => a -> CEnum a
cEnum = CEnum . fromIntegral . fromEnum

unCEnum :: (Enum a) => CEnum a -> a
unCEnum (CEnum i) = toEnum $ fromIntegral i

type SynsetPtr = Ptr Synset

-- | A relative offset to a synset in a database.
newtype PtrOffset = PtrOffset CLong
  deriving newtype (Show, Storable)

-- | One-indexed word number in a synset.
newtype WordNumber = WordNumber Int
  deriving newtype (Show, Eq)
  deriving (Storable) via (EnumCInt Int)

newtype SenseNumber = SenseNumber Int
  deriving newtype (Show, Eq)
  deriving (Storable) via (EnumCInt Int)

allSenses :: SenseNumber
allSenses = SenseNumber 0 -- #const ALLSENSES

data Synset = Synset
  { pos :: String
  , sWords :: [String]
  , wnsns :: [SenseNumber]
--   , wcount :: Int
  , defn :: String
  , whichword :: WordNumber
--   , nextform :: [Synset]
  , ptrcount :: Int
  , links :: [SynsetLink]
  }
  deriving (Show)

instance Storable Synset where
  sizeOf _ = #size Synset
  alignment _ = #alignment Synset
  peek ptr = do
    pos <- peekCString =<< (#peek Synset, pos) ptr
    wcount <- fromCInt <$> (#peek Synset, wcount) ptr
    sWords <- mapM peekCString =<< peekArray wcount =<< (#peek Synset, words) ptr
    wnsns <- peekArray wcount =<< (#peek Synset, wnsns) ptr
    defn <- peekCString =<< (#peek Synset, defn) ptr
    whichword <- (#peek Synset, whichword) ptr
    -- nextform <- peekSynsetList =<< (#peek Synset, nextform) ptr

    -- Pointers/Links
    ptrcount <- fromCInt <$> (#peek Synset, ptrcount) ptr
    ppos     <- peekArray ptrcount =<< (#peek Synset, ppos) ptr
    ptroff   <- peekArray ptrcount =<< (#peek Synset, ptroff) ptr
    pfrm     <- peekArray ptrcount =<< (#peek Synset, pfrm) ptr
    pto      <- peekArray ptrcount =<< (#peek Synset, pto) ptr
    ptrtyp   <- peekArray ptrcount =<< (#peek Synset, ptrtyp) ptr
    let links = zipWith5 SynsetLink ptrtyp ptroff ppos pto pfrm
    return $ Synset
        { pos
        -- , wcount
        , sWords
        , wnsns
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
  , lpos :: POS -- ^ Part of speech for target
  , lto  :: WordNumber -- ^ 'to' fields
  , lfrm :: WordNumber -- ^ 'from' fields
  }
  deriving (Show)

-- | Parse and deallocate a synset pointer
parseSynset :: IO SynsetPtr -> IO [Synset]
parseSynset mkPtr = bracket mkPtr free_syns peekSynsetList 

peekSynsetList :: Ptr Synset -> IO [Synset]
peekSynsetList ptr | ptr == nullPtr = return []
peekSynsetList ptr = do
  next <- (#peek Synset, nextss) ptr
  this <- peek ptr
  rest <- peekSynsetList next
  return $ this : rest

-- | Read a synset from a database given a part of speech and offset
readSynset :: DB.POS -> PtrOffset -> IO [Synset]
readSynset pos off = do
  DB.ensureInit
  parseSynset $ withCString "" (read_synset (cEnum pos) off)

-- | Find the synsets for a word given a part of speech and search option
findTheInfo :: String -> DB.POS -> DB.Search -> IO [Synset]
findTheInfo s p opts = do
  let sense = (#const ALLSENSES)  -- Always return all senses
  DB.ensureInit
  parseSynset $ withCString s $ \ cstr -> findtheinfo_ds cstr (cEnum p) (cEnum opts) sense
