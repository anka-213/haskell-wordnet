{-# language CPP #-}
{-# language TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
#include "wn.h"

module WordNet.C where
-- import Data.List.NonEmpty (NonEmpty(..))
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (nullPtr, Ptr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array
-- import Data.Text (Text, pack, unpack)
-- import Control.Exception (Exception, throw)
-- import Data.Dynamic (Typeable)
import qualified WordNet.DB as DB

newtype SearchOpts = SearchOpts CInt

-- foreign import ccall "findtheinfo"     findtheinfo_ :: CString -> CInt -> CInt -> CInt -> IO SynsetPtr
foreign import ccall "findtheinfo_ds"     findtheinfo_ds :: CString -> POS -> SearchOpts -> CInt -> IO SynsetPtr

type SynsetPtr = Ptr Synset
data Synset = Synset
  { pos :: String
  , ptrcount :: Int
  , sWords :: [String]
--   , wcount :: Int
  , defn :: String
  }
  deriving (Show)

instance Storable Synset where
  sizeOf _ = #size Synset
  alignment _ = #alignment Synset
  peek ptr = do
    pos <- peekCString =<< (#peek Synset, pos) ptr
    ptrcount <- fromIntegral @CInt @Int <$> (#peek Synset, ptrcount) ptr
    wcount <- fromIntegral @CInt @Int <$> (#peek Synset, wcount) ptr
    sWords <- mapM peekCString =<< peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- peekArray wcount =<< (#peek Synset, words) ptr
    -- sWords <- (#peek Synset, words) ptr
    defn <- peekCString =<< (#peek Synset, defn) ptr
    return $ Synset
        { pos
        , ptrcount
        -- , wcount
        , sWords
        , defn
        }
  poke _ _ = error "Synset: poke not implemented"


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