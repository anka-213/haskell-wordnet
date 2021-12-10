{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WordNet.DB where

import Data.List.NonEmpty (NonEmpty(..))
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (nullPtr, castPtr)
import Data.Text (Text, pack, unpack)
import Control.Exception (Exception, throw)
import Data.Dynamic (Typeable)
import Paths_wordnet (getDataFileName)
import Foreign.Storable (Storable(..))

foreign import ccall "wn_init_wordnet" init_wordnet :: CString -> IO CBool
foreign import ccall "morphstr"        morphstr     :: CString -> CInt -> IO CString
foreign import ccall "findtheinfo"     findtheinfo_ :: CString -> CInt -> CInt -> CInt -> IO CString

initialise :: IO Bool
initialise = do
    dict <- getDataFileName "dict"
    initialiseFrom dict

initialiseFrom :: FilePath -> IO Bool
initialiseFrom path = do
    cpath <- newCString path
    toEnum . fromEnum <$> init_wordnet cpath

ensureInit :: IO ()
ensureInit = do
    isOpen <- initialise
    if not isOpen then
        throw CannotInitialise
    else
        pure ()

-- | Part of speech
data POS = Any | Noun | Verb | Adj | Adv
 deriving stock (Show, Enum)
 deriving Storable via (EnumCInt POS)

data Search =
 UNKNOWN_SEARCH
 | ANTPTR          -- 1    /* ! */
 | HYPERPTR        -- 2    /* @ */
 | HYPOPTR         -- 3    /* ~ */
 | ENTAILPTR       -- 4    /* * */
 | SIMPTR          -- 5    /* & */

 | ISMEMBERPTR     -- 6    /* #m */
 | ISSTUFFPTR      -- 7    /* #s */
 | ISPARTPTR       -- 8    /* #p */

 | HASMEMBERPTR    -- 9    /* %m */
 | HASSTUFFPTR     -- 10    /* %s */
 | HASPARTPTR      -- 11    /* %p */

 | MERONYM         -- 12    /* % (not valid in lexicographer file) */
 | HOLONYM         -- 13    /* # (not valid in lexicographer file) */
 | CAUSETO         -- 14    /* > */
 | PPLPTR          -- 15    /* < */
 | SEEALSOPTR      -- 16    /* ^ */
 | PERTPTR         -- 17    /* \ */
 | ATTRIBUTE       -- 18    /* = */
 | VERBGROUP       -- 19    /* $ */
 | DERIVATION      -- 20    /* + */
 | CLASSIFICATION  -- 21    /* ; */
 | CLASS           -- 22    /* - */
  deriving (Show, Eq, Enum)
 deriving Storable via (EnumCInt Search)

-- | Used for deriving via to get a 'Storable' instance for any 'Enum' type.
-- Marshalled as a 'CInt'
newtype EnumCInt a = EnumCInt a
  deriving newtype (Enum)

instance Enum a => Storable (EnumCInt a) where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek x = fromCInt <$> peek @CInt (castPtr x)
  poke ptr x = poke @CInt (castPtr ptr) (toCInt x)


morph1 :: Text -> POS -> IO (NonEmpty Text)
morph1 w p = addFirst <$> morph w p
    where
        addFirst []     = w :| []
        addFirst (x:xs) = x :| xs

morph :: Text -> POS -> IO [Text]
morph w Any = concat <$> mapM (morph w) [Noun, Verb, Adj, Adv]
morph w pos = do
    s <- newCString $ unpack w
    results <- morph' s $ fromEnum pos
    mapM ((pack <$>) . peekCString) results

morph' :: CString -> Int -> IO [CString]
morph' w i = do
    _      <- ensureInit
    result <- morphstr w (fromIntegral i)
    if result == nullPtr then
        pure []
    else do
        rest <- morph' nullPtr i
        pure $ result : rest


-- | Corresponds to the 'findtheinfo' function in the C library.
findtheinfo :: Text -> POS -> Search -> Maybe Int -> IO Text
findtheinfo w pos search whichSense = do
    ensureInit
    s <- newCString $ unpack w
    result <- findtheinfo_ s (fromIntegral $ fromEnum pos) (fromIntegral $ fromEnum search) (maybe 0 fromIntegral whichSense)
    if result == nullPtr then
        pure $ pack ""
    else
        pack <$> peekCString result

-- | Corresponds to running `wn -deriv` on the command line.
getDerivations :: Text -> IO Text
getDerivations w = findtheinfo w Verb DERIVATION Nothing

data WordNetException
    = CannotInitialise
    deriving (Show, Typeable)

instance Exception WordNetException


fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromIntegral

toCInt :: Enum a => a -> CInt
toCInt = fromIntegral . fromEnum
