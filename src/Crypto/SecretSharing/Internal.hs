{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.SecretSharing.Internal
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Crypto.SecretSharing.Internal where

import Control.Applicative ((<$>), pure)
import Control.Exception (mask)
import Control.Monad
import Control.Monad.CIO
import Control.Monad.IO.Class
import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Gf256
import Data.List (transpose)
import Data.Vector (Vector, (!))
import Data.Word (Word8)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.Random

import qualified Data.ByteString as ByteString
import qualified Data.Vector as Vector

-- | @encodeSecret m n secret@ encodes @secret@ into @n@ shares such that any
-- @m@ are sufficient to decode it.
--
-- The following preconditions must be met:
--
--   * @m > 1@, because if @m = 1@, each share is itself the secret.
--
--   * @m <= n@, because otherwise the secret could not be decoded.
--
-- If either of these are violatied, this function throws 'AssertionFailed' in
-- IO.
--
-- /since 2.0/
encodeSecret :: Word8 -> Word8 -> ByteString -> IO [ByteString]
encodeSecret m n secret = lower $ do
  secret_ptr :: Ptr Gf256 <-
    mutableShare secret

  -- Allocate memory for each share (length of secret + 1 byte for share number)

  shares :: Vector ByteString <-
    Vector.generateM ni
      (\i -> liftIO $ do
        mask $ \restore -> do
          -- With async exceptions masked, malloc the share and associate it
          -- with a free finalizer (non-atomically).

          ptr :: Ptr CChar <-
            mallocBytes (len + 1)

          share :: ByteString <-
            unsafePackMallocCStringLen (ptr, len + 1)

          restore $ do
            poke ptr (fromIntegral (i+1))
            pure share)

  -- Get a mutable view of the shares

  share_ptrs :: Vector (Ptr Gf256) <-
    traverse mutableShare shares

  liftIO $ do
    -- (m-1) random coefficients for each byte of the secret
    coeffs :: Vector Gf256 <-
      liftIO (Vector.replicateM ((mi - 1) * len) (Gf256 <$> randomIO))

    --               m-1
    --   [ 1^1, 1^2, 1^3, 1^4, ... ]
    -- n [ 2^1, 2^2, 2^3, 2^4, ... ]
    --   [ 3^1, 3^2, 3^3, 3^4, ... ]
    --   [ ...                     ]
    let indeterminates :: Vector (Vector Gf256)
        indeterminates =
          Vector.generate ni
            (\(fromIntegral . succ -> i) -> Vector.iterateN (mi - 1) (* i) i)

    -- Loop over each byte of the secret
    forM_ [0..len-1] $ \i -> do
      -- Slice (m-1) coefficients for byte i, which begin at index i*(m-1)
      let cs :: Vector Gf256
          cs = Vector.slice (i * (mi - 1)) (mi - 1) coeffs

      x <- peekByteOff secret_ptr i

      -- Evaluate each byte share
      forM_ [0..n-1] $ \(fromIntegral -> j) -> do
        let y = Vector.sum (Vector.zipWith (*) cs (indeterminates ! j))
        pokeByteOff (share_ptrs ! j) (i+1) (x+y)

    pure (Vector.toList shares)

  where
    len :: Int
    len = ByteString.length secret

    mi :: Int
    mi = fromIntegral m

    ni :: Int
    ni = fromIntegral n

mutableShare :: ByteString -> CIO (Ptr Gf256)
mutableShare share =
  CIO (\k -> unsafeUseAsCString share (k . castPtr))

-- | @decodeSecret shares@ decodes a secret with at least @m@ of @n@ shares.
--
-- If any share does not decode successfully, this function will throw
-- 'AssertionFailed', which can be caught in IO using
-- 'Control.Exception.evaluate'.
--
-- Note that if an insufficient number of shares are provided, or if shares from
-- different secrets are mixed together, /some/ nonsense bytes will still be
-- decoded.
--
-- /since 2.0/
decodeSecret :: [ByteString] -> ByteString
decodeSecret =
    pack
  . map (coerce . interp)
  . transpose
  . map (go . coerce . unpack)
  where
    go :: [Gf256] -> [(Gf256, Gf256)]
    go (x:xs) = map (x,) xs

interp :: [(Gf256, Gf256)] -> Gf256
interp (zip [(0::Int)..] -> vals) = sum $ do
  (i, (xi, yi)) <- vals

  let p = product [ xj / (xi - xj) | (j, (xj, _)) <- vals
                                   , i /= j
                                   ]
  [yi * p]
