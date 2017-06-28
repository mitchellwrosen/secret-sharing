{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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

module Crypto.SecretSharing.Internal
  ( encodeSecret
  , decodeSecret
  ) where

import Control.Applicative ((<$>), pure, some)
import Control.Exception (AssertionFailed(..), throw, throwIO)
import Data.Binary (Get)
import Data.Binary.Get (getWord16be, runGetOrFail)
import Data.ByteString.Lazy.Builder (toLazyByteString, word16BE)
import Data.ByteString.Lazy (ByteString, pack, unpack)
import Data.Foldable (foldMap)
import Data.List (transpose)
import Data.Word (Word8)
import GHC.Exts (build)
import Math.Polynomial.Interpolation
import System.Random.Dice (getDiceRolls)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.FiniteField.PrimeField as PrimeField

-- | @encodeSecret m n secret@ encodes @secret@ into @n@ shares such that any
-- @m@ are sufficient to decode it.
--
-- The following preconditions must be met:
--
--   * @m > 1@, because if @m = 1@, each share is itself the secret.
--
--   * @m <= n@, because otherwise the secret could not be decoded.
--
--   * @m < 1021@, as a technical limitation of the underlying implementation.
--
-- If any of these are violatied, this function throws 'AssertionFailed' in IO.
--
-- /since 1.1.0/
encodeSecret :: Int -> Int -> ByteString -> IO [ByteString]
encodeSecret m n _ | n >= 1021 || m == 1 || m > n =
  throwIO (AssertionFailed "encodeSecret: require m < 1021, m > 1, and m <= n")
encodeSecret _ _ secret | ByteString.null secret = pure []
encodeSecret m n secret = do
  coeffs <-
    chunksOf (m-1) . map fromIntegral <$>
      getDiceRolls 1021 (fromIntegral (ByteString.length secret) * (m-1))

  let shares :: [[Field]]
      shares = zipWith (encodeByte n) coeffs (unpack secret)

      shares' :: [(Field, [Field])]
      shares' = zip shareIds (transpose shares)

  pure (map encodeShare shares')

encodeShare :: (Field, [Field]) -> ByteString
encodeShare (x, xs) = toLazyByteString (foldMap (word16BE . fromField) (x:xs))

encodeByte :: Int -> Polyn -> Word8 -> [Field]
encodeByte n coeffs byte =
  [ fromField (evalPolynomial (fromIntegral byte : coeffs) i)
  | i <- take n shareIds
  ]

shareIds :: [Field]
shareIds = map fromIntegral [(1::Int)..]

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
-- /since 1.1.0/
decodeSecret :: [ByteString] -> ByteString
decodeSecret shares =
  case mapM decodeShare shares of
    Nothing -> throw (AssertionFailed "decodeSecret: decoding share failed")
    Just shares' -> go shares'
 where
  go :: [(Field, [Field])] -> ByteString
  go = pack
     . map (fromField . decodeByte)
     . transpose
     . map (\(x, xs) -> map (x,) xs)

decodeShare :: ByteString -> Maybe (Field, [Field])
decodeShare bytes =
  case runGetOrFail get bytes of
    Right (leftover, _, share)
      | ByteString.null leftover -> Just share
    _ -> Nothing
 where
  get :: Get (Field, [Field])
  get = do
    x  <- getWord16be
    xs <- some getWord16be
    pure (fromIntegral x, map fromIntegral xs)

decodeByte :: [(Field, Field)] -> Field
decodeByte ss = polyInterp ss 0

-- Copied from Data.List.Split (not worth the dependency for this one function)
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n


type Field = $(PrimeField.primeField 1021)

fromField :: Num a => Field -> a
fromField = fromInteger . PrimeField.toInteger

-- A polynomial over the finite field given as a list of coefficients.
type Polyn = [Field]

-- Evaluates the polynomial at a given point.
evalPolynomial :: Polyn -> Field -> Field
evalPolynomial coeffs x = foldr (\c res -> c + (x * res)) 0 coeffs
