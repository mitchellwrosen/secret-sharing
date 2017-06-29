module Main where

import Crypto.SecretSharing

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.ByteString.Lazy (pack)
import Test.Hspec
import Test.Hspec.Core.Runner (Config(..), defaultConfig, hspecWith)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, choose, listOf1)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

main :: IO ()
main = hspecWith config spec
  where
    config = defaultConfig { configQuickCheckMaxSuccess = Just 3000 }

spec :: Spec
spec = do
  prop "encode-decode" $ monadicIO $ do
    secret <- pack <$> pick (listOf1 arbitrary)
    m <- pick (choose (2, 5))
    n <- pick (choose (m, 5))
    shares <- run (encodeSecret m n secret)

    -- All combinations of m+ shares successfully decode the secret.
    forM_ [m..n] $ \m' ->
      forM_ (combinations m' shares) $ \shares' ->
        assert (decodeSecret shares' == secret)

-- All ways to choose n elements
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = [[]]
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs
