-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.SecretSharing
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of an (@m@, @n@)-threshold secret sharing scheme, as described
-- in \"How to share a secret.\" by Shamir, Adi. In Communications of the ACM
-- 22 (11): 612–613, 1979.
--
-- @
-- λ> [x,y,z] <- 'encodeSecret' 2 3 "Hello!"
-- λ> map 'decodeSecret' [[x,y],[y,z],[x,z],[x,y,z]]
-- ["Hello!","Hello!","Hello!","Hello!"]
-- @
-----------------------------------------------------------------------------

module Crypto.SecretSharing
  ( encodeSecret
  , decodeSecret
  ) where

import Crypto.SecretSharing.Internal
