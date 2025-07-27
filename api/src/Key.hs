module Key (getKey) where

import Crypto.JOSE.JWK (JWK)
import Servant.Auth.Server (generateKey)
import System.IO.Unsafe (unsafePerformIO)

getKey :: JWK
getKey = unsafePerformIO generateKey
{-# NOINLINE getKey #-}
