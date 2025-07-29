module Utils.Key (getKey) where

import Crypto.JOSE.JWK (JWK)
import Servant.Auth.Server (generateKey)

getKey :: IO JWK
getKey = generateKey
{-# NOINLINE getKey #-}
