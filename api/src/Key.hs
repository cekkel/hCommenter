module Key where

import Crypto.JOSE.JWK (JWK)
import Servant.Auth.Server (generateKey)

-- In a real application, you would not want to do this.
-- You would want to load this from a file or from an environment variable.
-- But for now, this is fine.
getKey :: IO JWK
getKey = generateKey
