{-# LANGUAGE UndecidableInstances #-}

module RestAPI.Endpoints.Auth (AuthAPI, authServer) where

import Control.Monad.Logger (logError)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Text.Display (display)
import Effectful (Eff, IOE, (:>))
import Optics ((^.))
import Servant
import Servant.Auth.Server (Auth, Cookie, JWT)

import Auth (NewUser (..), User (..))
import Database.Authors.Interface
import Utils.RequestContext (RequestContext)

type AuthTypes = '[JWT, Cookie]
type ProtectedRoutes = Auth AuthTypes User

type AuthAPI =
  "auth"
    :> ( "register" :> ReqBody '[JSON] NewUser :> PostCreated '[JSON] NoContent
          :<|> "login"
            :> ReqBody '[JSON] NewUser
            :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
          :<|> "me" :> ProtectedRoutes :> Get '[JSON] User
       )

authServer :: (AuthorsRepo :> es, IOE :> es) => ServerT AuthAPI (Eff es)
authServer = register :<|> login :<|> me
 where
  register :: (AuthorsRepo :> es, IOE :> es) => NewUser -> Eff es NoContent
  register newUser = do
    hashedPassword <-
      hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ newUser ^. #password)
    res <-
      insertAuthor
        (newUser ^. #username)
        hashedPassword
    case res of
      Right _ -> pure NoContent
      Left e -> do
        $logError (display e)
        throwError err500{errBody = "Failed to register user."}

  login :: (AuthorsRepo :> es) => NewUser -> Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  login _ = error "login should be handled by servant-auth"

  me :: User -> Eff es User
  me = pure
