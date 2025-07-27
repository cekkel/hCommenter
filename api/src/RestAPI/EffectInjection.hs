{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module RestAPI.EffectInjection (effToHandler) where

import Control.Monad.Trans.Except (except)
import Effectful (Eff, IOE, runEff)
import Servant
  ( Handler (Handler)
  , ServerError (errBody, errHTTPCode, errHeaders)
  , err400
  , err404
  , err500
  )

import Data.Aeson qualified as JSON

import Database.Schema
import EffectInjection (SharedEffectsPlus, runSharedEffects)
import RestAPI.ServerTypes (ErrorResponse (ErrorResponse), InputError (..))
import Utils.Error (CustomError (..))
import Utils.RequestContext (RequestContext)

effToHandler
  :: RequestContext
  -> Eff (SharedEffectsPlus '[IOE]) a
  -> Handler a
effToHandler ctx m = do
  result <- liftIO $ runEff $ runSharedEffects ctx m
  Handler $ except $ handleServerResponse result

handleServerResponse :: Either (CallStack, CustomError) a -> Either ServerError a
handleServerResponse (Right val) = Right val
handleServerResponse (Left (_, err)) = case err of
  StorageError innerErr -> Left $ case innerErr of
    CommentNotFound _ -> servantErrorWithText err404 "Comment not found"
    UserOrConvoNotFound _ -> servantErrorWithText err404 "Unable to find user or conversation"
    UnhandledStorageError _ -> servantErrorWithText err500 "An unhandled storage exception occurred. Sorry!"
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> [fmt|Bad argument: {txt}|]
    AuthError txt -> [fmt|Authentication error: {txt}|]
 where
  servantErrorWithText sErr msg =
    sErr
      { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr)
      , errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
      }
