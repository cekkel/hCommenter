{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module EffectInjection (effToHandler) where

import Control.Monad.Trans.Except (except)
import Data.Either.Extra (mapLeft)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (CallStack, Error, runError)
import Optics
import PyF (fmt)
import Servant
  ( Handler (Handler)
  , ServerError (errBody, errHTTPCode, errHeaders)
  , err400
  , err404
  , err500
  )
import Prelude hiding (Handler)

import Data.Aeson qualified as JSON
import Effectful qualified as E
import Effectful.Error.Static qualified as ES
import Effectful.Reader.Static qualified as RS

import Database.Comments.Effect (runCommentStorageSQL)
import Database.Comments.Interface (CommentStorage)
import Database.Schema
import Database.SqlPool (SqlPool, runSqlPool)
import Logging.LogContext (LogField (AppError, CorrelationID))
import Logging.LogEffect (Log, runLog)
import Logging.Utilities (addLogContext, logError)
import Server.ServerTypes (CustomError (..), ErrorResponse (ErrorResponse), InputError (..))
import Utils.RequestContext (RequestContext)

effToHandler
  :: RequestContext
  -> Eff [CommentStorage, SqlPool, Error InputError, Error StorageError, Log, RS.Reader RequestContext, IOE] a
  -> Handler a
effToHandler ctx m = do
  result <-
    liftIO
      $ runEff
        . RS.runReader ctx
        . runLog (ctx ^. #env)
        . addGlobalLogContext
        . runAndLiftError StorageError
        . runAndLiftError InputError
        . fmap Right
        . runSqlPool
        . runCommentStorageSQL
      $ m
  Handler $ except $ handleServerResponse result

runAndLiftError
  :: (Log E.:> es)
  => (e -> CustomError)
  -> Eff (ES.Error e : es) (Either (CallStack, CustomError) a)
  -> Eff es (Either (CallStack, CustomError) a)
runAndLiftError f effs = do
  errs <- runError effs

  let
    combinedEithers = join $ mapLeft (fmap f) errs

  case combinedEithers of
    Left e -> do
      addLogContext [AppError e] $ logError [fmt|ERROR ENCOUNTERED: {tshow e}|]
      pure $ Left e
    _ -> pure combinedEithers

addGlobalLogContext
  :: ( Log E.:> es
     , RS.Reader RequestContext E.:> es
     )
  => Eff es a
  -> Eff es a
addGlobalLogContext eff = do
  corrId <- RS.asks $ view #correlationId
  addLogContext [CorrelationID corrId] eff

handleServerResponse :: Either (CallStack, CustomError) a -> Either ServerError a
handleServerResponse (Right val) = Right val
handleServerResponse (Left (_, err)) = case err of
  StorageError innerErr -> Left $ case innerErr of
    CommentNotFound _ -> servantErrorWithText err404 "Comment not found"
    UserOrConvoNotFound _ -> servantErrorWithText err404 "Unable to find user or conversation"
    UnhandledStorageError _ -> servantErrorWithText err500 "An unhandled storage exception occurred. Sorry!"
  InputError innerErr -> Left $ servantErrorWithText err400 $ case innerErr of
    BadArgument txt -> [fmt|Bad argument: {txt}|]
 where
  servantErrorWithText sErr msg =
    sErr
      { errBody = JSON.encode $ ErrorResponse msg (errHTTPCode sErr)
      , errHeaders = [(fromString "Content-Type", "application/json;charset=utf-8")]
      }
