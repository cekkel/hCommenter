{-# LANGUAGE QuasiQuotes #-}

module EffectInjection (runSharedEffects, SharedEffectsPlus) where

import Effectful (Eff, IOE)
import Effectful.Error.Static (CallStack, Error, runError)
import Optics
import PyF (fmt)
import Prelude hiding (Handler)

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
import RestAPI.ServerTypes (InputError (..))
import Utils.Error (CustomError (InputError, StorageError))
import Utils.RequestContext (RequestContext)

type SharedEffectsPlus es =
  CommentStorage
    : SqlPool
    : Error InputError
    : Error StorageError
    : Error CustomError
    : Log
    : RS.Reader RequestContext
    : es

runSharedEffects
  :: (IOE E.:> es)
  => RequestContext
  -> Eff (SharedEffectsPlus es) a
  -> Eff es (Either (CallStack, CustomError) a)
runSharedEffects ctx =
  RS.runReader ctx
    . runLog (ctx ^. #env)
    . addGlobalLogContext
    . runError @CustomError
    . liftError StorageError
    . liftError InputError
    . runSqlPool
    . runCommentStorageSQL

liftError
  :: (ES.Error CustomError E.:> es, Log E.:> es, Show e)
  => (e -> CustomError)
  -> Eff (ES.Error e : es) a
  -> Eff es a
liftError f eff = do
  result <- runError eff
  case result of
    Right val -> pure val
    Left (stack, err) -> do
      let
        liftedErr = f err
      addLogContext [AppError (stack, liftedErr)] $ logError [fmt|ERROR ENCOUNTERED: {tshow err}|]
      ES.throwError liftedErr

addGlobalLogContext
  :: ( Log E.:> es
     , RS.Reader RequestContext E.:> es
     )
  => Eff es a
  -> Eff es a
addGlobalLogContext eff = do
  corrId <- RS.asks $ view #correlationId
  addLogContext [CorrelationID corrId] eff
