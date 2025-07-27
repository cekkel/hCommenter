{-# LANGUAGE QuasiQuotes #-}

module Database.Users.Effect (runUserStorageSQL) where

import Database.Persist.Sql (fromSqlKey)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import PyF (fmt)
import UnliftIO (catchAny)

import Database.Persist qualified as P
import Effectful.Error.Static qualified as ES

import Database.Schema
  ( EntityField (..)
  , StorageError (..)
  , Unique (UniqueEmail, UniqueUser)
  , User (..)
  , fromNewUser
  )
import Database.SqlPool (SqlPool, withConn)
import Database.Users.Interface (UserStorage (..), UserUpdate (..))
import Logging.LogEffect (Log)
import Logging.Utilities (logDebug)
import Mapping.Typeclass (MapsFrom (mapFrom))

runUserStorageSQL
  :: ( ES.Error StorageError :> es
     , IOE :> es
     , Log :> es
     , SqlPool :> es
     )
  => Eff (UserStorage : es) a
  -> Eff es a
runUserStorageSQL = do
  interpret
    ( \_ action ->
        withConn $ case action of
          GetUser uName -> do
            maybeUser <- P.getBy $ UniqueUser uName
            case maybeUser of
              Nothing -> lift . ES.throwError . UserOrConvoNotFound $ [fmt|User '{uName}' not found|]
              Just user -> pure $ mapFrom user
          InsertUser newUser -> do
            fullUser <- liftIO $ fromNewUser newUser
            catchAny (P.insert fullUser) $ \e ->
              lift . ES.throwError . UserOrConvoNotFound $
                [fmt|Failed to insert user with error: {tshow e}|]
            -- TODO: This is a bit inefficient to immediately query, but it's the simplest way to get the full entity back
            -- to map to a view type.
            maybeUser <- P.getBy $ UniqueUser (userUsername fullUser)
            case maybeUser of
              Nothing -> lift . ES.throwError . UnhandledStorageError $ [fmt|Could not find user '{userUsername fullUser}' after insert|]
              Just user -> pure $ mapFrom user
          EditUser uId edits -> do
            let
              sqlEdits =
                edits <&> \case
                  SendNewEmail newEmail -> UserEmail P.=. newEmail
                  SendNewPasswordHash newHash -> UserPasswordHash P.=. newHash

            lift $ logDebug [fmt|Performing user edits: {tshow edits}|]

            updatedUser <-
              catchAny (P.updateGet uId sqlEdits) $ \e -> lift $ do
                ES.throwError $
                  UserOrConvoNotFound [fmt|Failed to update user {fromSqlKey uId} with error: {tshow e}|]

            lift $ logDebug [fmt|Updated user to: {tshow updatedUser}|]

            pure $ mapFrom $ P.Entity uId updatedUser
          DeleteUser uId -> P.delete uId
    )
