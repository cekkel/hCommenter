module Database.Mockserver where

import Data.Time (getCurrentTime)
import Database.Persist (Filter, selectList)
import Database.Persist.Sql (PersistStoreWrite (insertMany_), runMigration, toSqlKey)
import Effectful (runEff)
import Optics

import Data.Map qualified as M
import Effectful.Reader.Static qualified as ES

import Database.Schema
import Database.SqlPool (runSqlPool, withConn)
import Logging.LogEffect (runLog)
import Utils.Environment (Env)
import Utils.RequestContext (mkRequestContext)

mkMockComments :: IO PureStorage
mkMockComments = do
  comment1 <- mkMockComment "Johnny" "convo.com" "First"
  comment2 <- mkMockComment "Abby" "convo.com" "Dangit, almost got first."
  comment3 <- mkMockComment "Abby" "chat.com" "Whoops, wrong site."

  currTime <- getCurrentTime

  pure $
    PureStorage
      ( M.fromList
          [ (ConversationKey "convo.com", Conversation "convo.com" "Convo")
          , (ConversationKey "chat.com", Conversation "chat.com" "Chat")
          ]
      )
      ( M.fromList
          [ (UserKey "Johnny", User "Johnny" "John" "Smith" currTime currTime)
          , (UserKey "Abby", User "Abby" "Abigail" "Smith" currTime currTime)
          ]
      )
      ( M.fromList
          [ (toSqlKey 1, comment1)
          , (toSqlKey 2, comment2)
          , (toSqlKey 3, comment3)
          ]
      )
      (toSqlKey 4)

initDevSqliteDB :: Env -> IO ()
initDevSqliteDB env = do
  mockComments <- mkMockComments
  let
    ctx = mkRequestContext env "DevInitialisation"

  runEff $ runLog env . ES.runReader ctx . runSqlPool $ withConn $ do
    runMigration migrateAll

    comments <- selectList ([] :: [Filter Comment]) []

    when (null comments) $ do
      insertMany_ $ map snd $ M.toList $ mockComments ^. #convoStore
      insertMany_ $ map snd $ M.toList $ mockComments ^. #userStore
      insertMany_ $ map snd $ M.toList $ mockComments ^. #commentStore
