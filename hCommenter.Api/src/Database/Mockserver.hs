module Database.Mockserver where

import           ClassyPrelude
import           Control.Lens           ((^.))
import           Data.Binary            (encodeFile)
import qualified Data.Map               as M
import           Database.Persist.Sql   (PersistStoreWrite (repsertMany),
                                         toSqlKey)
import           Database.SqlPool       (runSqlPool, withConn)
import           Database.StorageTypes
import           Effectful              (runEff)
import           Logging                (runLog)
import           Server.ServerTypes     (Backend, Env)
import           System.Directory.Extra (doesFileExist)

mkMockComments :: IO PureStorage
mkMockComments = do
  comment1 <- mkComment "Johnny" "convo.com" "First"
  comment2 <- mkComment "Abby" "convo.com" "Dangit, almost got first."
  comment3 <- mkComment "Abby" "chat.com" "Whoops, wrong site."

  pure $ PureStorage
    (M.fromList [
      (ConversationKey "convo.com", Conversation "convo.com" "Convo"),
      (ConversationKey "chat.com", Conversation "chat.com" "Chat")
    ])
    (M.fromList [
      (UserKey "Johnny", User "Johnny" "John" "Smith"),
      (UserKey "Abby", User "Abby" "Abigail" "Smith")
    ])
    (M.fromList [
      (toSqlKey 1, comment1),
      (toSqlKey 2, comment2),
      (toSqlKey 3, comment3)
    ])
    (toSqlKey 4)

fileName :: FilePath
fileName = "localStorage.txt"

initialiseLocalFile :: IO ()
initialiseLocalFile = do
  exists <- doesFileExist fileName
  unless exists $ do
    mockComments <- mkMockComments
    encodeFile fileName mockComments

initDevSqliteDB :: Backend -> Env -> IO ()
initDevSqliteDB backend env = do
  mockComments <- mkMockComments

  runEff $ runLog env . runSqlPool backend $ withConn $ do
    repsertMany $ M.toList $ mockComments ^. convoStore
    repsertMany $ M.toList $ mockComments ^. userStore
    repsertMany $ M.toList $ mockComments ^. commentStore
