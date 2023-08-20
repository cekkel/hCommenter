{-# LANGUAGE TemplateHaskell #-}

module Database.StorageTypes (mkComment, Comment, replies, message, commentId, upvotes, downvotes, ID, SortBy(..), emptyComment, StorageError (..)) where

import           ClassyPrelude hiding (Handler, sortBy)
import           Control.Lens
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Swagger  (ToParamSchema, ToSchema)
import           Servant       (FromHttpApiData (parseQueryParam))

data StorageError = CommentNotFound

type ID = Int

data Comment = Comment
  { _commentId :: ID
  , _message   :: Text
  , _replies   :: [ID]
  , _upvotes   :: Int
  , _downvotes :: Int
  } deriving (Read, Generic)

makeLenses ''Comment

instance ToJSON Comment
instance FromJSON Comment
instance ToSchema Comment
instance FromHttpApiData Comment where
  parseQueryParam :: Text -> Either Text Comment
  parseQueryParam = maybe (Left "Comment not formatted correctly") Right . readMay

data SortBy = Old | New | Popular | Controversial
  deriving (Eq, Read, Generic)

instance ToParamSchema SortBy
instance FromHttpApiData SortBy where
  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam = maybe (Left "Invalid sorting method") Right . readMay

emptyComment :: Comment
emptyComment = mkComment 0 ""

mkComment :: ID -> Text -> Comment
mkComment cID msg = Comment cID msg [] 0 0
