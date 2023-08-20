{-# LANGUAGE TemplateHaskell #-}

module Database.StorageTypes (mkComment, Comment, replies, message, upvotes, downvotes, ID, SortBy(..), emptyComment, StorageError (..)) where

import           ClassyPrelude hiding (Handler, sortBy)
import           Control.Lens  (makeLenses)
import           Data.Aeson    (defaultOptions)
import           Data.Aeson.TH (deriveJSON, fieldLabelModifier)
import           Data.Swagger  (ToParamSchema, ToSchema)
import           Servant       (FromHttpApiData (parseQueryParam))

data StorageError = CommentNotFound

type ID = Int

data Comment = Comment
  { _message   :: Text
  , _replies   :: [ID]
  , _upvotes   :: Int
  , _downvotes :: Int
  } deriving (Read, Generic)

makeLenses ''Comment
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Comment

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
emptyComment = mkComment ""

mkComment :: Text -> Comment
mkComment msg = Comment msg [] 0 0
