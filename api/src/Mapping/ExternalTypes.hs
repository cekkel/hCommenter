{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Mapping.ExternalTypes
  ( Paginated (..)
  , PaginationData (..)
  , ViewComment (..)
  )
where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.OpenApi (ToSchema)
import Optics

-- | TODO: fill out pagination implementation
data PaginationData = PaginationData
  { totalReplies :: Int
  , cursor :: Int -- indicates position
  }
  deriving (Eq, Generic, Show)

data Paginated a = Paginated
  { info :: [a]
  , pagination :: PaginationData
  }
  deriving (Eq, Generic, Show)

data ViewComment = ViewComment
  { id :: Int64
  , created :: UTCTime
  , message :: Text
  , score :: Int
  , replies :: Maybe (Paginated ViewComment)
  , authorName :: Text
  , conversationUrl :: Text
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''PaginationData
deriveJSON defaultOptions ''PaginationData

instance ToSchema PaginationData

makeFieldLabelsNoPrefix ''Paginated
deriveJSON defaultOptions ''Paginated

instance (ToSchema a) => ToSchema (Paginated a)

makeFieldLabelsNoPrefix ''ViewComment
deriveJSON defaultOptions ''ViewComment

instance ToSchema ViewComment
