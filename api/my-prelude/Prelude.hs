module Prelude
  ( module Relude
  , tshow
  , readMay
  , UTCTime
  , toLower
  , toUpper
  , pack
  , unpack
  , genericSortBy
  , fmt
  )
where

import Data.Text (pack, toLower, toUpper, unpack)
import Data.Time.Clock (UTCTime)
import PyF (fmt)
import Relude hiding (sortBy)

import Relude qualified as R

tshow :: (Show a) => a -> Text
tshow = pack . show

readMay :: (Read a) => Text -> Maybe a
readMay = readMaybe <<< unpack

genericSortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
genericSortBy = R.sortBy
