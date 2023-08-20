module Database.Mockserver where

import qualified Data.Map              as M
import           Database.Interface    (PureStorage (..))
import           Database.StorageTypes

mockComments :: PureStorage
mockComments = PureStorage
  (M.fromList [
    (1, mkComment 1 "First"),
    (2, mkComment 2 "Dangit, almost got first.")
  ])
  3
