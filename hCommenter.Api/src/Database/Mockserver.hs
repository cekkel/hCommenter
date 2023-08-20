module Database.Mockserver where

import qualified Data.Map              as M
import           Database.Interface    (PureStorage (..))
import           Database.StorageTypes

mockComments :: PureStorage
mockComments = PureStorage
  (M.fromList [
    (1, mkComment "First"),
    (2, mkComment "Dangit, almost got first.")
  ])
  3
