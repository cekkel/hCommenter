module Database.Mockserver where

import qualified Data.Map              as M
import           Database.Persist.Sql  (toSqlKey)
import           Database.StorageTypes

mockComments :: PureStorage
mockComments = PureStorage
  (M.fromList [
    (toSqlKey 1, mkComment (toSqlKey 1) (toSqlKey 1) "First"),
    (toSqlKey 2, mkComment (toSqlKey 2) (toSqlKey 2) "Dangit, almost got first.")
  ])
  (toSqlKey 3)
