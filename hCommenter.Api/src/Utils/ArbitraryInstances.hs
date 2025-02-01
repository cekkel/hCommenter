module Utils.ArbitraryInstances where

import Database.Persist.Sqlite (PersistEntity (Key), toSqlKey)
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

import Database.StorageTypes (Comment, NewComment (NewComment), SortBy (..))

instance Arbitrary (Key Comment) where
  arbitrary = toSqlKey <$> arbitrary

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary SortBy where
  arbitrary = oneof $ pure <$> [Old, New, Popular, Controversial]

instance Arbitrary NewComment where
  arbitrary = NewComment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
