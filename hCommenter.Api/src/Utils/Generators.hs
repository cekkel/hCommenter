module Utils.Generators (genKey, genSortBy, genNewComment, genCommentKey, genNum, genText) where

import Database.Persist.Sqlite (PersistEntity (Key), SqlBackend, ToBackendKey, toSqlKey)
import Hedgehog

import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range

import Database.StorageTypes (Comment, NewComment (NewComment), SortBy (..))

--
-- instance Arbitrary (Key Comment) where
--   arbitrary = toSqlKey <$> arbitrary
--
-- instance Arbitrary Text where
--   arbitrary = pack <$> arbitrary
--
-- instance Arbitrary SortBy where
--   arbitrary = oneof $ pure <$> [Old, New, Popular, Controversial]
--
-- instance Arbitrary NewComment where
--   arbitrary = NewComment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genKey :: forall record m. (MonadGen m, ToBackendKey SqlBackend record) => m (Key record)
genKey = toSqlKey <$> Gen.integral (Range.constantFrom 1 0 1000)

genCommentKey :: (MonadGen m) => m (Key Comment)
genCommentKey = genKey @Comment

genSortBy :: (MonadGen m) => m SortBy
genSortBy = Gen.enumBounded

genNewComment :: (MonadGen m) => m NewComment
genNewComment = NewComment <$> genText <*> Gen.maybe genNum <*> genText <*> genText

genNum :: (Integral a, MonadGen m) => m a
genNum = Gen.integral (Range.constantFrom 0 0 10000)

genText :: (MonadGen m) => m Text
genText = Gen.text (Range.constantFrom 0 0 10000) Gen.unicode
