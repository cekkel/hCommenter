module Utils.Generators where

import Database.Persist.Sqlite (PersistEntity (Key, keyFromValues), PersistValue (PersistText), SqlBackend, ToBackendKey, toSqlKey)
import Hedgehog

import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range

import Database.Schema (Comment, NewComment (NewComment), NewUser (NewUser), SortBy (..), User)

--
-- instance Arbitrary (Key Comment) where
--   arbitrary = toSqlKey <$> arbitrary
-- instance Arbitrary Text where arbitrary = pack <$> arbitrary
--
-- instance Arbitrary SortBy where
--   arbitrary = oneof $ pure <$> [Old, New, Popular, Controversial]
--
-- instance Arbitrary NewComment where
--   arbitrary = NewComment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genKey :: forall record m. (MonadGen m, ToBackendKey SqlBackend record) => m (Key record)
genKey = toSqlKey <$> Gen.integral (Range.constantFrom 1 0 1000)

genSortBy :: (MonadGen m) => m SortBy
genSortBy = Gen.enumBounded

genCommentKey :: (MonadGen m) => m (Key Comment)
genCommentKey = genKey @Comment

genNewComment :: (MonadGen m) => m NewComment
genNewComment = NewComment <$> genText <*> Gen.maybe genNum <*> genText <*> genText

genUserKey :: (MonadGen m) => m (Key User)
genUserKey = do
  text <- genText
  let
    key = keyFromValues [PersistText text]
  pure $ fromRight (error "Invalid key") key

genNewUser :: (MonadGen m) => m NewUser
genNewUser = NewUser <$> genText <*> genText <*> genText

genNum :: (Integral a, MonadGen m) => m a
genNum = Gen.integral (Range.constantFrom 0 0 10000)

genInt64 :: (MonadGen m) => m Int64
genInt64 = Gen.int64 (Range.constantFrom 0 0 10000)

genText :: (MonadGen m) => m Text
genText = Gen.text (Range.constantFrom 0 0 10000) Gen.unicode
