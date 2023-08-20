module Handlers.Voting (VotingAPI, votingServer) where

import           ClassyPrelude
import           Control.Lens
import           Database.Interface    (CommentStorage, editComment)
import           Database.StorageTypes (downvotes, upvotes)
import qualified Effectful             as E
import           Handlers.Comment      (ID)
import           Servant               (Capture, Description,
                                        HasServer (ServerT),
                                        NoContent (NoContent), PostNoContent,
                                        type (:<|>) (..), type (:>))

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comment" :> Capture "id" ID :>
    (
      "upvote" :> Description "Upvote a comment" :> PostNoContent :<|>
      "downvote" :> Description "Downvote a comment" :> PostNoContent
    )

votingServer
  :: CommentStorage E.:> es
  => ServerT VotingAPI (E.Eff es)
votingServer cID = vote upvotes :<|> vote downvotes
  where
    vote voteBox = editComment cID (voteBox +~ 1) >> pure NoContent
