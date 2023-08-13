module Handlers.Voting (VotingAPI, votingServer) where

import           ClassyPrelude
import           Handlers.Comment (ID)
import           Servant          (Capture, Description, NoContent (NoContent),
                                   PostNoContent, Server, type (:<|>) (..),
                                   type (:>))

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comment" :> Capture "id" ID :>
    (
      "upvote" :> Description "Upvote a comment" :> PostNoContent :<|>
      "downvote" :> Description "Downvote a comment" :> PostNoContent
    )

votingServer :: Server VotingAPI
votingServer commentID = postUpvote :<|> postDownvote
  where
    postUpvote = pure NoContent
    postDownvote = pure NoContent
