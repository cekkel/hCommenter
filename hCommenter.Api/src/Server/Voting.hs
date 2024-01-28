module Server.Voting (VotingAPI, votingServer) where

import           ClassyPrelude
import           Control.Lens          ((+~))
import           Data.Aeson            (object, (.=))
import           Database.Interface    (CommentStorage, editComment)
import           Database.StorageTypes (CommentId, commentDownvotes,
                                        commentUpvotes)
import qualified Effectful             as E
import           Logging               (Log, addLogContext, addLogNamespace,
                                        logInfo)
import           Servant               (Capture, Description,
                                        HasServer (ServerT),
                                        NoContent (NoContent), PostNoContent,
                                        type (:<|>) (..), type (:>))

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comments" :> Capture "id" CommentId :>
    (
      "upvote" :> Description "Upvote a comment" :> PostNoContent :<|>
      "downvote" :> Description "Downvote a comment" :> PostNoContent
    )

votingServer
  :: ( CommentStorage E.:> es
     , Log E.:> es
     )
  => ServerT VotingAPI (E.Eff es)
votingServer cID = vote "UpvoteComment" commentUpvotes :<|> vote "DownvoteComment" commentDownvotes
  where
    vote namespace voteBox = addLogNamespace namespace . addLogContext (object ["CommentID" .= cID]) $ do
      logInfo "Incrementing vote box"
      _ <- editComment cID (voteBox +~ 1)
      logInfo "Vote box incremented successfully"
      pure NoContent
