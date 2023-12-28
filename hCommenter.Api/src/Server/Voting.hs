module Server.Voting (VotingAPI, votingServer) where

import           ClassyPrelude
import           Control.Lens          ((+~))
import           Data.Aeson            (object, (.=))
import           Database.Interface    (CommentStorage, editComment)
import           Database.StorageTypes (downvotes, upvotes)
import qualified Effectful             as E
import           Logging               (Log, addLogContext, addLogNamespace,
                                        logInfo)
import           Servant               (Capture, Description,
                                        HasServer (ServerT),
                                        NoContent (NoContent), PostNoContent,
                                        type (:<|>) (..), type (:>))
import           Server.Comment        (ID)

-- | Easy to abuse, needs authentication added later
type VotingAPI =
  "comment" :> Capture "id" ID :>
    (
      "upvote" :> Description "Upvote a comment" :> PostNoContent :<|>
      "downvote" :> Description "Downvote a comment" :> PostNoContent
    )

votingServer
  :: ( CommentStorage E.:> es
     , Log E.:> es
     )
  => ServerT VotingAPI (E.Eff es)
votingServer cID = vote "UpvoteComment" upvotes :<|> vote "DownvoteComment" downvotes
  where
    vote namespace voteBox = addLogNamespace namespace . addLogContext (object ["CommentID" .= cID]) $ do
      logInfo "Incrementing vote box"
      _ <- editComment cID (voteBox +~ 1)
      logInfo "Vote box incremented successfully"
      pure NoContent
