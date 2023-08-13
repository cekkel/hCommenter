module Handlers.Reply (ReplyAPI, replyServer) where

import           ClassyPrelude
import           Handlers.Comment (Comment, ID)
import           Servant

type ReplyAPI = "reply" :> Capture "id" ID :>
  (
    Description "Reply to a comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment :<|>
    Description "Get replies in a range"
      :> QueryParam "from" ID :> QueryParam "to" ID :> Get '[JSON] [Comment]
  )

replyServer :: Server ReplyAPI
replyServer commentID = postReply :<|> getReplies
  where
    getReplies from to = return []
    postReply = return
