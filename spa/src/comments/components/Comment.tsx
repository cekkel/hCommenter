import { Calendar, LinkIcon, PlayIcon, UserRoundPen } from "lucide-react";
import { ViewComment } from "../../client";
import { useMutation } from "@tanstack/react-query";
import { postCommentsByIdDownvoteMutation, postCommentsByIdUpvoteMutation } from "../../client/@tanstack/react-query.gen";
import { NewComment } from "./NewComment";
import { Button } from "react-aria-components";

interface CommentProps {
  comment: ViewComment;
  showUrl?: boolean;
}

export const Comment = ({ comment, showUrl = false }: CommentProps) => {
  const sendUpvote = useMutation({ ...postCommentsByIdUpvoteMutation() })
  const sendDownvote = useMutation({ ...postCommentsByIdDownvoteMutation() })

  const upvote = () => {
    // Handle upvote logic here
    sendUpvote.mutate({ path: { id: comment.id } });
    console.log("Upvoted comment with id:", comment.id);
  };
  const downvote = () => {
    // Handle downvote logic here
    sendDownvote.mutate({ path: { id: comment.id } });
    console.log("Downvoted comment with id:", comment.id);
  };
  return (
    <div className="bg-white shadow rounded-lg p-6 transition duration-150 ease-in-out hover:shadow-md relative">
      <div className="absolute top-4 right-4 flex items-center space-x-2 text-sm text-gray-500">
        <span className="text-lg">{comment.score}</span>
        <div className="flex flex-col">
          <Button onPress={upvote} className="cursor-pointer">
            <PlayIcon
              className="fill-blue-500 h-5 w-5 -mb-1.5 rotate-270 hover:fill-blue-800 active:stroke-2 stroke-blue-800 transition-all"
              size={24}
              strokeWidth={0}
            />
          </Button>
          <Button onPress={downvote} className="cursor-pointer">
            <PlayIcon
              className="fill-red-400 h-5 w-5 rotate-90 hover:fill-red-800 active:stroke-2 stroke-red-800 transition-all"
              size={24}
              strokeWidth={0}
            />
          </Button>
        </div>
      </div>
      <p className="text-gray-800 text-lg mb-4">{comment.message}</p>
      <div className="flex items-center justify-between text-sm text-gray-500">
        <div className="flex items-center space-x-4">
          <div className="flex items-center space-x-2">
            <UserRoundPen
              className="text-gray-400 h-5 w-5"
              size={24}
              strokeWidth={2}
            />
            <span className="font-medium text-gray-600">
              {comment.authorName}
            </span>
          </div>
          {showUrl && comment.conversationUrl && (
            <div className="flex items-center space-x-2">
              <LinkIcon
                className="text-gray-400 h-5 w-5"
                size={24}
                strokeWidth={2}
              />
              <a
                href={comment.conversationUrl}
                className="text-blue-500 hover:text-blue-600 hover:underline"
                target="_blank"
                rel="noopener noreferrer"
              >
                {comment.conversationUrl}
              </a>
            </div>
          )}
        </div>
        <div className="flex items-center space-x-2">
          <Calendar
            className="text-gray-400 h-5 w-5"
            size={24}
            strokeWidth={2}
          />
          <span>{new Date(comment.created).toLocaleDateString()}</span>
        </div>
        <NewComment convoUrl={comment.conversationUrl} author={comment.authorName} parentId={comment.id} />
      </div>
    </div>
  );
};
