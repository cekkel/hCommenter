import { useQuery } from "@tanstack/react-query";
import { getCommentsConversationByConvoUrlOptions } from "../client/@tanstack/react-query.gen";
import { CommentsList } from "./components/CommentsList";
import { NewComment } from "./components/NewComment";

interface ConversationProps {
  convoUrl: string;
}

export const ConversationComments = ({ convoUrl }: ConversationProps) => {
  const { data: comments, isLoading, error } = useQuery({
    ...getCommentsConversationByConvoUrlOptions({
      path: { convoUrl },
      query: { sortby: "Popular" },
    }),
  });

  return (
    <div>
      <div className="flex justify-between items-center mb-6 pb-2 border-b border-gray-200">
        <h2 className="text-2xl font-semibold text-gray-900">Comments</h2>
        <NewComment convoUrl={convoUrl} />
      </div>
      <CommentsList
        comments={comments}
        isLoading={isLoading}
        error={error}
        showUrl={false}
        emptyMessage="No comments found for this conversation."
      />
    </div>
  );
};
