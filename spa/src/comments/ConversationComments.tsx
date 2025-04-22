import { useQuery } from "@tanstack/react-query";
import { getCommentsConversationByConvoUrlOptions } from "../client/@tanstack/react-query.gen";
import { CommentsList } from "./components/CommentsList";

interface ConversationProps {
  convoUrl: string;
}

export const ConversationComments = ({ convoUrl }: ConversationProps) => {
  const {
    data: comments,
    isLoading,
    error,
  } = useQuery({
    ...getCommentsConversationByConvoUrlOptions({
      path: { convoUrl },
      query: { sortby: "Popular" }
    })
  });

  return (
    <CommentsList
      title={`Comments for ${convoUrl}`}
      comments={comments}
      isLoading={isLoading}
      error={error}
      showUrl={false}
      emptyMessage="No comments found for this conversation."
    />
  );
};
