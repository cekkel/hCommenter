import { useQuery } from "@tanstack/react-query";
import { getCommentsUserByUsernameOptions } from "../client/@tanstack/react-query.gen";
import { CommentsList } from "./components/CommentsList";

interface AuthorProps {
  username: string;
}

export const AuthorComments = ({ username }: AuthorProps) => {
  const {
    data: comments,
    isLoading,
    error,
  } = useQuery({ ...getCommentsUserByUsernameOptions({ path: { username } }) });

  return (
    <CommentsList
      title={`Comments by ${username}`}
      comments={comments}
      isLoading={isLoading}
      error={error}
      showUrl={true}
      emptyMessage="No comments found for this user."
    />
  );
};
