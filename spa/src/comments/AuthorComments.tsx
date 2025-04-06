import { useGetCommentsUserUsername } from "../api/generated/hCommenterAPI";
import { Comment } from "./Comment";

interface AuthorProps {
  username: string;
}

export const CommentsList = (props: AuthorProps) => {
  const { data: comments, isLoading, error } = useGetCommentsUserUsername(props.username);

  if (isLoading) return (
    <div className="flex justify-center items-center py-12">
      <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500"></div>
    </div>
  );

  if (error) return (
    <div className="bg-red-50 border-l-4 border-red-500 p-4 rounded">
      <div className="flex">
        <div className="flex-shrink-0">
          <svg className="h-5 w-5 text-red-500" viewBox="0 0 20 20" fill="currentColor">
            <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
          </svg>
        </div>
        <div className="ml-3">
          <p className="text-sm text-red-700">Error loading comments</p>
        </div>
      </div>
    </div>
  );

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-semibold text-gray-900 mb-6 pb-2 border-b border-gray-200">
        Comments by {props.username}
      </h2>
      {comments?.length === 0 ? <EmptyComments /> : comments?.map((comment) => (
        <Comment key={comment.id} comment={comment} showUrl={true} />
      ))}
    </div>
  );
}

const EmptyComments = () => {
  return (
    <div className="bg-gray-100 p-4 rounded-lg shadow-md">
      <p className="text-gray-600">No comments found for this user.</p>
    </div>
  );
}
