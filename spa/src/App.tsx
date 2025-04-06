import "./App.css";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { useGetCommentsUserUsername } from "./api/generated/hCommenterAPI";

const queryClient = new QueryClient();

const App = () => {
  return (
    <QueryClientProvider client={queryClient}>
      <h1>Vite + React</h1>
      <p className="read-the-docs">
        Click on the Vite and React logos to learn more
      </p>
      <CommentsList />
    </QueryClientProvider>
  );
}

const CommentsList = () => {
  const { data: comments, isLoading, error } = useGetCommentsUserUsername("Abby");

  if (isLoading) return <div>Loading comments...</div>;
  if (error) return <div>Error loading comments</div>;

  return (
    <div>
      <h2>Comments by Abby</h2>
      {comments?.map((comment) => (
        <div key={comment.id} className="comment">
          <p>{comment.message}</p>
          <div className="comment-meta">
            <div>Score: {comment.score}</div>
            <div>Posted: {new Date(comment.created).toLocaleDateString()}</div>
          </div>
        </div>
      ))}
    </div>
  );
}

export default App;
