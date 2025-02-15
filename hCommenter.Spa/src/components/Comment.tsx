import { ViewComment } from '../api/models/view-comment';

interface CommentProps {
    comment: ViewComment;
}

export const Comment: React.FC<CommentProps> = ({ comment }) => {
    return (
        <div className="comment">
            <div className="comment-header">
                <div className="comment-header-left">
                    <span className="comment-author">{comment.authorName}</span>
                    <span className="comment-timestamp">
                        {new Date(comment.created).toLocaleString()}
                    </span>
                </div>
                <div className="comment-header-right">
                    <span className="comment-score">Score: {comment.score}</span>
                </div>
            </div>
            <div className="comment-content">{comment.message}</div>
            {comment.replies && comment.replies.info.length > 0 && (
                <div className="comment-replies">
                    {comment.replies.info.map((reply) => (
                        <Comment key={reply.id} comment={reply} />
                    ))}
                </div>
            )}
        </div>
    );
};
