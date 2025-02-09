import { ViewComment } from '../api/models/view-comment';

interface CommentProps {
    comment: ViewComment;
}

export const Comment: React.FC<CommentProps> = ({ comment }) => {
    const formatDate = (timestamp: string | undefined) => {
        if (!timestamp) return '';
        return new Date(timestamp).toLocaleString();
    };

    return (
        <div className="comment">
            <div className="comment-header">
                <span className="comment-author">{comment.author}</span>
                <span className="comment-timestamp">
                    {formatDate(comment.timestamp)}
                </span>
            </div>
            <div className="comment-content">{comment.content}</div>
            {comment.replies && comment.replies.length > 0 && (
                <div className="comment-replies">
                    {comment.replies.map((reply) => (
                        <Comment key={reply.id} comment={reply} />
                    ))}
                </div>
            )}
        </div>
    );
};
