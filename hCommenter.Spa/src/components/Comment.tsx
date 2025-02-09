import { Comment as CommentType } from '../types/comment';

interface CommentProps {
    comment: CommentType;
}

export const Comment: React.FC<CommentProps> = ({ comment }) => {
    return (
        <div className="comment">
            <div className="comment-header">
                <span className="comment-author">{comment.author}</span>
                <span className="comment-timestamp">
                    {comment.timestamp.toLocaleString()}
                </span>
            </div>
            <div className="comment-content">{comment.content}</div>
            {comment.replies && comment.replies.length > 0 && (
                <div className="comment-replies">
                    {comment.replies.map(reply => (
                        <Comment key={reply.id} comment={reply} />
                    ))}
                </div>
            )}
        </div>
    );
};
