import { ViewComment } from '../api/models/view-comment';
import { Comment } from './Comment';

interface CommentListProps {
    comments: ViewComment[];
}

export const CommentList: React.FC<CommentListProps> = ({ comments }) => {
    return (
        <div className="comment-list">
            {comments.map(comment => (
                <Comment key={comment.id} comment={comment} />
            ))}
        </div>
    );
};
