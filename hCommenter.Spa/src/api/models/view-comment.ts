export interface ViewComment {
    id: number;
    created: string;
    message: string;
    score: number;
    authorName: string;
    conversationUrl: string;
    replies?: {
        info: ViewComment[];
        pagination: {
            totalReplies: number;
            cursor: number;
        };
    };
}
