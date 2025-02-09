export interface Comment {
    id: string;
    content: string;
    author: string;
    timestamp: Date;
    replies?: Comment[];
}
