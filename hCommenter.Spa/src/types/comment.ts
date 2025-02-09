export type SortBy = 'Old' | 'New' | 'Popular' | 'Controversial';

export interface ViewComment {
    id: number;
    content: string;
    author: string;
    timestamp: Date;
    replies?: ViewComment[];
}

export interface NewComment {
    body: string;
}

export interface EditComment {
    id: number;
    body: string;
}
