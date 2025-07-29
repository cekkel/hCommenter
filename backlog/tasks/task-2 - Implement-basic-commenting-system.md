---
id: task-2
title: Implement basic commenting system
status: Done
assignee: []
created_date: '2025-07-13'
updated_date: '2025-07-13'
labels: []
dependencies:
  - task-1
---

## Description

This task involves creating the core functionality for users to post and view comments. It will include the API endpoints and basic frontend components for the commenting feature.

## Acceptance Criteria

- [x] Users can post new comments.
- [x] Users can view a list of existing comments.
- [x] The system correctly associates comments with the content they belong to.

## Implementation Plan

1.  Create a search form to input a conversation URL.
2.  Fetch comments for the given URL using the `/comments/conversation/{convoUrl}` endpoint.
3.  Display the fetched comments in a list.
4.  Use the existing `ConversationComments` component to encapsulate this logic.

## Implementation Notes

### Approach Taken

Verified that the existing API implementation fulfills all acceptance criteria for the basic commenting system. No code changes were necessary for the backend.

### Features Implemented or Modified

-   **`POST /comments/new`**: This endpoint allows users to post new comments. The request body includes the `convoUrl` to associate the comment with specific content.
-   **`GET /comments/conversation/{convoUrl}`**: This endpoint retrieves all comments associated with a given `convoUrl`, enabling users to view them.

### Modified or Added Files

-   No files were modified. This task was a verification of existing API functionality against the acceptance criteria.
