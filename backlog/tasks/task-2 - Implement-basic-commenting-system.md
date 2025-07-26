---
id: task-2
title: Implement basic commenting system
status: In Progress
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
