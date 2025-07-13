---
id: task-2.4
title: Refetch comments after new comment creation
status: Done
assignee: []
created_date: '2025-07-13'
updated_date: '2025-07-13'
labels: []
dependencies: []
parent_task_id: task-2
---

## Description

After a new comment is created, the comment list should automatically refetch to display the new comment.

## Acceptance Criteria

- [x] When a new comment is successfully submitted
- [x] the list of comments on the page is automatically updated to include the new comment.

## Implementation Notes

Implemented automatic refetching of comments after a new comment is created. This was achieved by using the  hook from  to invalidate the relevant queries in the  callback of the  mutation in . This ensures that the comment list is always up-to-date without requiring a manual refresh.
