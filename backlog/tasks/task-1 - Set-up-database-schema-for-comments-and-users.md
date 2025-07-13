---
id: task-1
title: Set up database schema for comments and users
status: Done
assignee: []
created_date: '2025-07-13'
updated_date: '2025-07-13'
labels: []
dependencies: []
---

## Description

This task involves defining and creating the necessary database tables for storing user information and comments. This is a foundational step for the commenting system.

## Acceptance Criteria

- [ ] User table is created with fields for user ID
- [ ] username
- [ ] email
- [ ] and password hash.
- [ ] Comments table is created with fields for comment ID
- [ ] user ID (foreign key)
- [ ] content
- [ ] and timestamp.
- [ ] A migration script is created to set up the initial schema.
