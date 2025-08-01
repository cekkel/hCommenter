<context>
# Overview  

This project is a commenting system that allows users to leave comments on various types of content. It is designed to be flexible and extensible, supporting different content types and user interactions.

It is intended for integration with existing services, by being added to the bottom of a webpage or application.

As such, the user of the service does not need to manage the comment system themselves, but rather can focus on the content they are providing.

# Core Features  

1. **Commenting**: Allows users to leave comments on content. This is the core functionality, enabling interaction and feedback.
2. **Voting System**: Enables users to upvote or downvote comments, promoting constructive discussions.
3. **User Authentication**: Supports user accounts for personalized experiences, allowing users to manage their comments and profiles.
4. **Content Moderation**: Provides tools for content owners to moderate comments, ensuring a safe and respectful environment.
5. **Notifications**: Notifies users of replies or mentions, keeping them engaged with the content.
5. **Search and Filtering**: Enables users to search and filter comments, making it easier to find relevant discussions.
6. **Analytics**: Offers insights into comment activity, helping content owners understand user engagement.
7. **Integration**: Easily integrates with existing web applications, allowing for seamless addition of commenting functionality.
8. **Customizable UI**: Allows customization of the comment section's appearance to match the host application's design.
9. **Rich Text Support**: Enables users to format their comments with basic rich text features, enhancing readability and engagement.
10. **Spam Protection**: Implements measures to prevent spam and abusive comments, maintaining the quality of discussions.
11. **Threaded Discussions**: Supports nested comments for better organization of discussions, allowing users to reply directly to specific comments.

# User Experience  

User personas include content creators, moderators, and end-users who interact with the comments. Key user flows involve leaving a comment, replying to existing comments, moderating comments, and managing user profiles.

</context>
<PRD>
# Technical Architecture  

The system consists of the following components:
1. **Frontend**: A web-based interface for users to interact with comments, built using React and vite.
2. **Backend**: A RESTful API built with Haskell and Servant, handling comment storage, user authentication, and moderation.
2. **API Management**: A reverse proxy to manage access to the SPA and API. In production, this will be handled by Azure API Management, but for development purposes, it is currently handled by nginx.
3. **Database**: A PostgreSQL database for storing comments, user profiles, and moderation data. Currently this is just a sqlite database for development purposes.
4. **Deployment**: The application will be containerized using Docker, allowing for easy deployment on various platforms. It will be hosted in azure container apps. Images are hosted in github container registry.
5. **Logging and Monitoring**: Implement logging for debugging and monitoring purposes, using tools like Loki and Grafana for performance metrics.

# Development Roadmap  
- **Phase 1: Core Functionality**
  - Implement basic commenting system with user authentication.
  - Set up the database schema for comments and users.
  - Create a simple frontend interface for posting and viewing comments.
- **Phase 2: Integration and Deployment**
  - Ensure easy integration with existing web applications.
  - Finalize deployment setup using Docker and Azure Container Apps.
- **Phase 3: Moderation and Notifications**
  - Add content moderation tools for comment approval and deletion.
  - Implement user notifications for replies and mentions.
- **Phase 4: Search and Analytics**
  - Develop search and filtering capabilities for comments.
  - Integrate analytics to track comment activity and user engagement.
- **Phase 5: UI Customization and Rich Text Support**
  - Allow customization of the comment section's appearance.
  - Implement basic rich text formatting for comments.
- **Phase 6: Spam Protection and Threaded Discussions**
  - Add spam protection mechanisms to filter out abusive comments.
  - Support nested comments for threaded discussions.

# Logical Dependency Chain
[Define the logical order of development:
- Which features need to be built first (foundation)
- Getting as quickly as possible to something usable/visible front end that works
- Properly pacing and scoping each feature so it is atomic but can also be built upon and improved as development approaches]

1. **Foundation**: Start with the core commenting functionality and user authentication, as these are essential for any interaction.
2. **Usable Frontend**: Develop a simple frontend interface that allows users to post and view comments, ensuring immediate usability.
3. **Deployment**: Set up the deployment pipeline using Docker and Azure Container Apps to ensure the application can be easily accessed and tested. Includes CI/CD.
4. **Moderation and Notifications**: Implement moderation tools and user notifications to enhance user engagement and content management.
5. **Search and Analytics**: Add search and filtering capabilities, followed by analytics to provide insights into user engagement.
6. **UI Customization and Rich Text**: Allow customization of the comment section's appearance and implement rich text support to enhance user experience.
7. **Spam Protection and Threaded Discussions**: Finally, implement spam protection and support for threaded discussions to improve comment quality and organization.


# Risks and Mitigations  

- Only one developer working on this as a personal project, which may lead to slower progress. Mitigation: Prioritize features and focus on building a minimal viable product (MVP) that can be iterated upon.
- Technical challenges with integrating the commenting system into various web applications. Mitigation: Ensure the API is well-documented and provide examples for easy integration.
- Potential performance issues with high comment volumes. Mitigation: Optimize database queries and implement caching strategies to handle large datasets efficiently.
- Haskell is a niche language, which may limit the pool of developers who can contribute. Mitigation: Focus on clear documentation and maintainability to ensure the codebase is accessible to future contributors.

# Appendix  
This project is inspired by existing commenting systems like Disqus, Github comments and Facebook comments, but aims to provide a more flexible and customizable solution that can be easily integrated into any web application.
</PRD>
