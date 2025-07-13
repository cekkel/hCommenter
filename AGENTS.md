# Project Handover Document: hCommenter

## Project Overview
hCommenter is an embeddable commenting system designed as a hobby project. It is not intended for serious production use, but serves as a flexible solution for adding comments to web pages.

## Project Structure
```
.
├── api/           # Haskell Backend API
├── spa/           # React Frontend Single Page Application
├── docs/          # Project Documentation
├── infra/         # Infrastructure as Code
├── proxy/         # Nginx Proxy Configuration
└── backlog/       # Project Backlog and Configuration
```

## Key Technologies
- Backend: Haskell
- Frontend: React + TypeScript + Vite
- Infrastructure: Docker, Docker Compose
- Build Tool: `just` (command runner)
- Deployment: Containerized

## Development Setup
### Prerequisites
- Docker (with docker daemon)
- Docker Compose plugin
- `just` command runner

### Local Development Steps
1. Initial Build: `just build-base-image` (10-20 minutes)
2. Start Development Environment: `just dev`
   - Starts API & SPA containers in file watch mode
3. Access Points:
   - Web App: http://localhost:5173
   - API Endpoint: http://localhost:80

## Project Roadmap
### Completed
- [x] Basic commenting API in Haskell
- [x] CI/CD for backend deployment
- [x] Basic React UI to view comments

### Pending
- [ ] Add comment creation, deletion, nesting in UI
- [ ] CI/CD for UI deployment
- [ ] Authentication for API and UI
- [ ] Comprehensive testing
- [ ] Alternative UI using Haskell + Hyperbole

## Database
Database schema is visualized in `./docs/DatabaseSchema.drawio.svg`

## Development Notes
- Project is in early stages
- Intended as a learning/hobby project
- Not recommended for production use without significant enhancements

## Contribution Guidelines
1. Follow existing code structure and patterns
2. Ensure Docker and `just` command compatibility
3. Maintain separation of concerns between API and SPA
4. Update documentation with significant changes

## Potential Improvements
- Implement robust authentication
- Add comprehensive error handling
- Develop more advanced comment features
- Create more extensive test coverage

## Contact
For any questions about the project, please refer to the project maintainers.

