# hCommenter - Embeddable commenting system

## Introduction

![WARN] hCommenter is a hobby program not intended for serious production use. 

Putting that aside, the intention of this project is to create a commenting solution which can be easily added to any web page.

## Dependencies

- docker (with docker daemon running)
- docker compose plugin
- just (a 'make' alternative specialised for running commands)

## Get started locally

1. If the project has never been built on your machine, first run `just build-base-image`. This may take 10-20 minutes.
2. Once the base image has been created, run `just dev` to build & start the API & SPA containers in file watch mode.
   The API & SPA should each restart when their respective source code changes.
3. Access the full app at [[http://localhost:5173]], or call the API directly at [[http://localhost:8080]].

## Database Schema

![Database Schema Diagram](./docs/DatabaseSchema.drawio.svg)

## Roadmap/TODO

- [x] Create basic commenting API with haskell
- [x] Setup CI/CD to deploy backend to a 'production' environment
- [x] Create basic UI with react to view comments associated with an author or url.
- [ ] Add comment creation, deletion, nesting, etc. to UI.
- [ ] Setup CI/CD to deploy UI to a 'production' environment
- [ ] Add authentication to API and UI.
- [ ] Add tests for API and UI.
- [ ] Add alternative UI solution using haskell + hyperbole (backend-driven web UI framework).


