<system_context>
This document provides guidelines for agentic coding agents operating in the hCommenter repository.
It outlines build, lint, test, and code style conventions.

The API is a Haskell-based backend that serves a JSON API.
The SPA is a TypeScript/React single-page application that consumes the API.
</system_context>

<paved_path>
## Build

- **API (Haskell)**: From the `/api` directory, run `just build` to run a cabal build.
- **SPA (TypeScript)**: From the `/spa` directory, run `yarn build`

## Running the App

- To run the entire application stack (API, SPA, and proxy), run `just dev`
  from the root of the repository. This uses `docker-compose` to build and run all services.

## Lint

- **SPA**: `cd spa && npm run lint`

## Test

- **API**: The tests are run inside the docker container.
  See the `.github/workflows/api-build-and-deploy.yml` file for the test command: `just test`.
- **SPA**: There are no explicit test commands in `package.json`.
</paved_path>

<patterns>
## Code Style

- **Haskell**: Follows standard Haskell conventions, uses `fourmolu` for formatting,
  organizes imports, and uses a custom prelude.
- **TypeScript**: Uses `prettier` for formatting and `eslint` for linting.
  See `spa/eslint.config.js` and `spa/.prettierrc` for rules.

## Naming Conventions

- **Haskell**: UpperCamelCase for types, camelCase for functions.
- **TypeScript**: UpperCamelCase for components and types,
  camelCase for functions and variables.

## Error Handling

- **Haskell**: Uses `Effectful` for effects and error handling.
- **TypeScript**: Standard try/catch blocks and React error boundaries.
</patterns>

<critical_notes>
## Dependencies

- **Haskell**: Managed with `cabal` and `hpack`. See `api/package.yaml`.
- **TypeScript**: Managed with `npm`. See `spa/package.json`.
</critical_notes>
