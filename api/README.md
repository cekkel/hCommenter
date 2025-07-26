# hCommenter API

This directory contains the Haskell backend for the hCommenter application.

## Development

The API is designed to be run inside a Docker container. The main project `justfile` provides commands to build and run the development environment.

From the root of the project:
1.  Build the base image: `just build-base-image`
2.  Start the development servers: `just dev`

The API will be available at `http://localhost:80`.

## API Documentation

The API endpoints are documented using the OpenAPI specification. When the API is running, the specification can be generated and saved to `api/api-swagger.json` by running `just write-swagger` from the root of the project.

## Testing

To run the tests for the API, use the following command from the `api` directory:

```sh
just lint
```
