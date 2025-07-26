# Project configuration
project-name := "hCommenter-Api"

# List available commands
default:
    @just --list

# Need to be running the API locally first and have prettier installed.
write-swagger:
    curl http://localhost:80/swagger.json | prettier --parser json > api/api-swagger.json

# Build the initial Docker image
build-base-image:
    docker build ./api \
      --file ./api/deploy/build.Dockerfile \
      --tag hcommenter:base

# Build Docker image using the base image
rebuild-base-image:
    docker build ./api \
      --file ./api/deploy/build.Dockerfile \
      --tag hcommenter:base \
      --build-arg BASE_IMAGE=hcommenter:base

# Run development environment
dev:
    docker compose --profile dev up --watch --remove-orphans --build

# Run production environment
prod:
    docker compose --profile prod up --build --remove-orphans
