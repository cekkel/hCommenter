# Project configuration
project-name := "hCommenter-Api"

# List available commands
default:
    @just --list

# Need to be running the API locally first and have prettier installed.
write-swagger:
  curl http://localhost:8080/swagger.json | prettier --parser json > spa/api-swagger.json \

# Build the initial Docker image
docker-build-init:
    docker build ./api \
      --file ./api/deploy/build.Dockerfile \
      --tag hcommenter:base

# Build Docker image using the base image
docker-build:
    docker build ./api \
      --file ./api/deploy/build.Dockerfile \
      --tag hcommenter:base \
      --build-arg BASE_IMAGE=hcommenter:base

# Run development environment
dev:
    docker compose --profile dev up --build

# Run production environment
prod:
    docker compose --profile prod up --build
