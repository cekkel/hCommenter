# hCommenter - Embeddable commenting system

## Introduction

This is a commenting solution which can be easily added to any web page.

## Dependencies

- docker (with docker daemon running)
- docker compose plugin
- make

## Get started locally

1. Run `make docker-base` to create the base image for the API (to speed up build).
2. Run `docker compose --profile dev up --build` to build & start the API & SPA containers.
3. Access the app at `http://localhost:5173`.
