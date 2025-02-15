PROJECT_NAME := hCommenter-Api

docker-base:
	cd hCommenter.Api; \
	docker build --file ./base.Dockerfile . --tag hcommenter:base

dev:
	docker compose --profile dev up --build

prod:
	docker compose --profile prod up --build
