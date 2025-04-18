PROJECT_NAME := hCommenter-Api

docker-base-init:
	cd api; \
	docker build --file ./deploy/base.Dockerfile . --tag hcommenter:base

docker-base:
	cd api; \
	docker build --file ./deploy/base.Dockerfile . --tag hcommenter:base --build-arg BASE_IMAGE=hcommenter:base

dev:
	docker compose --profile dev up --build

prod:
	docker compose --profile prod up --build
