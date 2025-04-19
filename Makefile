PROJECT_NAME := hCommenter-Api

docker-build-init:
	cd api; \
	docker build --file ./deploy/build.Dockerfile . --tag hcommenter:base

docker-build:
	cd api; \
	docker build --file ./deploy/build.Dockerfile . --tag hcommenter:base --build-arg BASE_IMAGE=hcommenter:base

dev:
	docker compose --profile dev up --build

prod:
	docker compose --profile prod up --build
