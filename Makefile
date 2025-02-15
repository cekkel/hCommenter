PROJECT_NAME := hCommenter-Api

docker-base:
	cd hCommenter.Api; \
	docker build --file ./base.Dockerfile . --tag hcommenter:base
