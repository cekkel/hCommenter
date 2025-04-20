PROJECT_NAME := hCommenter-Api
CABAL_FILE := hCommenter-Api.cabal
HPACK_FILE := package.yaml
HPACK := hpack

# Default target to build the project
all: build

# Target to run hpack (generate .cabal file if it doesn't exist or if it needs updating)
run-hpack:
	$(HPACK) -f

check-hpack:
	@if [ ! -f "$(CABAL_FILE)" ] || [ "$(CABAL_FILE)" -ot "$(HPACK_FILE)" ]; then \
		@echo "Cabal file is out of date, hpack needs to be run."; \
		@false; \
	fi

build: run-hpack
	cabal build

build-only-deps: check-hpack
	cabal build --only-dependencies -j

build-backend-static: check-hpack
	cabal build backend-exe --enable-executable-static \
	&& cp "$(shell cabal list-bin backend-exe)" .

build-frontend-static: check-hpack
	cabal build frontend-exe --enable-executable-static \
	&& cp "$(shell cabal list-bin frontend-exe)" .

test: check-hpack
	cabal test \
		--test-show-details=direct

# Uncomment the following lines to run specific tests for hedgehog
# --test-option=--match="" \
# --test-option=--seed=""

run-back: check-hpack
	cabal run backend-exe

run-front: check-hpack
	cabal run frontend-exe

.PHONY: all run-hpack build build-no-deps build-backend-static build-frontend-static run-back run-front
