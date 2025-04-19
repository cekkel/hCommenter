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

run-test:
	cabal test \
		--test-show-details=direct

build: run-hpack
	cabal build

test: run-hpack run-test

test-ci: check-hpack run-test

# --test-option=--match="" \
# --test-option=--seed=""

run-back: check-hpack
	cabal run backend-exe

run-front: check-hpack
	cabal run frontend-exe

build-only-deps: check-hpack
	cabal build --only-dependencies -j

build-static: check-hpack
	cabal build --enable-executable-static

.PHONY: all run-hpack build build-no-deps build-static
