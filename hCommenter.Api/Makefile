PROJECT_NAME := hCommenter-Api
CABAL_FILE := hCommenter-Api.cabal
HPACK := hpack

# Default target to build the project
all: build

# Target to run hpack (generate .cabal file if it doesn't exist or if it needs updating)
run-hpack:
	@if [ ! -f "$(CABAL_FILE)" ] || [ "$(CABAL_FILE)" -ot "package.yaml" ]; then \
		$(HPACK); \
	fi

check-hpack:
	@if [ ! -f "$(CABAL_FILE)" ] || [ "$(CABAL_FILE)" -ot "package.yaml" ]; then \
		@echo "Cabal file is out of date, hpack needs to be run."; \
		@false; \
	fi

build: run-hpack
	cabal build

build-no-deps: check-hpack
	cabal build --only-dependencies -j

build-static: check-hpack
	cabal build --enable-executable-static

.PHONY: all run-hpack build build-no-deps build-static
