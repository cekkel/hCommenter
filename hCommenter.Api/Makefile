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

build: run-hpack
	cabal build

# Delete build artifacts
clean:
	cabal clean

.PHONY: all run-hpack build clean
