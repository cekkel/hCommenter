# syntax=docker/dockerfile:1
# NOTE: Please, occasionally check for the latest version of the base image
ARG BASE_IMAGE=haskell:9.10.1-slim-bullseye@sha256:8bac50f7fb10b02f631ed1db17f953f722c47a7d6c8d137154d88a5b556c42d4

FROM $BASE_IMAGE AS base
WORKDIR /opt/hCommenter

# The following can install ghcup to get the latest version of cabal if needed.
#
# ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# 
# # Add ghcup to PATH
# ENV PATH=/root/.local/bin:${PATH}
# ENV PATH=/root/.ghcup/bin:${PATH}
# 
# RUN ghcup install ghc 9.10.1 && \
#     ghcup set ghc 9.10.1 && \
#     ghcup install cabal 3.12.1.0 && \
#     ghcup set cabal 3.12.1.0 && \
#     cabal update

##
## Dependency stage
##

# Install pkg-config (local) dependencies and other tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config zlib1g

# Install 'just' (a makefile alternative). Not available in apt, so installing it manually
COPY ./scripts/install_just.sh .
RUN chmod +x ./install_just.sh \
    && ./install_just.sh

# Then install dependencies only first, to improve caching
COPY ./justfile .
COPY ./package.yaml .
COPY ./cabal.* .
COPY ./*.cabal .
RUN cabal update && just build-only-deps

# Build app to speed up new compilations further.
COPY . ./
RUN cabal build

# Allow use of base image for running tests
ENTRYPOINT ["/bin/bash"]
