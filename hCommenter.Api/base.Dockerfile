# syntax=docker/dockerfile:1
ARG BASE_IMAGE=haskell:9.10.1-slim-bullseye

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

# First install dependencies only, to improve caching
COPY ./Makefile .
COPY ./package.yaml .
COPY ./cabal.project .
COPY ./hCommenter-Api.cabal .
RUN cabal update && make build-only-deps

# Build app to speed up new compilations further.
COPY . ./
RUN cabal build

# Allow use of base image for running tests
ENTRYPOINT ["/bin/bash"]
