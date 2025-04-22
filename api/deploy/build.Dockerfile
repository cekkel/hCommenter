# syntax=docker/dockerfile:1
# NOTE: Please, occasionally check for the latest version of the base image
ARG BASE_IMAGE=haskell:9.10.1-bullseye@sha256:63cc96fd6a57c345dc259881d20929839797677346cbfdd770b8504b9238f822

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

# Install ghcid for use as a hot-reload tool.
RUN cabal update && cabal install ghcid --overwrite-policy=always

# Then install dependencies only first, to improve caching
COPY ./justfile .
COPY ./package.yaml .
COPY ./cabal.* .
COPY ./*.cabal .
RUN just build-only-deps

# Build app to speed up new compilations further.
COPY . ./
RUN cabal build

# Allow use of base image for running tests
# -c for executing commands properly
ENTRYPOINT ["/bin/bash"]
