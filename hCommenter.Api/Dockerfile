# syntax=docker/dockerfile:1
FROM haskell:9.10.1-slim-bullseye AS build-stage
WORKDIR /opt/hCommenter

# Install ghcup for now since the image has an old version of cabal.
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
COPY ./hCommenter-Api.cabal /opt/hCommenter/hCommenter-Api.cabal
RUN cabal update && make build-no-deps

# Now build the app
COPY . /opt/hCommenter
RUN make build-static && \
    cp "$(cabal list-bin hCommenter-Api)" .

#
# SEPARATE IMAGE WITH JUST THE STATICALLY-LINKED EXECUTABLE
#

FROM scratch
COPY --from=build-stage /opt/hCommenter/hCommenter-Api /

# Port 80 to support azure app service expectations
ENV API_PORT=8080
EXPOSE 8080

ENTRYPOINT ["/hCommenter-Api"]
