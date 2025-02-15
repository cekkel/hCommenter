ARG BASE_IMAGE=hcommenter:base

FROM $BASE_IMAGE AS build-stage
WORKDIR /opt/hCommenter

# Build app and run tests
COPY . ./
RUN make test-ci

# Now build the app executable
RUN make build-static && \
    cp "$(cabal list-bin hCommenter-Api)" .

#
# FINAL SEPARATE IMAGE WITH JUST THE STATICALLY-LINKED EXECUTABLE
#

FROM scratch
COPY --from=build-stage /opt/hCommenter/hCommenter-Api /

# Port 80 to support azure app service expectations
ENV API_PORT=8080
EXPOSE 8080

ENTRYPOINT ["/hCommenter-Api"]
