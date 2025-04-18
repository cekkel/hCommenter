ARG BASE_IMAGE=hcommenter:base

FROM $BASE_IMAGE AS build-stage
WORKDIR /opt/hCommenter

# Now build the app executable
COPY . ./
RUN make build-static && \
    cp "$(cabal list-bin hCommenter-Api)" .

#
# FINAL SEPARATE IMAGE WITH JUST THE STATICALLY-LINKED EXECUTABLE
#

FROM scratch
COPY --from=build-stage /opt/hCommenter/hCommenter-Api /

# Port 80 to support azure app service expectations
ENV APP_PORT=80
EXPOSE 80

ENTRYPOINT ["/hCommenter-Api"]
