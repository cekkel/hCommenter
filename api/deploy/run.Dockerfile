ARG BASE_IMAGE=hcommenter:base

FROM $BASE_IMAGE AS build-stage
WORKDIR /opt/hCommenter

# Now build the app executable (it will be copied to current directory)
COPY . ./
RUN make build-backend-static

#
# FINAL SEPARATE IMAGE WITH JUST THE STATICALLY-LINKED EXECUTABLE
#

FROM scratch
COPY --from=build-stage /opt/hCommenter/backend-exe /

# Port 80 to support azure app service expectations
ENV APP_PORT=8080
EXPOSE 8080

ENTRYPOINT ["/backend-exe"]
