ARG BASE_IMAGE=hcommenter:base

FROM $BASE_IMAGE AS build-stage
WORKDIR /opt/hCommenter

# Now build the app executable (it will be copied to current directory)
COPY . ./
RUN just build-backend-static

#
# FINAL SEPARATE IMAGE WITH JUST THE STATICALLY-LINKED EXECUTABLE
#

FROM scratch
COPY --from=build-stage /opt/hCommenter/backend-exe /

# Port 80 to support azure app service expectations
ENV APP_PORT=80
EXPOSE 80

ENTRYPOINT ["/backend-exe"]
