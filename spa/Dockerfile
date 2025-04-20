# syntax=docker.io/docker/dockerfile:1

FROM node:18 AS base

# Install dependencies only when needed
FROM base AS deps
WORKDIR /app

# Install dependencies based on the preferred package manager
COPY package.json yarn.lock* ./
RUN yarn --frozen-lockfile

# Rebuild the source code only when needed
FROM base AS builder
WORKDIR /app

COPY --from=deps /app/node_modules ./node_modules
COPY . .

RUN yarn build


#
# Target for running on a local machine.
#
FROM builder as dev

EXPOSE 5173
CMD ["yarn", "dev", "--host"]


#
# Production image, copy all the files and run using nginx
#
FROM nginx:alpine AS prod

COPY --from=builder /app/dist ./usr/share/nginx/html

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
