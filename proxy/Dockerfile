# Use the official nginx image as base
FROM nginx:alpine

# Copy our custom nginx configuration
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Expose port 80
EXPOSE 80

# Use nginx's default CMD
CMD ["nginx", "-g", "daemon off;"]
