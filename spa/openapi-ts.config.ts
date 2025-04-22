import { defineConfig } from '@hey-api/openapi-ts';

export default defineConfig({
  input: '../api-swagger.json',
  output: 'src/client',
  plugins: ['@tanstack/react-query', {
    name: '@hey-api/client-fetch',
    baseUrl: 'http://localhost:80',
  }],
});
