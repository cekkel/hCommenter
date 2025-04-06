import { defineConfig } from 'orval';

export default defineConfig({
  api: {
    input: "../api-swagger.json",
    output: {
      mode: "split",
      target: "./src/api/generated",
      schemas: "./src/api/model",
      client: "react-query",
      override: {
        mutator: {
          path: "./src/api/mutator/custom-instance.ts",
          name: "customInstance",
        },
      },
    },
    hooks: {
      afterAllFilesWrite: 'prettier --write',
    },
  },
});
