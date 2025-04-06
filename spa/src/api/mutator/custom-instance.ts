import axios, { AxiosRequestConfig } from "axios";

export const AXIOS_INSTANCE = axios.create({
  // baseURL: process.env.API_URL || "http://localhost:8080",
  baseURL: "http://localhost:8080",
  headers: {
    "Content-Type": "application/json",
  },
});

export const customInstance = <T>(
  config: AxiosRequestConfig<T>,
  options?: AxiosRequestConfig<T>,
): Promise<T> => {
  const source = axios.CancelToken.source();

  const promise = AXIOS_INSTANCE({
    ...config,
    ...options,
    cancelToken: source.token,
  }).then(({ data }) => data);

  // @ts-ignore
  promise.cancel = () => {
    source.cancel("Request canceled by the user.");
  }

  return promise;
}
