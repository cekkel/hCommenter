import { Configuration } from './configuration';
import axios from 'axios';

// Create an axios instance with default config
const axiosInstance = axios.create({
    baseURL: 'http://localhost:8080',
    headers: {
        'Content-Type': 'application/json',
    },
});

// Add response interceptor for better error handling
axiosInstance.interceptors.response.use(
    response => response,
    error => {
        if (error.response) {
            // The request was made and the server responded with a status code
            // that falls out of the range of 2xx
            console.error('API Error:', error.response.data);
            return Promise.reject(new Error(error.response.data?.message || 'An error occurred'));
        } else if (error.request) {
            // The request was made but no response was received
            console.error('Network Error:', error.request);
            return Promise.reject(new Error('Unable to connect to the server'));
        } else {
            // Something happened in setting up the request that triggered an Error
            console.error('Request Error:', error.message);
            return Promise.reject(error);
        }
    }
);

export const apiConfig = new Configuration({
    basePath: 'http://localhost:8080',
    baseOptions: {
        headers: {
            'Accept': 'application/json',
        }
    },
});

// Export the axios instance to be used by the generated API client
export const apiAxios = axiosInstance;
