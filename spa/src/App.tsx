import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { CommentsList } from "./comments/AuthorComments";
import { useState } from "react";

const queryClient = new QueryClient();

const App = () => {
  const [username, setUsername] = useState("Abby");
  const [inputValue, setInputValue] = useState("Abby");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setUsername(inputValue);
  };

  return (
    <QueryClientProvider client={queryClient}>
      <div className="min-h-screen bg-gray-100 py-8 px-4 sm:px-6 lg:px-8">
        <div className="max-w-3xl mx-auto">
          <h1 className="text-4xl font-bold text-center text-gray-900 mb-8">
            hCommenter
          </h1>

          <div className="mb-8">
            <form onSubmit={handleSubmit} className="flex gap-2">
              <div className="flex-1">
                <input
                  type="text"
                  value={inputValue}
                  onChange={(e) => setInputValue(e.target.value)}
                  placeholder="Enter username..."
                  className="w-full px-4 py-2 rounded-lg border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                />
              </div>
              <button
                type="submit"
                className="px-4 py-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 transition-colors"
              >
                Search
              </button>
            </form>
          </div>

          <CommentsList username={username} />
        </div>
      </div>
    </QueryClientProvider>
  );
}

export default App;
