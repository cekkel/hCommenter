import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { CommentsList } from "./comments/AuthorComments";
import { ConversationComments } from "./comments/ConversationComments";
import { useState } from "react";

const queryClient = new QueryClient();

const App = () => {
  const [username, setUsername] = useState("Abby");
  const [inputValue, setInputValue] = useState("Abby");
  const [convoUrl, setConvoUrl] = useState("convo.com");
  const [convoInputValue, setConvoInputValue] = useState("convo.com");

  const handleUsernameSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setUsername(inputValue);
  };

  const handleConvoSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setConvoUrl(convoInputValue);
  };

  return (
    <QueryClientProvider client={queryClient}>
      <div className="min-h-screen bg-gray-100 py-8 px-4 sm:px-6 lg:px-8">
        <div className="max-w-3xl mx-auto">
          <h1 className="text-4xl font-bold text-center text-gray-900 mb-8">
            hCommenter
          </h1>

          <div className="space-y-8">
            <div>
              <h2 className="text-xl font-semibold text-gray-800 mb-4">Search by Username</h2>
              <form onSubmit={handleUsernameSubmit} className="flex gap-2">
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

            <div>
              <h2 className="text-xl font-semibold text-gray-800 mb-4">Search by Conversation URL</h2>
              <form onSubmit={handleConvoSubmit} className="flex gap-2">
                <div className="flex-1">
                  <input
                    type="text"
                    value={convoInputValue}
                    onChange={(e) => setConvoInputValue(e.target.value)}
                    placeholder="Enter conversation URL..."
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

            <ConversationComments convoUrl={convoUrl} />

          </div>
        </div>
      </div>
    </QueryClientProvider>
  );
}

export default App;
