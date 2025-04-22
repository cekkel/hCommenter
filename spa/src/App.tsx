import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { CommentsList } from "./comments/AuthorComments";
import { ConversationComments } from "./comments/ConversationComments";
import { useState } from "react";
import { SearchForm } from "./components/SearchForm";
import { Layout } from "./components/Layout";

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
      <Layout>
        <SearchForm
          title="Search by Username"
          inputValue={inputValue}
          setInputValue={setInputValue}
          onSubmit={handleUsernameSubmit}
          placeholder="Enter username..."
        />

        <CommentsList username={username} />

        <SearchForm
          title="Search by Conversation URL"
          inputValue={convoInputValue}
          setInputValue={setConvoInputValue}
          onSubmit={handleConvoSubmit}
          placeholder="Enter conversation URL..."
        />

        <ConversationComments convoUrl={convoUrl} />
      </Layout>
    </QueryClientProvider>
  );
};

export default App;
