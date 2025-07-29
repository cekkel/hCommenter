import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { useState } from "react";
import { ConversationComments } from "./comments/ConversationComments";
import { Layout } from "./components/Layout";
import { SearchForm } from "./components/SearchForm";

const queryClient = new QueryClient();

const App = () => {
  const [convoUrl, setConvoUrl] = useState("example.com/my-article");
  const [submittedUrl, setSubmittedUrl] = useState(convoUrl);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setSubmittedUrl(convoUrl);
  };

  return (
    <QueryClientProvider client={queryClient}>
      <Layout>
        <div className="space-y-8">
          <SearchForm
            title="Search Comments by URL"
            inputValue={convoUrl}
            setInputValue={setConvoUrl}
            onSubmit={handleSubmit}
            placeholder="example.com/my-article"
          />
          <ConversationComments convoUrl={submittedUrl} />
        </div>
      </Layout>
    </QueryClientProvider>
  );
};

export default App;
