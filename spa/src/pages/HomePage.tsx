import { useState } from "react";
import { ConversationComments } from "../comments/ConversationComments";
import { SearchForm } from "../components/SearchForm";

export const HomePage = () => {
  const [convoUrl, setConvoUrl] = useState("convo.com");
  const [convoInputValue, setConvoInputValue] = useState("convo.com");

  const handleConvoSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setConvoUrl(convoInputValue);
  };

  return (
    <>
      <SearchForm
        title="Search by Conversation URL"
        inputValue={convoInputValue}
        setInputValue={setConvoInputValue}
        onSubmit={handleConvoSubmit}
        placeholder="Enter conversation URL..."
      />

      <ConversationComments convoUrl={convoUrl} />
    </>
  );
};
