import { useState } from "react";
import { AuthorComments } from "../comments/AuthorComments";
import { ConversationComments } from "../comments/ConversationComments";
import { SearchForm } from "../components/SearchForm";

export const HomePage = () => {
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
    <>
      <SearchForm
        title="Search by Username"
        inputValue={inputValue}
        setInputValue={setInputValue}
        onSubmit={handleUsernameSubmit}
        placeholder="Enter username..."
      />

      <AuthorComments username={username} />

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
