import React from 'react';

interface SearchFormProps {
  title: string;
  inputValue: string;
  setInputValue: (value: string) => void;
  onSubmit: (e: React.FormEvent) => void;
  placeholder: string;
}

export const SearchForm = ({ 
  title, 
  inputValue, 
  setInputValue, 
  onSubmit, 
  placeholder 
}: SearchFormProps) => {
  return (
    <div>
      <h2 className="text-xl font-semibold text-gray-800 mb-4">
        {title}
      </h2>
      <form onSubmit={onSubmit} className="flex gap-2">
        <div className="flex-1">
          <input
            type="text"
            value={inputValue}
            onChange={(e) => setInputValue(e.target.value)}
            placeholder={placeholder}
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
  );
};