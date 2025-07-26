import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { Layout } from "./components/Layout";
import { HomePage } from "./pages/HomePage";

const queryClient = new QueryClient();

const App = () => {
  return (
    <QueryClientProvider client={queryClient}>
      <Layout>
        <HomePage />
      </Layout>
    </QueryClientProvider>
  );
};

export default App;
