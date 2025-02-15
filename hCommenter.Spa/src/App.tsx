import { useState, useEffect } from 'react'
import './App.css'
import { CommentList } from './components/CommentList'
import { ViewComment } from './api/models/view-comment'
import { DefaultApi } from './api/api/default-api'
import { apiConfig, apiAxios } from './api/config'
import { PaginationData } from './api/models/pagination-data'
import { Paginated } from './api/models/paginated'

function App() {
  const [comments, setComments] = useState<ViewComment[]>([])
  const [pagination, setPagination] = useState<PaginationData | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  const api = new DefaultApi(apiConfig, undefined, apiAxios)
  const conversationUrl = '1' // Initial conversation ID

  useEffect(() => {
    const fetchComments = async () => {
      try {
        setLoading(true)
        setError(null)
        const response = await api.commentsConversationConvoUrlGet(conversationUrl)
        const data = response.data as Paginated
        setComments(data.info)
        setPagination(data.pagination)
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : 'Failed to load comments. Please try again later.'
        setError(errorMessage)
        console.error('Error fetching comments:', err)
      } finally {
        setLoading(false)
      }
    }

    fetchComments()
  }, [conversationUrl])

  const loadMoreComments = async () => {
    if (!pagination || loading) return;

    try {
      setLoading(true);
      const response = await api.commentsConversationConvoUrlGet(conversationUrl);
      const data = response.data as Paginated;
      setComments(prevComments => [...prevComments, ...data.info]);
      setPagination(data.pagination);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to load more comments.';
      setError(errorMessage);
      console.error('Error loading more comments:', err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="app">
      <header className="app-header">
        <h1>Comment System</h1>
      </header>
      <main className="app-content">
        {loading && comments.length === 0 ? (
          <div className="loading">Loading comments...</div>
        ) : error ? (
          <div className="error">{error}</div>
        ) : comments.length === 0 ? (
          <div className="no-comments">No comments yet. Be the first to comment!</div>
        ) : (
          <>
            <CommentList comments={comments} />
            {pagination && pagination.cursor < pagination.totalReplies && (
              <div className="load-more">
                <button 
                  className="load-more-button" 
                  disabled={loading}
                  onClick={loadMoreComments}
                >
                  {loading ? 'Loading...' : 'Load More Comments'}
                </button>
              </div>
            )}
          </>
        )}
      </main>
    </div>
  )
}

export default App
