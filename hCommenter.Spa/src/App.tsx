import { useState, useEffect } from 'react'
import './App.css'
import { CommentList } from './components/CommentList'
import { ViewComment } from './api/models/view-comment'
import { DefaultApi } from './api/api/default-api'
import { apiConfig, apiAxios } from './api/config'

function App() {
  const [comments, setComments] = useState<ViewComment[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  const api = new DefaultApi(apiConfig, undefined, apiAxios)
  const conversationUrl = 'test-conversation' // This would typically come from the current page URL

  useEffect(() => {
    const fetchComments = async () => {
      try {
        setLoading(true)
        setError(null)
        const response = await api.commentsConversationConvoUrlGet(conversationUrl)
        setComments(response.data)
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

  return (
    <div className="app">
      <header className="app-header">
        <h1>Comment System</h1>
      </header>
      <main className="app-content">
        {loading ? (
          <div className="loading">Loading comments...</div>
        ) : error ? (
          <div className="error">{error}</div>
        ) : comments.length === 0 ? (
          <div className="no-comments">No comments yet. Be the first to comment!</div>
        ) : (
          <CommentList comments={comments} />
        )}
      </main>
    </div>
  )
}

export default App
