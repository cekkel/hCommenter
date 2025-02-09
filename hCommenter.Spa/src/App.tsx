import { useState } from 'react'
import './App.css'
import { CommentList } from './components/CommentList'
import { Comment } from './types/comment'

function App() {
  // Mock data - will be replaced with backend data later
  const [comments] = useState<Comment[]>([
    {
      id: '1',
      content: 'This is a test comment',
      author: 'John Doe',
      timestamp: new Date('2023-09-02T10:00:00'),
      replies: [
        {
          id: '2',
          content: 'This is a reply to the test comment',
          author: 'Jane Smith',
          timestamp: new Date('2023-09-02T10:30:00')
        }
      ]
    },
    {
      id: '3',
      content: 'Another top-level comment',
      author: 'Bob Wilson',
      timestamp: new Date('2023-09-02T11:00:00')
    }
  ])

  return (
    <div className="app">
      <header className="app-header">
        <h1>Comment System</h1>
      </header>
      <main className="app-content">
        <CommentList comments={comments} />
      </main>
    </div>
  )
}

export default App
