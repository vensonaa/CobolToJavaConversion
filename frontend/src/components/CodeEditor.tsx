import React, { useRef, useEffect } from 'react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { tomorrow } from 'react-syntax-highlighter/dist/esm/styles/prism'

interface CodeEditorProps {
  value: string
  onChange: (value: string) => void
  language: string
  placeholder?: string
  height?: string
}

const CodeEditor: React.FC<CodeEditorProps> = ({
  value,
  onChange,
  language,
  placeholder,
  height = "h-48 sm:h-64"
}) => {
  const textareaRef = useRef<HTMLTextAreaElement>(null)
  const syntaxHighlighterRef = useRef<HTMLDivElement>(null)

  // Synchronize scroll between textarea and syntax highlighter
  useEffect(() => {
    const textarea = textareaRef.current
    const syntaxHighlighter = syntaxHighlighterRef.current

    if (!textarea || !syntaxHighlighter) return

    const handleScroll = () => {
      syntaxHighlighter.scrollTop = textarea.scrollTop
      syntaxHighlighter.scrollLeft = textarea.scrollLeft
    }

    textarea.addEventListener('scroll', handleScroll)
    return () => textarea.removeEventListener('scroll', handleScroll)
  }, [])

  return (
    <div className="relative bg-white border border-gray-300 rounded-md shadow-sm overflow-hidden">
      <textarea
        ref={textareaRef}
        value={value}
        onChange={(e) => onChange(e.target.value)}
        placeholder={placeholder}
        className={`w-full ${height} px-3 sm:px-4 py-2 sm:py-3 border-0 focus:outline-none focus:ring-0 font-mono text-xs sm:text-sm bg-transparent relative z-10 resize-none overflow-y-auto`}
        style={{
          color: 'transparent',
          caretColor: 'black',
        }}
      />
      <div 
        ref={syntaxHighlighterRef}
        className="absolute inset-0 pointer-events-none overflow-y-auto"
      >
        <SyntaxHighlighter
          language={language}
          style={tomorrow}
          customStyle={{
            margin: 0,
            padding: '8px 12px',
            fontSize: '12px',
            lineHeight: '1.5',
            backgroundColor: 'transparent',
            border: 'none',
            borderRadius: '0',
            minHeight: '100%',
          }}
          showLineNumbers
        >
          {value || placeholder || ''}
        </SyntaxHighlighter>
      </div>
    </div>
  )
}

export default CodeEditor
