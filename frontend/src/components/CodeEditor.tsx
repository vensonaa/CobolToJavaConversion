import React from 'react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { tomorrow } from 'react-syntax-highlighter/dist/esm/styles/prism'

interface CodeEditorProps {
  value: string
  onChange: (value: string) => void
  language: string
  placeholder?: string
}

const CodeEditor: React.FC<CodeEditorProps> = ({
  value,
  onChange,
  language,
  placeholder
}) => {
  return (
    <div className="relative min-w-0">
      <textarea
        value={value}
        onChange={(e) => onChange(e.target.value)}
        placeholder={placeholder}
        className="w-full h-48 sm:h-64 px-3 sm:px-4 py-2 sm:py-3 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500 font-mono text-xs sm:text-sm bg-transparent relative z-10 resize-none overflow-hidden"
        style={{
          color: 'transparent',
          caretColor: 'black',
        }}
      />
      <div className="absolute inset-0 pointer-events-none">
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
            borderRadius: '6px',
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
