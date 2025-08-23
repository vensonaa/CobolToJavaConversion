import React, { useRef, useEffect, useCallback } from 'react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { tomorrow } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { useDropzone } from 'react-dropzone'


interface CodeEditorProps {
  value: string
  onChange: (value: string) => void
  language: string
  placeholder?: string
  height?: string
  onFileUpload?: (content: string) => void
}

const CodeEditor: React.FC<CodeEditorProps> = ({
  value,
  onChange,
  language,
  placeholder,
  height = "h-48 sm:h-64",
  onFileUpload
}) => {
  const textareaRef = useRef<HTMLTextAreaElement>(null)
  const syntaxHighlighterRef = useRef<HTMLDivElement>(null)

  // File upload handling
  const onDrop = useCallback((acceptedFiles: File[]) => {
    const file = acceptedFiles[0]
    if (file && onFileUpload) {
      const reader = new FileReader()
      reader.onload = (e) => {
        const content = e.target?.result as string
        onFileUpload(content)
      }
      reader.readAsText(file)
    }
  }, [onFileUpload])

  const { getRootProps, getInputProps, isDragActive } = useDropzone({
    onDrop,
    accept: {
      'text/plain': ['.cbl', '.cob', '.cobol'],
    },
    multiple: false,
  })

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

  // Debug scroll functionality
  useEffect(() => {
    const textarea = textareaRef.current
    if (textarea) {
      console.log('Textarea scroll height:', textarea.scrollHeight)
      console.log('Textarea client height:', textarea.clientHeight)
      console.log('Textarea overflow style:', textarea.style.overflow)
    }
  }, [value])

    return (
    <div 
      {...getRootProps()}
      className="relative bg-white border border-gray-300 rounded-md shadow-sm w-full h-full min-h-[500px] cursor-text"
    >
      {/* File Upload Overlay */}
      {isDragActive && (
        <div className="absolute inset-0 bg-blue-500/20 border-2 border-dashed border-blue-400 rounded-md z-20 flex items-center justify-center">
          <div className="text-center">
            <div className="w-8 h-8 text-blue-600 mx-auto mb-2">📁</div>
            <p className="text-blue-700 font-medium">Drop COBOL file here</p>
          </div>
        </div>
      )}

      {/* Editor Content */}
      <div className="relative h-full">
        <textarea
          ref={textareaRef}
          value={value}
          onChange={(e) => onChange(e.target.value)}
          placeholder=""
          className={`w-full h-full px-3 sm:px-4 py-2 sm:py-3 border-0 focus:outline-none focus:ring-0 font-mono text-xs sm:text-sm bg-transparent relative z-10 resize-none`}
          style={{
            color: 'transparent',
            caretColor: 'black',
            minHeight: '600px',
            overflowY: 'auto',
            overflowX: 'auto',
          }}
          onDragOver={(e) => e.preventDefault()}
          onDrop={(e) => e.preventDefault()}
        />
        <div 
          ref={syntaxHighlighterRef}
          className="absolute inset-0 pointer-events-none overflow-y-auto"
        >
          {!value && placeholder ? (
            <div className="px-3 sm:px-4 py-2 sm:py-3 text-gray-400 font-mono text-xs sm:text-sm">
              {placeholder}
              <div className="mt-2 text-xs text-gray-300">
                💡 You can also drag & drop a COBOL file (.cbl, .cob, .cobol) here
              </div>
            </div>
          ) : (
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
            }}
            showLineNumbers
          >
            {value}
          </SyntaxHighlighter>
          )}
        </div>
      </div>


    </div>
  )
}

export default CodeEditor
