import React, { useCallback } from 'react'
import { useDropzone } from 'react-dropzone'
import { Upload, FileText } from 'lucide-react'

interface FileUploaderProps {
  onFileUpload: (content: string) => void
}

const FileUploader: React.FC<FileUploaderProps> = ({ onFileUpload }) => {
  const onDrop = useCallback((acceptedFiles: File[]) => {
    const file = acceptedFiles[0]
    if (file) {
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

  return (
    <div
      {...getRootProps()}
      className={`border-2 border-dashed rounded-lg p-8 text-center cursor-pointer transition-colors ${
        isDragActive
          ? 'border-blue-400 bg-blue-50'
          : 'border-gray-300 hover:border-gray-400'
      }`}
    >
      <input {...getInputProps()} />
      <Upload className="w-12 h-12 text-gray-400 mx-auto mb-4" />
      {isDragActive ? (
        <p className="text-blue-600 font-medium">Drop the COBOL file here...</p>
      ) : (
        <div>
          <p className="text-gray-600 font-medium mb-2">
            Drag & drop a COBOL file here, or click to select
          </p>
          <p className="text-sm text-gray-500">
            Supports .cbl, .cob, .cobol files
          </p>
        </div>
      )}
    </div>
  )
}

export default FileUploader
