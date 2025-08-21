import React, { useState } from 'react'
import { FileUpload, Code, Download, Play, AlertCircle, CheckCircle, Clock } from 'lucide-react'
import CodeEditor from './components/CodeEditor'
import FileUploader from './components/FileUploader'
import ConversionStatus from './components/ConversionStatus'
import ResultsViewer from './components/ResultsViewer'
import { convertCobol, getConversionStatus, downloadResults } from './api/conversionApi'
import './App.css'

interface ConversionResult {
  conversion_id: string
  status: string
  progress: number
  message: string
  result?: any
}

function App() {
  const [cobolCode, setCobolCode] = useState('')
  const [priorKnowledge, setPriorKnowledge] = useState('')
  const [conversionResult, setConversionResult] = useState<ConversionResult | null>(null)
  const [isConverting, setIsConverting] = useState(false)
  const [activeTab, setActiveTab] = useState<'editor' | 'upload'>('editor')

  const handleConvert = async () => {
    if (!cobolCode.trim()) {
      alert('Please enter COBOL code')
      return
    }

    setIsConverting(true)
    try {
      const response = await convertCobol({
        cobol_code: cobolCode,
        prior_knowledge: priorKnowledge,
        generate_separate_files: true
      })

      setConversionResult({
        conversion_id: response.conversion_id,
        status: response.status,
        progress: 0,
        message: response.message
      })

      // Poll for status updates
      pollConversionStatus(response.conversion_id)
    } catch (error) {
      console.error('Conversion failed:', error)
      alert('Conversion failed. Please try again.')
    } finally {
      setIsConverting(false)
    }
  }

  const pollConversionStatus = async (conversionId: string) => {
    const pollInterval = setInterval(async () => {
      try {
        const status = await getConversionStatus(conversionId)
        setConversionResult(status)

        if (status.status === 'completed' || status.status === 'failed') {
          clearInterval(pollInterval)
        }
      } catch (error) {
        console.error('Error polling status:', error)
        clearInterval(pollInterval)
      }
    }, 2000)
  }

  const handleFileUpload = (fileContent: string) => {
    setCobolCode(fileContent)
    setActiveTab('editor')
  }

  const handleDownload = async () => {
    if (!conversionResult?.conversion_id) return

    try {
      await downloadResults(conversionResult.conversion_id)
    } catch (error) {
      console.error('Download failed:', error)
      alert('Download failed. Please try again.')
    }
  }

  const getStatusIcon = () => {
    if (!conversionResult) return null

    switch (conversionResult.status) {
      case 'processing':
        return <Clock className="w-5 h-5 text-blue-500 animate-spin" />
      case 'completed':
        return <CheckCircle className="w-5 h-5 text-green-500" />
      case 'failed':
        return <AlertCircle className="w-5 h-5 text-red-500" />
      default:
        return null
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <header className="bg-white shadow-sm border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-6">
            <div className="flex items-center space-x-3">
              <Code className="w-8 h-8 text-blue-600" />
              <div>
                <h1 className="text-2xl font-bold text-gray-900">
                  COBOL to Java Converter
                </h1>
                <p className="text-sm text-gray-500">
                  Multi-Agent LangGraph Conversion System
                </p>
              </div>
            </div>
            <div className="flex items-center space-x-4">
              <span className="text-sm text-gray-500">v2.0.0</span>
            </div>
          </div>
        </div>
      </header>

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
          {/* Input Section */}
          <div className="space-y-6">
            {/* Tab Navigation */}
            <div className="flex space-x-1 bg-gray-100 p-1 rounded-lg">
              <button
                onClick={() => setActiveTab('editor')}
                className={`flex-1 py-2 px-4 rounded-md text-sm font-medium transition-colors ${
                  activeTab === 'editor'
                    ? 'bg-white text-blue-600 shadow-sm'
                    : 'text-gray-600 hover:text-gray-900'
                }`}
              >
                <Code className="w-4 h-4 inline mr-2" />
                Code Editor
              </button>
              <button
                onClick={() => setActiveTab('upload')}
                className={`flex-1 py-2 px-4 rounded-md text-sm font-medium transition-colors ${
                  activeTab === 'upload'
                    ? 'bg-white text-blue-600 shadow-sm'
                    : 'text-gray-600 hover:text-gray-900'
                }`}
              >
                <FileUpload className="w-4 h-4 inline mr-2" />
                File Upload
              </button>
            </div>

            {/* Input Content */}
            {activeTab === 'editor' ? (
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    COBOL Code
                  </label>
                  <CodeEditor
                    value={cobolCode}
                    onChange={setCobolCode}
                    language="cobol"
                    placeholder="Enter your COBOL code here..."
                  />
                </div>

                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Prior Knowledge (Optional)
                  </label>
                  <textarea
                    value={priorKnowledge}
                    onChange={(e) => setPriorKnowledge(e.target.value)}
                    rows={3}
                    className="w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-blue-500 focus:border-blue-500"
                    placeholder="Additional context or requirements for the conversion..."
                  />
                </div>

                <button
                  onClick={handleConvert}
                  disabled={isConverting || !cobolCode.trim()}
                  className="w-full flex items-center justify-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {isConverting ? (
                    <>
                      <Clock className="w-4 h-4 mr-2 animate-spin" />
                      Converting...
                    </>
                  ) : (
                    <>
                      <Play className="w-4 h-4 mr-2" />
                      Convert to Java
                    </>
                  )}
                </button>
              </div>
            ) : (
              <FileUploader onFileUpload={handleFileUpload} />
            )}
          </div>

          {/* Results Section */}
          <div className="space-y-6">
            <div className="bg-white rounded-lg shadow-sm border p-6">
              <h2 className="text-lg font-medium text-gray-900 mb-4">
                Conversion Results
              </h2>

              {!conversionResult ? (
                <div className="text-center py-12">
                  <Code className="w-12 h-12 text-gray-400 mx-auto mb-4" />
                  <p className="text-gray-500">
                    Start a conversion to see results here
                  </p>
                </div>
              ) : (
                <div className="space-y-4">
                  {/* Status */}
                  <div className="flex items-center space-x-3">
                    {getStatusIcon()}
                    <div>
                      <p className="text-sm font-medium text-gray-900">
                        {conversionResult.message}
                      </p>
                      <p className="text-xs text-gray-500">
                        ID: {conversionResult.conversion_id}
                      </p>
                    </div>
                  </div>

                  {/* Progress */}
                  {conversionResult.status === 'processing' && (
                    <ConversionStatus
                      progress={conversionResult.progress}
                      message={conversionResult.message}
                    />
                  )}

                  {/* Results */}
                  {conversionResult.status === 'completed' && conversionResult.result && (
                    <ResultsViewer result={conversionResult.result} />
                  )}

                  {/* Download Button */}
                  {conversionResult.status === 'completed' && (
                    <button
                      onClick={handleDownload}
                      className="w-full flex items-center justify-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                    >
                      <Download className="w-4 h-4 mr-2" />
                      Download Results
                    </button>
                  )}
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default App
