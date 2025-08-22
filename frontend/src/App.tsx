import React, { useState } from 'react'
import { Upload, Code, Download, Play, AlertCircle, CheckCircle, Clock, BarChart3, FileCode, Activity, XCircle } from 'lucide-react'
import CodeEditor from './components/CodeEditor'
import FileUploader from './components/FileUploader'
import ConversionStatus from './components/ConversionStatus'
import ResultsViewer from './components/ResultsViewer'
import Dashboard from './components/Dashboard'
import { convertCobol, getConversionStatus, downloadResults } from './api/conversionApi'
import './index.css'

interface ConversionResult {
  conversion_id: string
  status: string
  progress: number
  message: string
  result?: any
  error_message?: string // Added for failed conversions
}

function App() {
  const [cobolCode, setCobolCode] = useState('')
  const [priorKnowledge, setPriorKnowledge] = useState('')
  const [conversionResult, setConversionResult] = useState<ConversionResult | null>(null)
  const [isConverting, setIsConverting] = useState(false)
  const [activeTab, setActiveTab] = useState<'editor' | 'upload' | 'dashboard'>('editor')
  const [resultsTab, setResultsTab] = useState<'java' | 'summary' | 'pseudo'>('java')

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
        return <Clock className="w-5 h-5 text-amber-500 animate-spin" />
      case 'completed':
        return <CheckCircle className="w-5 h-5 text-emerald-500" />
      case 'failed':
        return <AlertCircle className="w-5 h-5 text-rose-500" />
      default:
        return null
    }
  }

  return (
    <div className="w-full h-screen bg-gradient-to-br from-slate-50 via-blue-50 to-indigo-50 flex flex-col overflow-hidden">
      {/* Header */}
      <header className="flex-shrink-0 bg-white/90 backdrop-blur-sm shadow-lg border-b border-white/20 px-3 sm:px-4 py-2 sm:py-3">
        <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2 sm:gap-0">
          <h1 className="text-lg sm:text-2xl font-bold bg-gradient-to-r from-indigo-600 to-purple-600 bg-clip-text text-transparent">
            COBOL to Java Converter
          </h1>
          <nav className="flex space-x-1 sm:space-x-2">
            <button
              onClick={() => setActiveTab('editor')}
              className={`px-2 sm:px-4 py-1.5 sm:py-2 rounded-lg text-xs sm:text-sm font-medium transition-all duration-200 ${
                activeTab === 'editor'
                  ? 'bg-gradient-to-r from-indigo-500 to-purple-600 text-white shadow-lg'
                  : 'text-gray-600 hover:text-gray-900 hover:bg-white/50'
              }`}
            >
              <Code className="w-3 h-3 sm:w-4 sm:h-4 inline mr-1 sm:mr-2" />
              <span className="hidden sm:inline">Editor</span>
              <span className="sm:hidden">Edit</span>
            </button>
            <button
              onClick={() => setActiveTab('upload')}
              className={`px-2 sm:px-4 py-1.5 sm:py-2 rounded-lg text-xs sm:text-sm font-medium transition-all duration-200 ${
                activeTab === 'upload'
                  ? 'bg-gradient-to-r from-indigo-500 to-purple-600 text-white shadow-lg'
                  : 'text-gray-600 hover:text-gray-900 hover:bg-white/50'
              }`}
            >
              <Upload className="w-3 h-3 sm:w-4 sm:h-4 inline mr-1 sm:mr-2" />
              <span className="hidden sm:inline">Upload</span>
              <span className="sm:hidden">Up</span>
            </button>
            <button
              onClick={() => setActiveTab('dashboard')}
              className={`px-2 sm:px-4 py-1.5 sm:py-2 rounded-lg text-xs sm:text-sm font-medium transition-all duration-200 ${
                activeTab === 'dashboard'
                  ? 'bg-gradient-to-r from-indigo-500 to-purple-600 text-white shadow-lg'
                  : 'text-gray-600 hover:text-gray-900 hover:bg-white/50'
              }`}
            >
              <BarChart3 className="w-3 h-3 sm:w-4 sm:h-4 inline mr-1 sm:mr-2" />
              <span className="hidden sm:inline">Dashboard</span>
              <span className="sm:hidden">Dash</span>
            </button>
          </nav>
        </div>
      </header>

      {/* Main Content */}
      <main className="w-full flex-1 min-h-0 overflow-hidden">
        {activeTab === 'editor' && (
          <div className="h-full flex flex-col gap-3 sm:gap-4 p-2 sm:p-4">
            {/* Two Column Layout */}
            <div className="flex-1 flex flex-col lg:flex-row gap-3 sm:gap-4 min-h-0">
              {/* Left Column - COBOL Code */}
              <div className="flex-1 flex flex-col min-h-0">
                <div className="bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 p-3 sm:p-4 flex-1 flex flex-col min-h-0">
                  <h2 className="text-sm sm:text-lg font-bold text-gray-900 mb-3 sm:mb-4 flex items-center flex-shrink-0">
                    <Code className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-indigo-600" />
                    <span className="hidden sm:inline">Input COBOL Code</span>
                    <span className="sm:hidden">COBOL Code</span>
                  </h2>
                  <div className="flex-1 min-h-0 overflow-y-auto">
                    <div className="prose prose-sm max-w-none">
                      <h3 className="text-lg font-bold text-gray-900 mb-3">Original COBOL Code</h3>
                      <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                        <textarea
                          value={cobolCode}
                          onChange={(e) => setCobolCode(e.target.value)}
                          placeholder="Enter your COBOL code here..."
                          className="w-full h-full min-h-[300px] text-sm text-gray-900 font-mono bg-white border-none outline-none resize-none"
                          style={{ fontFamily: 'monospace' }}
                        />
                      </div>
                    </div>
                  </div>
                </div>
              </div>

              {/* Right Column - Conversion Results with Tabs */}
              <div className="flex-1 flex flex-col min-h-0">
                <div className="bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 flex-1 flex flex-col min-h-0">
                  {/* Results Header with Tabs */}
                  <div className="flex-shrink-0 border-b border-gray-200">
                    <div className="flex items-center justify-between p-3 sm:p-4">
                      <h2 className="text-sm sm:text-lg font-bold text-gray-900 flex items-center">
                        <FileCode className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-emerald-600" />
                        <span className="hidden sm:inline">Conversion Results</span>
                        <span className="sm:hidden">Results</span>
                      </h2>
                      {conversionResult && conversionResult.status === 'completed' && (
                        <button
                          onClick={handleDownload}
                          className="flex items-center px-2 sm:px-3 py-1 sm:py-1.5 bg-gradient-to-r from-emerald-500 to-teal-600 text-white text-xs sm:text-sm rounded sm:rounded-lg hover:from-emerald-600 hover:to-teal-700 transition-all duration-200 shadow-md hover:shadow-lg"
                        >
                          <Download className="w-3 h-3 sm:w-4 sm:h-4 mr-1 sm:mr-1.5" />
                          <span className="hidden sm:inline">Download</span>
                          <span className="sm:hidden">DL</span>
                        </button>
                      )}
                    </div>
                    
                    {/* Tabs */}
                    <div className="flex border-b border-gray-200">
                      <button
                        onClick={() => setResultsTab('java')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-colors ${
                          resultsTab === 'java'
                            ? 'border-emerald-500 text-emerald-600 bg-emerald-50'
                            : 'border-transparent text-gray-500 hover:text-gray-700 hover:bg-gray-50'
                        }`}
                      >
                        <span className="hidden sm:inline">Java Code</span>
                        <span className="sm:hidden">Java</span>
                      </button>
                      <button
                        onClick={() => setResultsTab('summary')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-colors ${
                          resultsTab === 'summary'
                            ? 'border-emerald-500 text-emerald-600 bg-emerald-50'
                            : 'border-transparent text-gray-500 hover:text-gray-700 hover:bg-gray-50'
                        }`}
                      >
                        <span className="hidden sm:inline">Summary</span>
                        <span className="sm:hidden">Sum</span>
                      </button>
                      <button
                        onClick={() => setResultsTab('pseudo')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-colors ${
                          resultsTab === 'pseudo'
                            ? 'border-emerald-500 text-emerald-600 bg-emerald-50'
                            : 'border-transparent text-gray-500 hover:text-gray-700 hover:bg-gray-50'
                        }`}
                      >
                        <span className="hidden sm:inline">Pseudo Code</span>
                        <span className="sm:hidden">Pseudo</span>
                      </button>
                    </div>
                  </div>

                  {/* Tab Content */}
                  <div className="flex-1 min-h-0 p-3 sm:p-4">
                    {!conversionResult ? (
                      <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                        <Code className="w-10 h-10 sm:w-12 sm:h-12 text-gray-400 mx-auto mb-3 sm:mb-4" />
                        <p className="text-gray-500 font-medium text-sm sm:text-base">No conversion results yet</p>
                        <p className="text-xs sm:text-sm text-gray-400 mt-2 sm:mt-3">Enter COBOL code and click Convert to get started</p>
                      </div>
                    ) : conversionResult.status === 'processing' ? (
                      <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                        <Activity className="w-10 h-10 sm:w-12 sm:h-12 text-indigo-500 mx-auto mb-3 sm:mb-4 animate-spin" />
                        <p className="text-indigo-600 font-medium text-sm sm:text-base">Converting COBOL to Java...</p>
                        <p className="text-xs sm:text-sm text-gray-500 mt-2 sm:mt-3">This may take a few moments</p>
                      </div>
                    ) : conversionResult.status === 'completed' ? (
                      <div className="h-full">
                        {resultsTab === 'java' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-gray-900 mb-3">Java Code</h3>
                              <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                                <pre className="text-sm text-gray-900 whitespace-pre-wrap font-mono bg-white">
                                  {conversionResult.result?.final_java_code || 'No Java code available'}
                                </pre>
                              </div>
                            </div>
                          </div>
                        )}
                        {resultsTab === 'summary' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-gray-900 mb-3">Conversion Summary</h3>
                              <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                                <p className="text-gray-900 whitespace-pre-wrap">
                                  {conversionResult.result?.summary || 'No summary available'}
                                </p>
                              </div>
                            </div>
                          </div>
                        )}
                        {resultsTab === 'pseudo' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-gray-900 mb-3">Pseudo Code</h3>
                              <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                                <pre className="text-sm text-gray-900 whitespace-pre-wrap font-mono bg-white">
                                  {conversionResult.result?.pseudo_code || 'No pseudo code available'}
                                </pre>
                              </div>
                            </div>
                          </div>
                        )}
                      </div>
                    ) : (
                      <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                        <XCircle className="w-10 h-10 sm:w-12 sm:h-12 text-rose-500 mx-auto mb-3 sm:mb-4" />
                        <p className="text-rose-600 font-medium text-sm sm:text-base">Conversion Failed</p>
                        {conversionResult.error_message && (
                          <p className="text-xs sm:text-sm text-rose-500 mt-2 sm:mt-3">{conversionResult.error_message}</p>
                        )}
                      </div>
                    )}
                  </div>
                </div>
              </div>
            </div>

            {/* Convert Button */}
            <div className="flex-shrink-0 flex justify-center py-2 sm:py-3">
              <button
                onClick={handleConvert}
                disabled={!cobolCode.trim() || conversionResult?.status === 'processing'}
                className="flex items-center px-6 sm:px-8 py-2.5 sm:py-3 bg-gradient-to-r from-indigo-500 to-purple-600 text-white font-medium rounded-lg sm:rounded-xl hover:from-indigo-600 hover:to-purple-700 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 shadow-lg hover:shadow-xl transform hover:scale-105 text-sm sm:text-base"
              >
                {conversionResult?.status === 'processing' ? (
                  <>
                    <Activity className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 animate-spin" />
                    <span className="hidden sm:inline">Converting...</span>
                    <span className="sm:hidden">Converting</span>
                  </>
                ) : (
                  <>
                    <Play className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2" />
                    <span className="hidden sm:inline">Convert to Java</span>
                    <span className="sm:hidden">Convert</span>
                  </>
                )}
              </button>
            </div>
          </div>
        )}

        {activeTab === 'upload' && (
          <div className="h-full flex items-center justify-center p-2 sm:p-4">
            <FileUploader onFileUpload={handleFileUpload} />
          </div>
        )}

        {activeTab === 'dashboard' && (
          <div className="h-full">
            <Dashboard />
          </div>
        )}
      </main>
    </div>
  )
}

export default App
