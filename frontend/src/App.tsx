import React, { useState, useEffect } from 'react'
import { Upload, Code, Download, Play, AlertCircle, CheckCircle, Clock, BarChart3, FileCode, Activity, XCircle, X } from 'lucide-react'
import CodeEditor from './components/CodeEditor'
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
  const [activeTab, setActiveTab] = useState<'editor' | 'dashboard'>('editor')
  const [resultsTab, setResultsTab] = useState<'java' | 'summary' | 'pseudo'>('java')

  // Debug: Monitor conversion result changes
  useEffect(() => {
    console.log('=== CONVERSION RESULT STATE CHANGED ===')
    console.log('conversionResult:', conversionResult)
    if (conversionResult?.result) {
      console.log('result object:', conversionResult.result)
      console.log('final_java_code length:', conversionResult.result.final_java_code?.length || 0)
      console.log('java_code length:', conversionResult.result.java_code?.length || 0)
    }
    console.log('=== END STATE CHANGE ===')
  }, [conversionResult])

  const handleConvert = async () => {
    if (!cobolCode.trim()) {
      alert('Please enter COBOL code')
      return
    }

    setIsConverting(true)
    try {
      console.log('Starting conversion with COBOL code length:', cobolCode.length)
      
      const response = await convertCobol({
        cobol_code: cobolCode,
        prior_knowledge: priorKnowledge,
        generate_separate_files: true
      })

      console.log('Conversion response received:', response)

      setConversionResult({
        conversion_id: response.conversion_id,
        status: response.status,
        progress: 0,
        message: response.message
      })

      console.log('Initial conversion result set, starting polling for:', response.conversion_id)

      // Poll for status updates
      pollConversionStatus(response.conversion_id)
    } catch (error) {
      console.error('Conversion failed:', error)
      console.error('Error details:', error.message)
      alert('Conversion failed. Please try again.')
    } finally {
      setIsConverting(false)
    }
  }

  const pollConversionStatus = async (conversionId: string) => {
    console.log(`Starting to poll for conversion: ${conversionId}`)
    const pollInterval = setInterval(async () => {
      try {
        console.log(`Polling status for conversion: ${conversionId}`)
        const status = await getConversionStatus(conversionId)
        console.log(`Received status:`, status)
        
        if (status.status === 'completed') {
          console.log(`Conversion completed, fetching detailed results...`)
          // Fetch detailed results when conversion completes
          try {
            const detailedResponse = await fetch(`/api/conversions/${conversionId}/details`)
            if (!detailedResponse.ok) {
              throw new Error(`HTTP ${detailedResponse.status}: ${detailedResponse.statusText}`)
            }
            const detailedData = await detailedResponse.json()
            console.log(`Detailed data received:`, detailedData)
            
            // Simplified mapping - use the data directly as it comes from API
            const mappedResult = {
              conversion_id: detailedData.conversion_id,
              status: detailedData.status,
              progress: detailedData.progress || 100,
              message: detailedData.message || 'Conversion completed',
              result: {
                final_java_code: detailedData.java_code || '', // Use java_code directly as final_java_code
                java_code: detailedData.java_code || '',
                pseudo_code: detailedData.pseudo_code || '',
                summary: detailedData.summary || '',
                total_chunks: detailedData.total_chunks || 0,
                java_files: detailedData.java_files || []
              }
            }
            
            // Additional debug logging
            console.log('Java code from API:', detailedData.java_code ? detailedData.java_code.substring(0, 100) + '...' : 'NULL')
            console.log('Final Java code from API:', detailedData.final_java_code ? detailedData.final_java_code.substring(0, 100) + '...' : 'NULL')
            console.log('Mapped final_java_code:', mappedResult.result.final_java_code ? mappedResult.result.final_java_code.substring(0, 100) + '...' : 'NULL')
            
            console.log('Detailed API response:', detailedData)
            console.log('Mapped conversion result:', mappedResult)
            console.log('Java code length:', mappedResult.result?.final_java_code?.length || 0)
            setConversionResult(mappedResult)
            console.log(`Successfully set conversion result`)
          } catch (detailedError) {
            console.error('Error fetching detailed results:', detailedError)
            console.error('Detailed error details:', detailedError.message)
            // Fallback to basic status
            setConversionResult(status)
          }
          clearInterval(pollInterval)
        } else if (status.status === 'failed') {
          console.log(`Conversion failed: ${status.message}`)
          setConversionResult(status)
          clearInterval(pollInterval)
        } else {
          console.log(`Conversion still processing: ${status.status} - ${status.message}`)
          setConversionResult(status)
        }
      } catch (error) {
        console.error('Error polling status:', error)
        console.error('Error details:', error.message)
        clearInterval(pollInterval)
      }
    }, 2000)
  }

  const handleFileUpload = (fileContent: string) => {
    setCobolCode(fileContent)
  }

  const handleLoadCobolCode = (cobolCode: string, conversionResult?: any) => {
    setCobolCode(cobolCode)
    
    // Debug logging
    console.log('Loading conversion data:', conversionResult)
    
    // If conversion results are provided, set them
    if (conversionResult) {
      const mappedResult = {
        conversion_id: conversionResult.conversion_id,
        status: conversionResult.status,
        progress: conversionResult.progress || 100,
        message: conversionResult.message || 'Loaded from dashboard',
        result: {
          final_java_code: conversionResult.final_java_code || conversionResult.java_code,
          pseudo_code: conversionResult.pseudo_code,
          summary: conversionResult.summary,
          total_chunks: conversionResult.total_chunks,
          java_files: conversionResult.java_files || []
        }
      }
      
      console.log('Mapped conversion result:', mappedResult)
      setConversionResult(mappedResult)
    }
    
    setActiveTab('editor') // Switch to editor tab to show the loaded code and results
    // Show a brief success message
    const message = document.createElement('div')
    message.className = 'fixed top-4 right-4 bg-green-500 text-white px-4 py-2 rounded-lg shadow-lg z-50'
    message.textContent = conversionResult ? '✅ COBOL code and results loaded into editor' : '✅ COBOL code loaded into editor'
    document.body.appendChild(message)
    setTimeout(() => {
      document.body.removeChild(message)
    }, 3000)
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
    <div className="w-full h-screen flex flex-col bg-gradient-to-br from-purple-50 via-pink-50 to-indigo-50">
      {/* Header */}
      <header className="flex-shrink-0 bg-gradient-to-r from-purple-600 via-pink-600 to-indigo-600 text-white shadow-xl border-b-4 border-purple-400">
        <div className="container mx-auto px-4 py-3 sm:py-4">
          <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2 sm:gap-0">
            <h1 className="text-xl sm:text-2xl font-bold bg-gradient-to-r from-yellow-300 to-orange-300 bg-clip-text text-transparent">
              🚀 COBOL to Java Converter
            </h1>
            <nav className="flex gap-1 sm:gap-2">
              <button
                onClick={() => setActiveTab('editor')}
                className={`px-3 sm:px-4 py-1.5 sm:py-2 rounded-lg sm:rounded-xl font-medium text-sm sm:text-base transition-all duration-200 ${
                  activeTab === 'editor'
                    ? 'bg-gradient-to-r from-yellow-400 to-orange-400 text-white shadow-lg transform scale-105'
                    : 'bg-white/20 hover:bg-white/30 text-white hover:scale-105'
                }`}
              >
                <span className="hidden sm:inline">Code Editor</span>
                <span className="sm:hidden">Editor</span>
              </button>

              <button
                onClick={() => setActiveTab('dashboard')}
                className={`px-3 sm:px-4 py-1.5 sm:py-2 rounded-lg sm:rounded-xl font-medium text-sm sm:text-base transition-all duration-200 ${
                  activeTab === 'dashboard'
                    ? 'bg-gradient-to-r from-yellow-400 to-orange-400 text-white shadow-lg transform scale-105'
                    : 'bg-white/20 hover:bg-white/30 text-white hover:scale-105'
                }`}
              >
                <span className="hidden sm:inline">Dashboard</span>
                <span className="sm:hidden">Dashboard</span>
              </button>
            </nav>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="w-full flex-1 min-h-0">
        {activeTab === 'editor' && (
          <div className="h-full flex flex-col gap-3 sm:gap-4 p-2 sm:p-4">
            {/* Two Column Layout */}
            <div className="flex-1 flex flex-col lg:flex-row gap-3 sm:gap-4 min-h-0">
              {/* Left Column - COBOL Code */}
              <div className="flex-1 flex flex-col min-h-0 lg:w-1/2 h-full">
                <div className="bg-gradient-to-br from-blue-50 to-cyan-50 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-xl border-2 border-blue-200 p-3 sm:p-4 flex-1 flex flex-col min-h-0 w-full h-full">
                  <div className="flex items-center justify-between mb-3 sm:mb-4 flex-shrink-0">
                    <h2 className="text-sm sm:text-lg font-bold text-blue-900 flex items-center">
                      <Code className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-blue-600" />
                      <span className="hidden sm:inline">Input COBOL Code</span>
                      <span className="sm:hidden">COBOL Code</span>
                    </h2>
                    <div className="flex items-center space-x-2">
                      {/* File Upload Button */}
                      <div className="cursor-pointer">
                        <input 
                          type="file" 
                          accept=".cbl,.cob,.cobol,.txt"
                          onChange={(e) => {
                            const file = e.target.files?.[0]
                            if (file) {
                              const reader = new FileReader()
                              reader.onload = (e) => {
                                const content = e.target?.result as string
                                handleFileUpload(content)
                              }
                              reader.readAsText(file)
                            }
                          }}
                          className="hidden"
                          id="cobol-file-upload"
                        />
                        <label 
                          htmlFor="cobol-file-upload"
                          className="flex items-center px-2 py-1 text-xs bg-blue-500 text-white rounded hover:bg-blue-600 transition-colors cursor-pointer"
                        >
                          <Upload className="w-3 h-3 mr-1" />
                          Upload
                        </label>
                      </div>
                      
                      {/* Clear Button */}
                      {cobolCode && (
                        <button
                          onClick={() => {
                            setCobolCode('')
                            setConversionResult(null) // Clear conversion results when clearing COBOL code
                          }}
                          className="flex items-center px-2 py-1 text-xs bg-gray-500 text-white rounded hover:bg-gray-600 transition-colors"
                          title="Clear editor and results"
                        >
                          <X className="w-3 h-3" />
                        </button>
                      )}
                    </div>
                  </div>
                  <div className="flex-1 min-h-0 w-full h-full">
                    <CodeEditor
                      value={cobolCode}
                      onChange={setCobolCode}
                      language="cobol"
                      placeholder="Enter your COBOL code here or upload a file..."
                      height="h-full"
                      onFileUpload={handleFileUpload}
                    />
                  </div>
                </div>
              </div>

              {/* Right Column - Conversion Results with Tabs */}
              <div className="flex-1 flex flex-col min-h-0 lg:w-1/2">
                <div className="bg-gradient-to-br from-green-50 to-emerald-50 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-xl border-2 border-green-200 flex-1 flex flex-col min-h-0 w-full">
                  {/* Results Header with Tabs */}
                  <div className="flex-shrink-0 border-b-2 border-green-200">
                    <div className="flex items-center justify-between p-3 sm:p-4">
                      <h2 className="text-sm sm:text-lg font-bold text-green-900 flex items-center">
                        <FileCode className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-green-600" />
                        <span className="hidden sm:inline">Conversion Results</span>
                        <span className="sm:hidden">Results</span>
                      </h2>
                      {conversionResult && conversionResult.status === 'completed' && (
                        <button
                          onClick={handleDownload}
                          className="flex items-center px-2 sm:px-3 py-1 sm:py-1.5 bg-gradient-to-r from-emerald-500 to-teal-600 text-white text-xs sm:text-sm rounded sm:rounded-lg hover:from-emerald-600 hover:to-teal-700 transition-all duration-200 shadow-md hover:shadow-lg transform hover:scale-105"
                        >
                          <Download className="w-3 h-3 sm:w-4 sm:h-4 mr-1 sm:mr-1.5" />
                          <span className="hidden sm:inline">Download</span>
                          <span className="sm:hidden">DL</span>
                        </button>
                      )}
                    </div>
                    
                    {/* Tabs */}
                    <div className="flex border-b-2 border-green-200">
                      <button
                        onClick={() => setResultsTab('java')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-all duration-200 ${
                          resultsTab === 'java'
                            ? 'border-emerald-500 text-emerald-700 bg-gradient-to-r from-emerald-100 to-green-100'
                            : 'border-transparent text-green-600 hover:text-green-700 hover:bg-gradient-to-r hover:from-green-50 hover:to-emerald-50'
                        }`}
                      >
                        <span className="hidden sm:inline">Java Code</span>
                        <span className="sm:hidden">Java</span>
                      </button>
                      <button
                        onClick={() => setResultsTab('summary')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-all duration-200 ${
                          resultsTab === 'summary'
                            ? 'border-emerald-500 text-emerald-700 bg-gradient-to-r from-emerald-100 to-green-100'
                            : 'border-transparent text-green-600 hover:text-green-700 hover:bg-gradient-to-r hover:from-green-50 hover:to-emerald-50'
                        }`}
                      >
                        <span className="hidden sm:inline">Summary</span>
                        <span className="sm:hidden">Sum</span>
                      </button>
                      <button
                        onClick={() => setResultsTab('pseudo')}
                        className={`flex-1 px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm font-medium border-b-2 transition-all duration-200 ${
                          resultsTab === 'pseudo'
                            ? 'border-emerald-500 text-emerald-700 bg-gradient-to-r from-emerald-100 to-green-100'
                            : 'border-transparent text-green-600 hover:text-green-700 hover:bg-gradient-to-r hover:from-green-50 hover:to-emerald-50'
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
                        <Code className="w-10 h-10 sm:w-12 sm:h-12 text-green-400 mx-auto mb-3 sm:mb-4" />
                        <p className="text-green-700 font-medium text-sm sm:text-base">No conversion results yet</p>
                        <p className="text-xs sm:text-sm text-green-600 mt-2 sm:mt-3">Enter COBOL code and click Convert to get started</p>
                      </div>
                    ) : conversionResult.status === 'processing' ? (
                      <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                        <Activity className="w-10 h-10 sm:w-12 sm:h-12 text-emerald-500 mx-auto mb-3 sm:mb-4 animate-spin" />
                        <p className="text-emerald-700 font-medium text-sm sm:text-base">Converting COBOL to Java...</p>
                        <p className="text-xs sm:text-sm text-emerald-600 mt-2 sm:mt-3">This may take a few moments</p>
                      </div>
                    ) : conversionResult.status === 'completed' ? (
                      <div className="h-full">
                        {resultsTab === 'java' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-green-900 mb-3">Java Code</h3>
                              <div className="bg-gradient-to-br from-white to-green-50 rounded-lg p-4 mb-4 border-2 border-green-200 shadow-lg">
                                <pre className="text-sm text-green-900 whitespace-pre-wrap font-mono bg-transparent">
                                  {conversionResult.result?.final_java_code || conversionResult.result?.java_code || 'No Java code available'}
                                </pre>
                                
                                {/* Temporary raw data display for debugging */}
                                <div className="mt-4 p-2 bg-gray-100 rounded text-xs">
                                  <strong>Raw Data Debug:</strong><br/>
                                  final_java_code exists: {conversionResult.result?.final_java_code ? 'YES' : 'NO'}<br/>
                                  java_code exists: {conversionResult.result?.java_code ? 'YES' : 'NO'}<br/>
                                  final_java_code length: {conversionResult.result?.final_java_code?.length || 0}<br/>
                                  java_code length: {conversionResult.result?.java_code?.length || 0}<br/>
                                  final_java_code preview: {conversionResult.result?.final_java_code?.substring(0, 50) || 'NONE'}...<br/>
                                  java_code preview: {conversionResult.result?.java_code?.substring(0, 50) || 'NONE'}...
                                </div>
                                {/* Debug info */}
                                <div className="text-xs text-gray-500 mt-2">
                                  Debug: Status={conversionResult.status}, Has result={!!conversionResult.result}, Has java code={!!conversionResult.result?.final_java_code}, Java code length: {conversionResult.result?.final_java_code?.length || 0}
                                  <br />
                                  Raw java_code length: {conversionResult.result?.java_code?.length || 0}
                                  <br />
                                  <button 
                                    onClick={() => {
                                      console.log('Full conversionResult:', conversionResult)
                                      console.log('Result object:', conversionResult.result)
                                      alert(`Java code length: ${conversionResult.result?.final_java_code?.length || 0}`)
                                    }}
                                    className="text-blue-500 underline"
                                  >
                                    Debug Data
                                  </button>
                                </div>
                              </div>
                            </div>
                          </div>
                        )}
                        {resultsTab === 'summary' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-green-900 mb-3">Conversion Summary</h3>
                              <div className="bg-gradient-to-br from-white to-green-50 rounded-lg p-4 mb-4 border-2 border-green-200 shadow-lg">
                                <p className="text-green-900 whitespace-pre-wrap">
                                  {conversionResult.result?.summary || 'No summary available'}
                                </p>
                                {/* Debug info */}
                                <div className="text-xs text-gray-500 mt-2">
                                  Debug: Summary length: {conversionResult.result?.summary?.length || 0}, 
                                  Has summary: {!!conversionResult.result?.summary}
                                </div>
                              </div>
                            </div>
                          </div>
                        )}
                        {resultsTab === 'pseudo' && (
                          <div className="h-full overflow-y-auto">
                            <div className="prose prose-sm max-w-none">
                              <h3 className="text-lg font-bold text-green-900 mb-3">Pseudo Code</h3>
                              <div className="bg-gradient-to-br from-white to-green-50 rounded-lg p-4 mb-4 border-2 border-green-200 shadow-lg">
                                <pre className="text-sm text-green-900 whitespace-pre-wrap font-mono bg-transparent">
                                  {conversionResult.result?.pseudo_code || 'No pseudo code available'}
                                </pre>
                              </div>
                            </div>
                          </div>
                        )}
                      </div>
                    ) : (
                      <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                        <XCircle className="w-10 h-10 sm:w-12 sm:h-12 text-red-500 mx-auto mb-3 sm:mb-4" />
                        <p className="text-red-700 font-medium text-sm sm:text-base">Conversion Failed</p>
                        {conversionResult.error_message && (
                          <p className="text-xs sm:text-sm text-red-600 mt-2 sm:mt-3">{conversionResult.error_message}</p>
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
                className="flex items-center px-6 sm:px-8 py-2.5 sm:py-3 bg-gradient-to-r from-purple-500 via-pink-500 to-indigo-600 text-white font-medium rounded-lg sm:rounded-xl hover:from-purple-600 hover:via-pink-600 hover:to-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 shadow-xl hover:shadow-2xl transform hover:scale-105 text-sm sm:text-base"
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



        {activeTab === 'dashboard' && (
          <div className="h-full">
            <Dashboard onLoadCobolCode={handleLoadCobolCode} />
          </div>
        )}
      </main>
    </div>
  )
}

export default App
