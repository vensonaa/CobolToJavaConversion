import React, { useState, useEffect } from 'react'
import { 
  Calendar, 
  Code, 
  Clock, 
  CheckCircle, 
  XCircle, 
  Eye,
  Filter,
  ChevronLeft,
  ChevronRight,
  BarChart3,
  Activity,
  FileCode,
  AlertCircle,
  Trash2,
  Download
} from 'lucide-react'
import { deleteConversion, getConversionDetails, downloadResults } from '../api/conversionApi'
import CodeEditor from './CodeEditor'
import ResultsViewer from './ResultsViewer'

interface ConversionItem {
  id: number
  conversion_id: string
  status: 'pending' | 'processing' | 'completed' | 'failed'
  created_at: string
  updated_at: string
  completed_at?: string
  cobol_code?: string
  prior_knowledge?: string
  total_chunks: number
  progress: number
  java_files_count: number
  java_files: string[]
  error_message?: string
}

interface DashboardData {
  conversions: ConversionItem[]
  total: number
  page: number
  limit: number
  total_pages: number
}

interface Stats {
  total_conversions: number
  successful_conversions: number
  failed_conversions: number
  total_java_files: number
}

const Dashboard: React.FC = () => {
  const [dashboardData, setDashboardData] = useState<DashboardData | null>(null)
  const [stats, setStats] = useState<Stats | null>(null)
  const [loading, setLoading] = useState(true)
  const [page, setPage] = useState(1)
  const [statusFilter, setStatusFilter] = useState<string>('')
  const [selectedConversion, setSelectedConversion] = useState<ConversionItem | null>(null)
  const [detailedConversion, setDetailedConversion] = useState<any>(null)
  const [showEditor, setShowEditor] = useState(false)
  const [loadingDetails, setLoadingDetails] = useState(false)

  const fetchDashboardData = async () => {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        limit: '10'
      })
      
      if (statusFilter) {
        params.append('status', statusFilter)
      }

      const response = await fetch(`/api/dashboard?${params}`)
      const data = await response.json()
      setDashboardData(data)
    } catch (error) {
      console.error('Failed to fetch dashboard data:', error)
    }
  }

  const fetchStats = async () => {
    try {
      const response = await fetch('/api/dashboard/stats')
      const data = await response.json()
      setStats(data)
    } catch (error) {
      console.error('Failed to fetch stats:', error)
    }
  }

  useEffect(() => {
    const loadData = async () => {
      setLoading(true)
      await Promise.all([fetchDashboardData(), fetchStats()])
      setLoading(false)
    }
    loadData()
  }, [page, statusFilter])

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'processing':
        return <Clock className="w-5 h-5 text-yellow-500 animate-spin" />
      case 'completed':
        return <CheckCircle className="w-5 h-5 text-green-500" />
      case 'failed':
        return <XCircle className="w-5 h-5 text-red-500" />
      default:
        return <AlertCircle className="w-5 h-5 text-gray-500" />
    }
  }

  const getStatusBadge = (status: string) => {
    const baseClasses = "px-3 py-1 rounded-full text-xs font-semibold"
    switch (status) {
      case 'processing':
        return `${baseClasses} bg-yellow-100 text-yellow-800 border border-yellow-200`
      case 'completed':
        return `${baseClasses} bg-green-100 text-green-800 border border-green-200`
      case 'failed':
        return `${baseClasses} bg-red-100 text-red-800 border border-red-200`
      default:
        return `${baseClasses} bg-gray-100 text-gray-800 border border-gray-200`
    }
  }

  const handleViewConversion = async (conversion: ConversionItem) => {
    setSelectedConversion(conversion)
    setShowEditor(true)
    setLoadingDetails(true)
    
    try {
      const detailedData = await getConversionDetails(conversion.conversion_id)
      console.log('Detailed conversion data:', detailedData)
      setDetailedConversion(detailedData)
    } catch (error) {
      console.error('Failed to load conversion details:', error)
      // Fallback to the basic conversion data if detailed fetch fails
      setDetailedConversion(conversion)
    } finally {
      setLoadingDetails(false)
    }
  }

  const handleDelete = async (conversionId: string) => {
    if (!confirm('Are you sure you want to delete this conversion? This action cannot be undone.')) {
      return
    }
    
    try {
      await deleteConversion(conversionId)
      // Refresh the dashboard data
      await Promise.all([fetchDashboardData(), fetchStats()])
    } catch (error) {
      console.error('Delete failed:', error)
      alert('Delete failed. Please try again.')
    }
  }

  const handleDownload = async (conversionId: string) => {
    try {
      await downloadResults(conversionId)
    } catch (error) {
      console.error('Download failed:', error)
      alert('Download failed. Please try again.')
    }
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleString()
  }

  const formatDuration = (start: string, end?: string) => {
    const startTime = new Date(start)
    const endTime = end ? new Date(end) : new Date()
    const diff = endTime.getTime() - startTime.getTime()
    const minutes = Math.floor(diff / 60000)
    const seconds = Math.floor((diff % 60000) / 1000)
    return `${minutes}m ${seconds}s`
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-96">
        <div className="flex items-center space-x-2">
          <Activity className="w-6 h-6 text-blue-500 animate-spin" />
          <span className="text-gray-600">Loading dashboard...</span>
        </div>
      </div>
    )
  }

  return (
    <div className="w-full h-full flex flex-col">
      <div className="px-4 sm:px-6 lg:px-8 py-4 sm:py-8 space-y-6">
        {!showEditor && (
          <>
            {/* Stats Cards */}
            {stats && (
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 sm:gap-6">
                <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-xl p-6 text-white shadow-lg">
                  <div className="flex items-center justify-between">
                    <div>
                      <p className="text-blue-100 text-sm font-medium">Total Conversions</p>
                      <p className="text-3xl font-bold">{stats.total_conversions}</p>
                    </div>
                    <BarChart3 className="w-8 h-8 text-blue-200" />
                  </div>
                </div>
                
                <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-xl p-6 text-white shadow-lg">
                  <div className="flex items-center justify-between">
                    <div>
                      <p className="text-green-100 text-sm font-medium">Successful</p>
                      <p className="text-3xl font-bold">{stats.successful_conversions}</p>
                    </div>
                    <CheckCircle className="w-8 h-8 text-green-200" />
                  </div>
                </div>
                
                <div className="bg-gradient-to-br from-red-500 to-red-600 rounded-xl p-6 text-white shadow-lg">
                  <div className="flex items-center justify-between">
                    <div>
                      <p className="text-red-100 text-sm font-medium">Failed</p>
                      <p className="text-3xl font-bold">{stats.failed_conversions}</p>
                    </div>
                    <XCircle className="w-8 h-8 text-red-200" />
                  </div>
                </div>
                
                <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-xl p-6 text-white shadow-lg">
                  <div className="flex items-center justify-between">
                    <div>
                      <p className="text-purple-100 text-sm font-medium">Java Files</p>
                      <p className="text-3xl font-bold">{stats.total_java_files}</p>
                    </div>
                    <FileCode className="w-8 h-8 text-purple-200" />
                  </div>
                </div>
              </div>
            )}

            {/* Filters */}
            <div className="bg-white rounded-lg shadow-sm border p-4">
              <div className="flex flex-col sm:flex-row gap-4 items-start sm:items-center justify-between">
                <h2 className="text-lg font-semibold text-gray-900 flex items-center">
                  <Activity className="w-5 h-5 mr-2 text-blue-500" />
                  Conversion History
                </h2>
                
                <div className="flex items-center space-x-3">
                  <Filter className="w-4 h-4 text-gray-400" />
                  <select
                    value={statusFilter}
                    onChange={(e) => {
                      setStatusFilter(e.target.value)
                      setPage(1)
                    }}
                    className="px-3 py-2 border border-gray-300 rounded-md text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                  >
                    <option value="">All Status</option>
                    <option value="processing">Processing</option>
                    <option value="completed">Completed</option>
                    <option value="failed">Failed</option>
                  </select>
                </div>
              </div>
            </div>

            {/* Conversions Table */}
            {dashboardData && (
              <div className="bg-white rounded-lg shadow-sm border overflow-hidden">
                <div className="overflow-x-auto">
                  <table className="min-w-full divide-y divide-gray-200">
                    <thead className="bg-gray-50">
                      <tr>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Status
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Created
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Code Size
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Java Files
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Duration
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          Actions
                        </th>
                      </tr>
                    </thead>
                    <tbody className="bg-white divide-y divide-gray-200">
                      {dashboardData.conversions.map((conversion) => (
                        <tr key={conversion.id} className="hover:bg-gray-50">
                          <td className="px-6 py-4 whitespace-nowrap">
                            <div className="flex items-center space-x-2">
                              {getStatusIcon(conversion.status)}
                              <span className={getStatusBadge(conversion.status)}>
                                {conversion.status}
                              </span>
                            </div>
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            <div className="flex items-center space-x-2">
                              <Calendar className="w-4 h-4 text-gray-400" />
                              <span>{formatDate(conversion.created_at)}</span>
                            </div>
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            <div className="flex items-center space-x-2">
                              <Code className="w-4 h-4 text-gray-400" />
                              <span>{conversion.cobol_code ? conversion.cobol_code.length.toLocaleString() : 0} chars</span>
                            </div>
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            <div className="flex items-center space-x-2">
                              <FileCode className="w-4 h-4 text-gray-400" />
                              <span>{conversion.java_files_count}</span>
                            </div>
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                            {formatDuration(conversion.created_at, conversion.completed_at)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm space-x-2">
                            <button
                              onClick={() => handleViewConversion(conversion)}
                              className="inline-flex items-center px-3 py-1 border border-transparent text-xs font-medium rounded-md text-blue-700 bg-blue-100 hover:bg-blue-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                            >
                              <Eye className="w-3 h-3 mr-1" />
                              View
                            </button>
                            <button
                              onClick={() => handleDelete(conversion.conversion_id)}
                              className="inline-flex items-center px-3 py-1 border border-transparent text-xs font-medium rounded-md text-red-700 bg-red-100 hover:bg-red-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500"
                            >
                              <Trash2 className="w-3 h-3 mr-1" />
                              Delete
                            </button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>

                {/* Pagination */}
                {dashboardData.total_pages > 1 && (
                  <div className="bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6">
                    <div className="flex-1 flex justify-between sm:hidden">
                      <button
                        onClick={() => setPage(Math.max(1, page - 1))}
                        disabled={page === 1}
                        className="relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        Previous
                      </button>
                      <button
                        onClick={() => setPage(Math.min(dashboardData.total_pages, page + 1))}
                        disabled={page === dashboardData.total_pages}
                        className="ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        Next
                      </button>
                    </div>
                    
                    <div className="hidden sm:flex-1 sm:flex sm:items-center sm:justify-between">
                      <div>
                        <p className="text-sm text-gray-700">
                          Showing{' '}
                          <span className="font-medium">{(page - 1) * dashboardData.limit + 1}</span>{' '}
                          to{' '}
                          <span className="font-medium">
                            {Math.min(page * dashboardData.limit, dashboardData.total)}
                          </span>{' '}
                          of <span className="font-medium">{dashboardData.total}</span> results
                        </p>
                      </div>
                      <div>
                        <nav className="relative z-0 inline-flex rounded-md shadow-sm -space-x-px">
                          <button
                            onClick={() => setPage(Math.max(1, page - 1))}
                            disabled={page === 1}
                            className="relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                          >
                            <ChevronLeft className="w-4 h-4" />
                          </button>
                          
                          {Array.from({ length: dashboardData.total_pages }, (_, i) => i + 1).map((pageNum) => (
                            <button
                              key={pageNum}
                              onClick={() => setPage(pageNum)}
                              className={`relative inline-flex items-center px-4 py-2 border text-sm font-medium ${
                                pageNum === page
                                  ? 'z-10 bg-blue-50 border-blue-500 text-blue-600'
                                  : 'bg-white border-gray-300 text-gray-500 hover:bg-gray-50'
                              }`}
                            >
                              {pageNum}
                            </button>
                          ))}
                          
                          <button
                            onClick={() => setPage(Math.min(dashboardData.total_pages, page + 1))}
                            disabled={page === dashboardData.total_pages}
                            className="relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                          >
                            <ChevronRight className="w-4 h-4" />
                          </button>
                        </nav>
                      </div>
                    </div>
                  </div>
                )}
              </div>
            )}
          </>
        )}

        {showEditor && selectedConversion && (
          <div className="space-y-6">
            {/* Back Button */}
            <div className="flex items-center justify-between">
              <button
                onClick={() => {
                  setShowEditor(false)
                  setSelectedConversion(null)
                  setDetailedConversion(null)
                }}
                className="inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
              >
                <ChevronLeft className="w-4 h-4 mr-2" />
                Back to Dashboard
              </button>
              <div className="flex items-center space-x-2">
                <span className={getStatusBadge(selectedConversion.status)}>
                  {selectedConversion.status}
                </span>
                {selectedConversion.status === 'completed' && (
                  <button
                    onClick={() => handleDownload(selectedConversion.conversion_id)}
                    className="inline-flex items-center px-3 py-2 border border-transparent text-sm font-medium rounded-md text-green-700 bg-green-100 hover:bg-green-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-green-500"
                  >
                    <Download className="w-4 h-4 mr-2" />
                    Download
                  </button>
                )}
                <button
                  onClick={() => handleDelete(selectedConversion.conversion_id)}
                  className="inline-flex items-center px-3 py-2 border border-transparent text-sm font-medium rounded-md text-red-700 bg-red-100 hover:bg-red-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500"
                >
                  <Trash2 className="w-4 h-4 mr-2" />
                  Delete
                </button>
              </div>
            </div>

            {/* Conversion Info */}
            <div className="bg-white rounded-lg shadow-sm border p-4">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm">
                <div>
                  <label className="text-gray-500">Created</label>
                  <p className="font-medium text-gray-900">{formatDate(selectedConversion.created_at)}</p>
                </div>
                <div>
                  <label className="text-gray-500">COBOL Code Size</label>
                  <p className="font-medium text-gray-900">
                    {detailedConversion?.cobol_code ? detailedConversion.cobol_code.length.toLocaleString() : 
                     selectedConversion.cobol_code ? selectedConversion.cobol_code.length.toLocaleString() : 0} characters
                  </p>
                </div>
                <div>
                  <label className="text-gray-500">Java Files</label>
                  <p className="font-medium text-gray-900">{detailedConversion?.java_files_count || selectedConversion.java_files_count}</p>
                </div>
              </div>
              {(detailedConversion?.prior_knowledge || selectedConversion.prior_knowledge) && (
                <div className="mt-4">
                  <label className="text-gray-500">Prior Knowledge</label>
                  <p className="text-sm text-gray-900 mt-1">{detailedConversion?.prior_knowledge || selectedConversion.prior_knowledge}</p>
                </div>
              )}
            </div>

            {/* COBOL Code - Full Width */}
            <div className="space-y-4">
              <div className="bg-white rounded-lg shadow-sm border p-6">
                <h3 className="text-xl font-semibold text-gray-900 mb-6">Original COBOL Code</h3>
                <CodeEditor
                  value={detailedConversion?.cobol_code || selectedConversion.cobol_code || ''}
                  onChange={() => {}} // Read-only
                  language="cobol"
                  placeholder="No COBOL code available"
                  height="h-96 sm:h-[32rem]"
                />
              </div>
            </div>

            {/* Conversion Results - Full Width */}
            <div className="space-y-4">
              <div className="bg-white rounded-lg shadow-sm border p-6">
                <h3 className="text-xl font-semibold text-gray-900 mb-6">Conversion Results</h3>
                {loadingDetails ? (
                  <div className="text-center py-8">
                    <Activity className="w-12 h-12 text-blue-500 mx-auto mb-4 animate-spin" />
                    <p className="text-blue-600 font-medium">Loading conversion details...</p>
                    <p className="text-sm text-gray-500 mt-2">Fetching data from database</p>
                  </div>
                ) : selectedConversion.status === 'completed' && detailedConversion ? (
                  <>
                    {/* Debug info */}
                    <div className="mb-4 p-4 bg-gray-100 rounded text-xs">
                      <strong>Debug Info:</strong><br/>
                      Status: {detailedConversion.status}<br/>
                      Has final_java_code: {detailedConversion.final_java_code ? 'Yes' : 'No'}<br/>
                      Java code length: {detailedConversion.final_java_code?.length || 0}<br/>
                      Has pseudo_code: {detailedConversion.pseudo_code ? 'Yes' : 'No'}<br/>
                      Has summary: {detailedConversion.summary ? 'Yes' : 'No'}
                    </div>
                    <ResultsViewer result={detailedConversion} />
                  </>
                ) : selectedConversion.status === 'completed' && selectedConversion.result ? (
                  <ResultsViewer result={selectedConversion.result} />
                ) : selectedConversion.status === 'failed' ? (
                  <div className="text-center py-8">
                    <XCircle className="w-12 h-12 text-red-500 mx-auto mb-4" />
                    <p className="text-red-600 font-medium">Conversion Failed</p>
                    {(detailedConversion?.error_message || selectedConversion.error_message) && (
                      <p className="text-sm text-red-500 mt-2">{detailedConversion?.error_message || selectedConversion.error_message}</p>
                    )}
                  </div>
                ) : (
                  <div className="text-center py-8">
                    <Clock className="w-12 h-12 text-yellow-500 mx-auto mb-4 animate-spin" />
                    <p className="text-yellow-600 font-medium">Processing...</p>
                    <p className="text-sm text-gray-500 mt-2">This conversion is still in progress</p>
                  </div>
                )}
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

export default Dashboard
