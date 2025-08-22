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
  Download,
  ArrowLeft
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
  filename?: string // Added for new layout
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
  total: number // Added for new layout
  completed: number // Added for new layout
  in_progress: number // Added for new layout
  failed: number // Added for new layout
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
  const [searchTerm, setSearchTerm] = useState(''); // Added for new layout
  const [currentPage, setCurrentPage] = useState(1); // Added for new layout
  const [pageSize, setPageSize] = useState(10); // Added for new layout
  const [resultsTab, setResultsTab] = useState('java'); // New state for results tab

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
        return <Clock className="w-5 h-5 text-amber-500 animate-spin" />
      case 'completed':
        return <CheckCircle className="w-5 h-5 text-emerald-500" />
      case 'failed':
        return <XCircle className="w-5 h-5 text-rose-500" />
      default:
        return <AlertCircle className="w-5 h-5 text-slate-500" />
    }
  }

  const getStatusBadge = (status: string) => {
    const baseClasses = "px-3 py-1 rounded-full text-xs font-semibold border-2"
    switch (status) {
      case 'processing':
        return `${baseClasses} bg-gradient-to-r from-amber-100 to-yellow-100 text-amber-800 border-amber-300 shadow-sm`
      case 'completed':
        return `${baseClasses} bg-gradient-to-r from-emerald-100 to-green-100 text-emerald-800 border-emerald-300 shadow-sm`
      case 'failed':
        return `${baseClasses} bg-gradient-to-r from-rose-100 to-red-100 text-rose-800 border-rose-300 shadow-sm`
      default:
        return `${baseClasses} bg-gradient-to-r from-slate-100 to-gray-100 text-slate-800 border-slate-300 shadow-sm`
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

  const handleDeleteConversion = async (conversionId: string) => {
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

  const handleDownloadResults = async (conversionId: string) => {
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

  // New function to filter conversions based on search term and status
  const filteredConversions = dashboardData?.conversions.filter(conversion => {
    const matchesSearchTerm = conversion.conversion_id.toLowerCase().includes(searchTerm.toLowerCase()) ||
                              conversion.filename?.toLowerCase().includes(searchTerm.toLowerCase());
    const matchesStatus = statusFilter === '' || conversion.status === statusFilter;
    return matchesSearchTerm && matchesStatus;
  }) || [];

  if (loading) {
    return (
      <div className="flex items-center justify-center h-96 bg-gradient-to-br from-slate-50 to-blue-50">
        <div className="flex items-center space-x-3 bg-white/80 backdrop-blur-sm rounded-xl p-6 shadow-lg border border-white/20">
          <Activity className="w-8 h-8 text-indigo-500 animate-spin" />
          <span className="text-indigo-700 font-medium text-lg">Loading dashboard...</span>
        </div>
      </div>
    )
  }

  return (
    <div className="w-full h-full bg-gradient-to-br from-slate-50 to-blue-50 p-2 sm:p-4 flex flex-col min-h-0">
      {/* Header */}
      <div className="flex-shrink-0 mb-3 sm:mb-6">
        <h1 className="text-xl sm:text-3xl font-bold bg-gradient-to-r from-indigo-600 to-purple-600 bg-clip-text text-transparent mb-1 sm:mb-2">
          Conversion Dashboard
        </h1>
        <p className="text-xs sm:text-base text-gray-600">Monitor and manage your COBOL to Java conversions</p>
      </div>

      {!showEditor ? (
        <>
          {/* Stats Cards - Responsive */}
          <div className="grid grid-cols-2 lg:grid-cols-4 gap-2 sm:gap-4 mb-3 sm:mb-6 flex-shrink-0">
            <div className="bg-gradient-to-r from-indigo-500 via-purple-500 to-pink-500 rounded-lg sm:rounded-xl p-3 sm:p-6 text-white shadow-lg hover:shadow-xl transition-all duration-300 transform hover:scale-105">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-xs sm:text-sm opacity-90 font-medium">Total</p>
                  <p className="text-lg sm:text-3xl font-bold">{stats?.total || 0}</p>
                </div>
                <BarChart3 className="w-5 h-5 sm:w-8 sm:h-8 opacity-80" />
              </div>
            </div>
            <div className="bg-gradient-to-r from-emerald-500 via-teal-500 to-cyan-500 rounded-lg sm:rounded-xl p-3 sm:p-6 text-white shadow-lg hover:shadow-xl transition-all duration-300 transform hover:scale-105">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-xs sm:text-sm opacity-90 font-medium">Completed</p>
                  <p className="text-lg sm:text-3xl font-bold">{stats?.completed || 0}</p>
                </div>
                <CheckCircle className="w-5 h-5 sm:w-8 sm:h-8 opacity-80" />
              </div>
            </div>
            <div className="bg-gradient-to-r from-amber-500 via-orange-500 to-red-500 rounded-lg sm:rounded-xl p-3 sm:p-6 text-white shadow-lg hover:shadow-xl transition-all duration-300 transform hover:scale-105">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-xs sm:text-sm opacity-90 font-medium">In Progress</p>
                  <p className="text-lg sm:text-3xl font-bold">{stats?.in_progress || 0}</p>
                </div>
                <Clock className="w-5 h-5 sm:w-8 sm:h-8 opacity-80" />
              </div>
            </div>
            <div className="bg-gradient-to-r from-rose-500 via-pink-500 to-purple-500 rounded-lg sm:rounded-xl p-3 sm:p-6 text-white shadow-lg hover:shadow-xl transition-all duration-300 transform hover:scale-105">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-xs sm:text-sm opacity-90 font-medium">Failed</p>
                  <p className="text-lg sm:text-3xl font-bold">{stats?.failed || 0}</p>
                </div>
                <AlertCircle className="w-5 h-5 sm:w-8 sm:h-8 opacity-80" />
              </div>
            </div>
          </div>

          {/* Filters - Responsive */}
          <div className="bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 p-2 sm:p-4 mb-3 sm:mb-6 flex-shrink-0">
            <div className="flex flex-col sm:flex-row gap-3">
              <div className="flex-1">
                <input
                  type="text"
                  placeholder="Search conversions..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-transparent bg-white/90"
                />
              </div>
              <div className="flex gap-2">
                <select
                  value={statusFilter}
                  onChange={(e) => setStatusFilter(e.target.value)}
                  className="px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-transparent bg-white/90"
                >
                  <option value="">All Status</option>
                  <option value="completed">Completed</option>
                  <option value="processing">Processing</option>
                  <option value="failed">Failed</option>
                </select>
                <button
                  onClick={() => { setSearchTerm(''); setStatusFilter(''); }}
                  className="px-4 py-2 bg-gradient-to-r from-gray-500 to-gray-600 text-white rounded-lg hover:from-gray-600 hover:to-gray-700 transition-all duration-200"
                >
                  Clear
                </button>
              </div>
            </div>
          </div>

          {/* Conversions Table */}
          <div className="bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 flex-1 flex flex-col min-h-0">
            <div className="bg-gradient-to-r from-indigo-500 to-purple-600 text-white px-3 sm:px-6 py-2 sm:py-4 rounded-t-lg sm:rounded-t-xl">
              <h2 className="text-base sm:text-xl font-bold">Recent Conversions</h2>
            </div>
            <div className="flex-1 overflow-hidden">
              {loading ? (
                <div className="flex items-center justify-center h-full">
                  <div className="text-center">
                    <Activity className="w-12 h-12 text-indigo-500 mx-auto mb-4 animate-spin" />
                    <p className="text-indigo-600 font-medium">Loading conversions...</p>
                  </div>
                </div>
              ) : filteredConversions.length === 0 ? (
                <div className="flex items-center justify-center h-full">
                  <div className="text-center">
                    <Code className="w-12 h-12 text-gray-400 mx-auto mb-4" />
                    <p className="text-gray-500 font-medium">No conversions found</p>
                    <p className="text-sm text-gray-400 mt-2">Start by converting some COBOL code</p>
                  </div>
                </div>
              ) : (
                <div className="overflow-x-auto">
                  <table className="w-full">
                    <thead className="bg-gray-50">
                      <tr>
                        <th className="px-3 sm:px-6 py-2 sm:py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">File</th>
                        <th className="px-3 sm:px-6 py-2 sm:py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
                        <th className="hidden sm:table-cell px-3 sm:px-6 py-2 sm:py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Created</th>
                        <th className="px-3 sm:px-6 py-2 sm:py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
                      </tr>
                    </thead>
                    <tbody className="bg-white divide-y divide-gray-200">
                      {filteredConversions.map((conversion) => (
                        <tr key={conversion.id} className="hover:bg-gray-50 transition-colors">
                          <td className="px-3 sm:px-6 py-2 sm:py-4 whitespace-nowrap">
                            <div className="text-xs sm:text-sm font-medium text-gray-900">{conversion.filename}</div>
                            <div className="text-xs text-gray-500">{conversion.conversion_id}</div>
                          </td>
                          <td className="px-3 sm:px-6 py-2 sm:py-4 whitespace-nowrap">
                            <span className={`inline-flex items-center px-2 py-1 rounded-full text-xs font-medium ${
                              conversion.status === 'completed' 
                                ? 'bg-gradient-to-r from-emerald-100 to-teal-100 text-emerald-800 border border-emerald-200 shadow-sm' 
                                : conversion.status === 'processing' 
                                ? 'bg-gradient-to-r from-amber-100 to-orange-100 text-amber-800 border border-amber-200 shadow-sm' 
                                : 'bg-gradient-to-r from-rose-100 to-pink-100 text-rose-800 border border-rose-200 shadow-sm'
                            }`}>
                              {conversion.status === 'completed' && <CheckCircle className="w-3 h-3 mr-1" />}
                              {conversion.status === 'processing' && <Clock className="w-3 h-3 mr-1" />}
                              {conversion.status === 'failed' && <AlertCircle className="w-3 h-3 mr-1" />}
                              <span className="hidden sm:inline">{conversion.status.replace('_', ' ')}</span>
                              <span className="sm:hidden">{conversion.status === 'completed' ? 'Done' : conversion.status === 'processing' ? 'Proc' : 'Fail'}</span>
                            </span>
                          </td>
                          <td className="hidden sm:table-cell px-3 sm:px-6 py-2 sm:py-4 whitespace-nowrap text-xs sm:text-sm text-gray-500">
                            {new Date(conversion.created_at).toLocaleDateString()}
                          </td>
                          <td className="px-3 sm:px-6 py-2 sm:py-4 whitespace-nowrap text-xs sm:text-sm font-medium">
                            <button
                              onClick={() => handleViewConversion(conversion)}
                              className="text-indigo-600 hover:text-indigo-900 mr-2 sm:mr-3 flex items-center"
                            >
                              <Code className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />
                              <span className="hidden sm:inline">View</span>
                              <span className="sm:hidden">V</span>
                            </button>
                            <button
                              onClick={() => handleDeleteConversion(conversion.conversion_id)}
                              className="text-rose-600 hover:text-rose-900 flex items-center"
                            >
                              <Trash2 className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />
                              <span className="hidden sm:inline">Delete</span>
                              <span className="sm:hidden">D</span>
                            </button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              )}
            </div>
          </div>

          {/* Pagination - Responsive */}
          {filteredConversions.length > 0 && (
            <div className="flex flex-col sm:flex-row justify-between items-center mt-3 sm:mt-4 gap-2 sm:gap-0 flex-shrink-0">
              <div className="text-xs sm:text-sm text-gray-700 text-center sm:text-left">
                Showing {((currentPage - 1) * pageSize) + 1} to {Math.min(currentPage * pageSize, filteredConversions.length)} of {filteredConversions.length} results
              </div>
              <div className="flex space-x-1 sm:space-x-2">
                <button
                  onClick={() => setCurrentPage(prev => Math.max(1, prev - 1))}
                  disabled={currentPage === 1}
                  className="px-2 sm:px-3 py-1 border border-gray-300 rounded text-xs sm:text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
                >
                  <span className="hidden sm:inline">Previous</span>
                  <span className="sm:hidden">Prev</span>
                </button>
                <span className="px-2 sm:px-3 py-1 text-xs sm:text-sm text-gray-700">
                  Page {currentPage} of {Math.ceil(filteredConversions.length / pageSize)}
                </span>
                <button
                  onClick={() => setCurrentPage(prev => Math.min(Math.ceil(filteredConversions.length / pageSize), prev + 1))}
                  disabled={currentPage >= Math.ceil(filteredConversions.length / pageSize)}
                  className="px-2 sm:px-3 py-1 border border-gray-300 rounded text-xs sm:text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
                >
                  <span className="hidden sm:inline">Next</span>
                  <span className="sm:hidden">Next</span>
                </button>
              </div>
            </div>
          )}
        </>
      ) : selectedConversion && (
        <div className="h-full flex flex-col min-h-0">
          {/* Editor Header */}
          <div className="flex-shrink-0 bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 p-2 sm:p-4 mb-2 sm:mb-4">
            <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2 sm:gap-0">
              <div className="flex items-center space-x-2 sm:space-x-4">
                <button
                  onClick={() => setShowEditor(false)}
                  className="flex items-center text-indigo-600 hover:text-indigo-800 transition-colors text-sm"
                >
                  <ArrowLeft className="w-4 h-4 sm:w-5 sm:h-5 mr-1 sm:mr-2" />
                  <span className="hidden sm:inline">Back to Dashboard</span>
                  <span className="sm:hidden">Back</span>
                </button>
                <div>
                  <h2 className="text-base sm:text-xl font-bold text-gray-900">{selectedConversion.filename}</h2>
                  <p className="text-xs sm:text-sm text-gray-500">ID: {selectedConversion.conversion_id}</p>
                </div>
              </div>
              {selectedConversion.status === 'completed' && (
                <button
                  onClick={() => handleDownloadResults(selectedConversion.conversion_id)}
                  className="flex items-center px-2 sm:px-4 py-1 sm:py-2 bg-gradient-to-r from-emerald-500 to-teal-600 text-white rounded sm:rounded-lg hover:from-emerald-600 hover:to-teal-700 transition-all duration-200 shadow-lg hover:shadow-xl text-xs sm:text-sm self-start sm:self-auto"
                >
                  <Download className="w-3 h-3 sm:w-4 sm:h-4 mr-1 sm:mr-2" />
                  <span className="hidden sm:inline">Download Results</span>
                  <span className="sm:hidden">Download</span>
                </button>
              )}
            </div>
          </div>

          {/* COBOL Code and Results - Two Column Layout with Tabs */}
          <div className="flex-1 flex flex-col lg:flex-row gap-3 sm:gap-4 min-h-0">
            {/* Left Column - COBOL Code */}
            <div className="flex-1 flex flex-col min-h-0">
              <div className="bg-white/80 backdrop-blur-sm rounded-lg sm:rounded-xl shadow-lg border border-white/20 p-3 sm:p-4 flex-1 flex flex-col min-h-0">
                <h3 className="text-sm sm:text-lg font-bold text-gray-900 mb-3 sm:mb-4 flex items-center flex-shrink-0">
                  <Code className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-indigo-600" />
                  <span className="hidden sm:inline">Original COBOL Code</span>
                  <span className="sm:hidden">COBOL Code</span>
                </h3>
                <div className="flex-1 min-h-0 overflow-y-auto">
                  <div className="prose prose-sm max-w-none">
                    <h3 className="text-lg font-bold text-gray-900 mb-3">Original COBOL Code</h3>
                    <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                      <pre className="text-sm text-gray-900 whitespace-pre-wrap font-mono bg-white">
                        {detailedConversion?.cobol_code || selectedConversion.cobol_code || 'No COBOL code available'}
                      </pre>
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
                    <h3 className="text-sm sm:text-lg font-bold text-gray-900 flex items-center">
                      <FileCode className="w-4 h-4 sm:w-5 sm:h-5 mr-1.5 sm:mr-2 text-emerald-600" />
                      <span className="hidden sm:inline">Conversion Results</span>
                      <span className="sm:hidden">Results</span>
                    </h3>
                    {selectedConversion.status === 'completed' && (
                      <button
                        onClick={() => handleDownloadResults(selectedConversion.conversion_id)}
                        className="flex items-center px-2 sm:px-3 py-1 sm:py-1.5 bg-gradient-to-r from-emerald-500 to-teal-600 text-white rounded text-xs sm:text-sm hover:from-emerald-600 hover:to-teal-700 transition-all duration-200 shadow-lg hover:shadow-xl"
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
                  {loadingDetails ? (
                    <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                      <Activity className="w-10 h-10 sm:w-12 sm:h-12 text-indigo-500 mx-auto mb-3 sm:mb-4 animate-spin" />
                      <p className="text-indigo-600 font-medium text-sm sm:text-base">Loading conversion details...</p>
                      <p className="text-xs sm:text-sm text-gray-500 mt-2 sm:mt-3">Fetching data from database</p>
                    </div>
                  ) : selectedConversion.status === 'completed' && detailedConversion ? (
                    <div className="h-full">
                      {resultsTab === 'java' && (
                        <div className="h-full overflow-y-auto">
                          <div className="prose prose-sm max-w-none">
                            <h3 className="text-lg font-bold text-gray-900 mb-3">Java Code</h3>
                            <div className="bg-white rounded-lg p-4 mb-4 border border-gray-200">
                              <pre className="text-sm text-gray-900 whitespace-pre-wrap font-mono bg-white">
                                {detailedConversion.final_java_code || 'No Java code available'}
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
                                {detailedConversion.summary || 'No summary available'}
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
                                {detailedConversion.pseudo_code || 'No pseudo code available'}
                              </pre>
                            </div>
                          </div>
                        </div>
                      )}
                    </div>
                  ) : selectedConversion.status === 'failed' ? (
                    <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                      <XCircle className="w-10 h-10 sm:w-12 sm:h-12 text-rose-500 mx-auto mb-3 sm:mb-4" />
                      <p className="text-rose-600 font-medium text-sm sm:text-base">Conversion Failed</p>
                      {(detailedConversion?.error_message || selectedConversion.error_message) && (
                        <p className="text-xs sm:text-sm text-rose-500 mt-2 sm:mt-3">{detailedConversion?.error_message || selectedConversion.error_message}</p>
                      )}
                    </div>
                  ) : (
                    <div className="text-center py-6 sm:py-8 h-full flex flex-col items-center justify-center">
                      <Clock className="w-10 h-10 sm:w-12 sm:h-12 text-amber-500 mx-auto mb-3 sm:mb-4 animate-spin" />
                      <p className="text-amber-600 font-medium text-sm sm:text-base">Processing...</p>
                      <p className="text-xs sm:text-sm text-gray-500 mt-2 sm:mt-3">This conversion is still in progress</p>
                    </div>
                  )}
                </div>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}

export default Dashboard
