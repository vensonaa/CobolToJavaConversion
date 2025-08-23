import React, { useState, useEffect } from 'react'
import { 
  Calendar, 
  Code, 
  Clock, 
  CheckCircle, 
  XCircle, 
  Filter,
  ChevronLeft,
  ChevronRight,
  BarChart3,
  Activity,
  FileCode,
  AlertCircle,
  Trash2,
  Download,
  ArrowLeft,
  Upload
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
  total_java_classes: number
  total_files_uploaded: number
  total: number // Added for new layout
  completed: number // Added for new layout
  in_progress: number // Added for new layout
  failed: number // Added for new layout
}

interface DashboardProps {
  onLoadCobolCode?: (cobolCode: string, conversionResult?: any) => void
}

const Dashboard: React.FC<DashboardProps> = ({ onLoadCobolCode }) => {
  const [dashboardData, setDashboardData] = useState<DashboardData | null>(null)
  const [stats, setStats] = useState<Stats | null>(null)
  const [loading, setLoading] = useState(true)
  const [page, setPage] = useState(1)
  const [statusFilter, setStatusFilter] = useState<string>('')
  const [searchTerm, setSearchTerm] = useState(''); // Added for new layout
  const [currentPage, setCurrentPage] = useState(1); // Added for new layout
  const [pageSize, setPageSize] = useState(10); // Added for new layout

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



  const handleLoadCobolCode = async (conversion: ConversionItem) => {
    if (!onLoadCobolCode) return
    
    try {
      // Always fetch the detailed data to get both COBOL code and conversion results
      const detailedData = await getConversionDetails(conversion.conversion_id)
      
      if (detailedData.cobol_code) {
        // Pass both the COBOL code and the conversion results
        onLoadCobolCode(detailedData.cobol_code, detailedData)
      } else {
        alert('No COBOL code found for this conversion.')
      }
    } catch (error) {
      console.error('Failed to load conversion data:', error)
      alert('Failed to load conversion data. Please try again.')
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
    <div className="w-full h-full bg-gradient-to-br from-purple-50 via-pink-50 to-indigo-50 overflow-y-auto">
      {/* Stats Cards */}
      <div className="grid grid-cols-2 lg:grid-cols-3 xl:grid-cols-6 gap-3 sm:gap-4 p-3 sm:p-4">
        <div className="bg-gradient-to-br from-blue-500 to-cyan-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Total Conversions</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.total || 0}</p>
            </div>
            <BarChart3 className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>
        
        <div className="bg-gradient-to-br from-green-500 to-emerald-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Completed</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.completed || 0}</p>
            </div>
            <CheckCircle className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>
        
        <div className="bg-gradient-to-br from-yellow-500 to-orange-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Processing</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.in_progress || 0}</p>
            </div>
            <Clock className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>
        
        <div className="bg-gradient-to-br from-red-500 to-rose-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Failed</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.failed || 0}</p>
            </div>
            <XCircle className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>

        <div className="bg-gradient-to-br from-purple-500 to-indigo-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Java Classes</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.total_java_classes || 0}</p>
            </div>
            <FileCode className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>

        <div className="bg-gradient-to-br from-teal-500 to-cyan-600 text-white rounded-xl sm:rounded-2xl p-3 sm:p-4 shadow-xl hover:shadow-2xl transition-all duration-200 transform hover:scale-105">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-xs sm:text-sm opacity-90">Files Uploaded</p>
              <p className="text-lg sm:text-2xl font-bold">{stats?.total_files_uploaded || 0}</p>
            </div>
            <Upload className="w-6 h-6 sm:w-8 sm:h-8 opacity-80" />
          </div>
        </div>
      </div>

      {/* Filters and Search */}
      <div className="bg-gradient-to-br from-white/90 to-purple-50/90 backdrop-blur-sm rounded-xl sm:rounded-2xl shadow-xl border-2 border-purple-200 p-3 sm:p-4 mx-3 sm:mx-4 mb-3 sm:mb-4">
        <div className="flex flex-col sm:flex-row gap-3 sm:gap-4">
          <div className="flex-1">
            <input
              type="text"
              placeholder="Search conversions..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full px-3 sm:px-4 py-2 sm:py-2.5 bg-white/80 border-2 border-purple-200 rounded-lg sm:rounded-xl focus:border-purple-400 focus:outline-none focus:ring-2 focus:ring-purple-200 text-sm sm:text-base"
            />
          </div>
          <select
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            className="px-3 sm:px-4 py-2 sm:py-2.5 bg-white/80 border-2 border-purple-200 rounded-lg sm:rounded-xl focus:border-purple-400 focus:outline-none focus:ring-2 focus:ring-purple-200 text-sm sm:text-base"
          >
            <option value="">All Status</option>
            <option value="processing">Processing</option>
            <option value="completed">Completed</option>
            <option value="failed">Failed</option>
          </select>
        </div>
      </div>

      {/* Conversions Table */}
      <div className="bg-gradient-to-br from-white/90 to-purple-50/90 backdrop-blur-sm rounded-xl sm:rounded-2xl shadow-xl border-2 border-purple-200 mx-3 sm:mx-4 mb-3 sm:mb-4 overflow-hidden">
        <div className="bg-gradient-to-r from-purple-600 via-pink-600 to-indigo-600 text-white px-3 sm:px-4 py-2 sm:py-3">
          <h2 className="text-sm sm:text-lg font-bold">Conversion History</h2>
        </div>
        
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead className="bg-gradient-to-r from-purple-100 to-pink-100">
              <tr>
                <th className="px-3 sm:px-4 py-2 sm:py-3 text-left text-xs sm:text-sm font-semibold text-purple-900">ID</th>
                <th className="px-3 sm:px-4 py-2 sm:py-3 text-left text-xs sm:text-sm font-semibold text-purple-900">Status</th>
                <th className="hidden md:table-cell px-3 sm:px-4 py-2 sm:py-3 text-left text-xs sm:text-sm font-semibold text-purple-900">Java Classes</th>
                <th className="hidden lg:table-cell px-3 sm:px-4 py-2 sm:py-3 text-left text-xs sm:text-sm font-semibold text-purple-900">Created</th>
                <th className="px-3 sm:px-4 py-2 sm:py-3 text-left text-xs sm:text-sm font-semibold text-purple-900">Actions</th>
              </tr>
            </thead>
            <tbody>
              {filteredConversions.map((conversion) => (
                <tr key={conversion.conversion_id} className="border-b border-purple-100 hover:bg-gradient-to-r hover:from-purple-50 hover:to-pink-50 transition-all duration-200">
                  <td className="px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm text-purple-800 font-mono">
                    {conversion.conversion_id.slice(0, 8)}...
                  </td>
                  <td className="px-3 sm:px-4 py-2 sm:py-3">
                    <span className={`inline-flex items-center px-2 sm:px-3 py-1 sm:py-1.5 rounded-full text-xs sm:text-sm font-medium ${
                      conversion.status === 'completed'
                        ? 'bg-gradient-to-r from-green-100 to-emerald-100 text-green-800 border border-green-200'
                        : conversion.status === 'processing'
                        ? 'bg-gradient-to-r from-yellow-100 to-orange-100 text-yellow-800 border border-yellow-200'
                        : 'bg-gradient-to-r from-red-100 to-rose-100 text-red-800 border border-red-200'
                    }`}>
                      {conversion.status === 'completed' && <CheckCircle className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />}
                      {conversion.status === 'processing' && <Clock className="w-3 h-3 sm:w-4 sm:h-4 mr-1 animate-spin" />}
                      {conversion.status === 'failed' && <XCircle className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />}
                      <span className="hidden sm:inline">{conversion.status}</span>
                      <span className="sm:hidden">{conversion.status.slice(0, 3)}</span>
                    </span>
                  </td>
                  <td className="hidden md:table-cell px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm text-purple-700">
                    <div className="flex items-center">
                      <FileCode className="w-3 h-3 sm:w-4 sm:h-4 mr-1 text-purple-600" />
                      <span className="font-medium">{conversion.java_files_count || 0}</span>
                    </div>
                  </td>
                  <td className="hidden lg:table-cell px-3 sm:px-4 py-2 sm:py-3 text-xs sm:text-sm text-purple-700">
                    {new Date(conversion.created_at).toLocaleDateString()}
                  </td>
                  <td className="px-3 sm:px-4 py-2 sm:py-3">
                    <div className="flex gap-1 sm:gap-2">
                      <button
                        onClick={() => handleLoadCobolCode(conversion)}
                        className="flex items-center px-2 sm:px-3 py-1 sm:py-1.5 bg-gradient-to-r from-green-500 to-emerald-600 text-white text-xs sm:text-sm rounded sm:rounded-lg hover:from-green-600 hover:to-emerald-700 transition-all duration-200 shadow-md hover:shadow-lg transform hover:scale-105"
                        title="Load COBOL code and results into editor"
                      >
                        <Code className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />
                        <span className="hidden sm:inline">Load All</span>
                        <span className="sm:hidden">LA</span>
                      </button>

                      <button
                        onClick={() => handleDeleteConversion(conversion.conversion_id)}
                        className="flex items-center px-2 sm:px-3 py-1 sm:py-1.5 bg-gradient-to-r from-red-500 to-rose-600 text-white text-xs sm:text-sm rounded sm:rounded-lg hover:from-red-600 hover:to-rose-700 transition-all duration-200 shadow-md hover:shadow-lg transform hover:scale-105"
                      >
                        <Trash2 className="w-3 h-3 sm:w-4 sm:h-4 mr-1" />
                        <span className="hidden sm:inline">Delete</span>
                        <span className="sm:hidden">D</span>
                      </button>
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        
        {filteredConversions.length === 0 && (
          <div className="text-center py-8 sm:py-12">
            <BarChart3 className="w-12 h-12 sm:w-16 sm:h-16 text-purple-400 mx-auto mb-3 sm:mb-4" />
            <p className="text-purple-700 font-medium text-sm sm:text-base">No conversions found</p>
            <p className="text-purple-600 text-xs sm:text-sm mt-1">Try adjusting your search or filters</p>
          </div>
        )}
      </div>

      {/* Pagination */}
      <div className="bg-gradient-to-br from-white/90 to-purple-50/90 backdrop-blur-sm rounded-xl sm:rounded-2xl shadow-xl border-2 border-purple-200 p-3 sm:p-4 mx-3 sm:mx-4 mb-3 sm:mb-4">
        <div className="flex flex-col sm:flex-row items-center justify-between gap-3 sm:gap-0">
          <div className="text-sm sm:text-base text-purple-700">
            Showing {((currentPage - 1) * pageSize) + 1} to {Math.min(currentPage * pageSize, filteredConversions.length)} of {filteredConversions.length} conversions
          </div>
          <div className="flex gap-1 sm:gap-2">
            <button
              onClick={() => setCurrentPage(Math.max(1, currentPage - 1))}
              disabled={currentPage === 1}
              className="px-3 sm:px-4 py-1.5 sm:py-2 bg-gradient-to-r from-purple-500 to-pink-500 text-white rounded-lg sm:rounded-xl hover:from-purple-600 hover:to-pink-600 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 shadow-md hover:shadow-lg transform hover:scale-105 text-sm sm:text-base"
            >
              Previous
            </button>
            <span className="px-3 sm:px-4 py-1.5 sm:py-2 bg-gradient-to-r from-purple-100 to-pink-100 text-purple-700 rounded-lg sm:rounded-xl font-medium text-sm sm:text-base">
              {currentPage}
            </span>
            <button
              onClick={() => setCurrentPage(currentPage + 1)}
              disabled={currentPage * pageSize >= filteredConversions.length}
              className="px-3 sm:px-4 py-1.5 sm:py-2 bg-gradient-to-r from-purple-500 to-pink-500 text-white rounded-lg sm:rounded-xl hover:from-purple-600 hover:to-pink-600 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 shadow-md hover:shadow-lg transform hover:scale-105 text-sm sm:text-base"
            >
              Next
            </button>
          </div>
        </div>
      </div>


    </div>
  )
}

export default Dashboard
