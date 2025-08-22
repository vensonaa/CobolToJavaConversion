import axios from 'axios'

const API_BASE_URL = '/api'

const api = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
})

export interface ConversionRequest {
  cobol_code: string
  prior_knowledge?: string
  generate_separate_files?: boolean
}

export interface ConversionResponse {
  status: string
  message: string
  conversion_id: string
  total_chunks: number
  java_classes_generated: number
  download_url?: string
}

export interface ConversionStatus {
  conversion_id: string
  status: string
  progress: number
  message: string
  result?: any
}

export const convertCobol = async (request: ConversionRequest): Promise<ConversionResponse> => {
  try {
    const response = await api.post('/convert', request)
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Conversion failed')
    }
    throw error
  }
}

export const getConversionStatus = async (conversionId: string): Promise<ConversionStatus> => {
  try {
    const response = await api.get(`/status/${conversionId}`)
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Failed to get status')
    }
    throw error
  }
}

export const downloadResults = async (conversionId: string): Promise<void> => {
  try {
    const response = await api.get(`/download/${conversionId}`, {
      responseType: 'blob',
    })
    
    // Create download link
    const url = window.URL.createObjectURL(new Blob([response.data]))
    const link = document.createElement('a')
    link.href = url
    link.setAttribute('download', `conversion_${conversionId}.zip`)
    document.body.appendChild(link)
    link.click()
    link.remove()
    window.URL.revokeObjectURL(url)
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Download failed')
    }
    throw error
  }
}

export const uploadFile = async (file: File): Promise<ConversionResponse> => {
  try {
    const formData = new FormData()
    formData.append('file', file)
    
    const response = await api.post('/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
    })
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'File upload failed')
    }
    throw error
  }
}



export const listConversions = async () => {
  try {
    const response = await api.get('/conversions')
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Failed to list conversions')
    }
    throw error
  }
}

export const deleteConversion = async (conversionId: string): Promise<any> => {
  try {
    const response = await api.delete(`/conversions/${conversionId}`)
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Failed to delete conversion')
    }
    throw error
  }
}

export const getConversionDetails = async (conversionId: string): Promise<any> => {
  try {
    const response = await api.get(`/conversions/${conversionId}/details`)
    return response.data
  } catch (error) {
    if (axios.isAxiosError(error)) {
      throw new Error(error.response?.data?.detail || 'Failed to get conversion details')
    }
    throw error
  }
}
