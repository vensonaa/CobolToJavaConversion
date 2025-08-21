import React from 'react'

interface ConversionStatusProps {
  progress: number
  message: string
}

const ConversionStatus: React.FC<ConversionStatusProps> = ({ progress, message }) => {
  return (
    <div className="space-y-3">
      <div className="flex justify-between text-sm">
        <span className="text-gray-600">{message}</span>
        <span className="text-gray-900 font-medium">{progress}%</span>
      </div>
      <div className="w-full bg-gray-200 rounded-full h-2">
        <div
          className="bg-blue-600 h-2 rounded-full transition-all duration-300 ease-out"
          style={{ width: `${progress}%` }}
        />
      </div>
    </div>
  )
}

export default ConversionStatus
