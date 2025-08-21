import React, { useState } from 'react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { tomorrow } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { FileText, Code, FileCode, BookOpen } from 'lucide-react'

interface ResultsViewerProps {
  result: any
}

const ResultsViewer: React.FC<ResultsViewerProps> = ({ result }) => {
  const [activeTab, setActiveTab] = useState<'java' | 'pseudo' | 'summary'>('java')

  const tabs = [
    { id: 'java', label: 'Java Code', icon: Code },
    { id: 'pseudo', label: 'Pseudo Code', icon: FileText },
    { id: 'summary', label: 'Summary', icon: BookOpen },
  ]

  const renderContent = () => {
    switch (activeTab) {
      case 'java':
        return (
          <div className="max-h-96 overflow-auto">
            <SyntaxHighlighter
              language="java"
              style={tomorrow}
              customStyle={{
                margin: 0,
                fontSize: '12px',
                lineHeight: '1.4',
              }}
              showLineNumbers
            >
              {result.final_java_code || 'No Java code generated'}
            </SyntaxHighlighter>
          </div>
        )
      case 'pseudo':
        return (
          <div className="max-h-96 overflow-auto">
            <SyntaxHighlighter
              language="text"
              style={tomorrow}
              customStyle={{
                margin: 0,
                fontSize: '12px',
                lineHeight: '1.4',
              }}
            >
              {result.pseudo_code || 'No pseudo code generated'}
            </SyntaxHighlighter>
          </div>
        )
      case 'summary':
        return (
          <div className="max-h-96 overflow-auto prose prose-sm">
            <div className="whitespace-pre-wrap text-sm text-gray-700">
              {result.summary || 'No summary available'}
            </div>
          </div>
        )
      default:
        return null
    }
  }

  return (
    <div className="space-y-4">
      {/* Statistics */}
      <div className="grid grid-cols-3 gap-4 text-sm">
        <div className="bg-gray-50 p-3 rounded-lg">
          <div className="text-gray-500">Total Chunks</div>
          <div className="font-semibold text-gray-900">{result.total_chunks || 1}</div>
        </div>
        <div className="bg-gray-50 p-3 rounded-lg">
          <div className="text-gray-500">Java Classes</div>
          <div className="font-semibold text-gray-900">
            {result.java_files?.length || 0}
          </div>
        </div>
        <div className="bg-gray-50 p-3 rounded-lg">
          <div className="text-gray-500">Status</div>
          <div className="font-semibold text-green-600">Completed</div>
        </div>
      </div>

      {/* Tabs */}
      <div className="border-b border-gray-200">
        <nav className="-mb-px flex space-x-8">
          {tabs.map((tab) => {
            const Icon = tab.icon
            return (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id as any)}
                className={`py-2 px-1 border-b-2 font-medium text-sm flex items-center space-x-2 ${
                  activeTab === tab.id
                    ? 'border-blue-500 text-blue-600'
                    : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                }`}
              >
                <Icon className="w-4 h-4" />
                <span>{tab.label}</span>
              </button>
            )
          })}
        </nav>
      </div>

      {/* Content */}
      <div className="bg-gray-50 rounded-lg border">
        {renderContent()}
      </div>
    </div>
  )
}

export default ResultsViewer
