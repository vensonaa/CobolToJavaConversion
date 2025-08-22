# COBOL to Java Conversion using LangGraph

A sophisticated multi-agent system built with LangGraph that converts COBOL code to Java using a collaborative workflow of specialized AI agents powered by Groq, with a modern web interface.

## 🏗️ Architecture Overview

This project implements a **7-agent workflow** using LangGraph for intelligent COBOL to Java conversion:

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Planner   │───▶│  Executor   │───▶│ Pseudo Code │───▶│Java Generator│
│   Agent     │    │   Agent     │    │  Generator  │    │   Agent     │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
                                                              │
                                                              ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Summarizer │◀───│   Fixer     │◀───│  Reviewer   │◀───│Java Code    │
│   Agent     │    │   Agent     │    │   Agent     │    │Generated    │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
```

### Agent Responsibilities

1. **Planner Agent**: Analyzes COBOL code and prior knowledge to create a conversion strategy
2. **Executor Agent**: Orchestrates the entire conversion workflow
3. **Pseudo Code Generator**: Converts COBOL logic into clear pseudo code
4. **Java Code Generator**: Transforms COBOL/pseudo code into Java implementation
5. **Code Reviewer**: Reviews Java code for quality, correctness, and best practices
6. **Code Fixer**: Addresses issues identified by the reviewer
7. **Summarizer Agent**: Provides comprehensive conversion summary

## 🚀 Features

- **Multi-Agent Collaboration**: 7 specialized agents working together
- **Iterative Refinement**: Review-fix cycle until code quality is satisfactory
- **Prior Knowledge Integration**: Leverages domain expertise for better conversions
- **Comprehensive Output**: Generates Java code, pseudo code, and detailed summaries
- **File I/O Support**: Handles complex COBOL file operations
- **Error Handling**: Robust error handling throughout the conversion process
- **Configurable**: Easy configuration through environment variables
- **Fast Processing**: Powered by Groq's high-performance LLM infrastructure
- **Web Interface**: Modern React frontend with Vite
- **REST API**: FastAPI backend with comprehensive endpoints
- **Separate Java Files**: Generates individual Java class files
- **Chunking Strategy**: Handles large COBOL programs efficiently
- **Dashboard**: Conversion history with statistics and management
- **Download Functionality**: Download conversion results as ZIP files
- **Delete Functionality**: Remove conversions and clean up files
- **Database Persistence**: SQLite database for conversion history
- **Real-time Updates**: Live status updates during conversion
- **Responsive Design**: Works on desktop, tablet, and mobile

## 📋 Prerequisites

- Python 3.8+
- Node.js 16+
- Groq API key
- Required Python packages (see `requirements.txt`)

## 🛠️ Quick Setup

### 1. Clone and Setup

```bash
git clone <repository-url>
cd CobolToJavaConversion
chmod +x setup.sh start.sh
./setup.sh
```

### 2. Configure Environment

Edit the `.env` file and add your Groq API key:
```env
GROQ_API_KEY=your_groq_api_key_here
```

### 3. Start the System

```bash
./start.sh
```

This will start:
- **FastAPI Backend**: http://localhost:8000
- **Vite Frontend**: http://localhost:5173
- **API Documentation**: http://localhost:8000/docs

## 🎯 Usage

### Web Interface

1. Open http://localhost:5173 in your browser
2. Navigate between three main sections:
   - **Code Editor**: Enter COBOL code directly
   - **File Upload**: Upload COBOL files for conversion
   - **Dashboard**: View conversion history and manage results

#### Dashboard Features

The dashboard provides comprehensive conversion management:

- **Statistics Overview**: View total conversions, success rates, and generated files
- **Conversion History**: Browse all past conversions with pagination
- **Status Filtering**: Filter by processing, completed, or failed conversions
- **Download Results**: Download conversion results as ZIP files containing:
  - Generated Java source files
  - Pseudo code documentation
  - Conversion summary reports
- **Delete Conversions**: Remove conversions and clean up associated files
- **Detailed View**: Click "View" to see full conversion details including:
  - Original COBOL code
  - Generated Java code
  - Pseudo code explanation
  - Conversion summary
- **Real-time Updates**: Live status updates during conversion process

#### Conversion Process

1. Enter COBOL code or upload a COBOL file
2. Add optional prior knowledge for better conversion
3. Click "Convert to Java"
4. Monitor progress in real-time
5. View results and download generated files
6. Manage conversions through the dashboard

### Command Line

```bash
# Simple conversion
python main.py

# Banking system conversion
python run_banking_conversion.py

# CLI interface
python cli.py --input samples/banking_system.cbl --save-all
```

### API Endpoints

```bash
# Convert COBOL code
curl -X POST "http://localhost:8000/api/convert" \
  -H "Content-Type: application/json" \
  -d '{"cobol_code": "IDENTIFICATION DIVISION...", "prior_knowledge": "..."}'

# Upload COBOL file
curl -X POST "http://localhost:8000/api/upload" \
  -F "file=@sample.cbl"

# Check conversion status
curl "http://localhost:8000/api/status/{conversion_id}"

# Download results
curl "http://localhost:8000/api/download/{conversion_id}" -o results.zip
```

## 📁 Project Structure

```
CobolToJavaConversion/
├── main.py                          # Main LangGraph application
├── config.py                        # Configuration settings
├── cli.py                          # Command-line interface
├── requirements.txt                 # Python dependencies
├── setup.sh                        # Setup script
├── start.sh                        # Start script
├── README.md                       # This file
├── env_template.txt                # Environment variables template
├── run_banking_conversion.py       # Banking system conversion
├── api/                            # FastAPI backend
│   └── main.py                     # API server
├── frontend/                       # Vite React frontend
│   ├── package.json               # Node.js dependencies
│   ├── vite.config.ts             # Vite configuration
│   └── src/                       # React source code
│       ├── App.tsx                # Main app component
│       ├── api/                   # API client
│       └── components/            # React components
├── utils/                          # Utility functions
│   ├── file_utils.py              # File operations
│   └── java_file_generator.py     # Java file generation
├── examples/                       # Example scripts
│   └── complex_cobol_example.py   # Complex COBOL conversion
├── samples/                        # Sample COBOL files
│   ├── sample.cbl                 # Simple COBOL example
│   └── banking_system.cbl         # Large banking system
└── output/                         # Generated output files
    ├── java/                      # Generated Java files
    ├── *.java                     # Individual Java classes
    ├── *_pseudo.txt              # Pseudo code analysis
    ├── *_summary.txt             # Conversion summaries
    └── *_complete.json           # Complete conversion results
```

## 🔧 Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GROQ_API_KEY` | Your Groq API key | Required |
| `GROQ_MODEL` | Groq model to use | `llama3-70b-8192` |
| `GROQ_TEMPERATURE` | Model temperature | `0.1` |
| `MAX_REVIEW_ITERATIONS` | Max review-fix cycles | `5` |
| `OUTPUT_DIR` | Output directory | `output` |
| `ENABLE_DEBUG_LOGGING` | Enable debug logging | `false` |

### Agent Configuration

Each agent can be configured with specific parameters in `config.py`:

```python
AGENT_CONFIGS = {
    "planner": {
        "temperature": 0.1,
        "max_tokens": 2000,
        "system_prompt": "Custom system prompt..."
    },
    # ... other agents
}
```

## 📊 Conversion Workflow

1. **Planning Phase**: Planner agent analyzes COBOL code and creates conversion strategy
2. **Execution Phase**: Executor agent coordinates the conversion process
3. **Analysis Phase**: Pseudo code generator creates logical representation
4. **Generation Phase**: Java code generator creates initial Java implementation
5. **Review Phase**: Code reviewer evaluates Java code quality
6. **Refinement Phase**: Code fixer addresses issues (iterative)
7. **Summary Phase**: Final summarizer provides comprehensive report

## 🎨 Example Output

### Input COBOL
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'
    STOP RUN.
```

### Generated Java
```java
package com.banking.system;

public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

### Pseudo Code
```
Program: HelloWorld
- Display a greeting message "Hello, World!"
- Terminate the program
```

## 🔍 Advanced Features

### File I/O Support
The system can handle complex COBOL file operations:
- Sequential file reading/writing
- Record structures
- File status handling
- Error management

### Business Logic Conversion
- COBOL data types to Java mapping
- Control structures (IF-THEN-ELSE, PERFORM, etc.)
- Mathematical operations
- String manipulation

### Quality Assurance
- Multiple review iterations
- Code quality assessment
- Best practices enforcement
- Error handling validation

### Chunking Strategy
- Automatic splitting of large COBOL programs
- Per-chunk processing through the workflow
- Result combination and aggregation
- Handles context length limitations

## 🚨 Error Handling

The system includes comprehensive error handling:
- File I/O errors
- API communication issues
- Invalid COBOL syntax
- Conversion failures
- Configuration errors

## 📈 Performance Considerations

- **Async Processing**: All operations are asynchronous for better performance
- **Token Management**: Configurable token limits for cost control
- **Iteration Limits**: Prevents infinite review-fix loops
- **Memory Management**: Efficient state management throughout the workflow
- **Fast LLM**: Powered by Groq's high-performance infrastructure
- **Chunking**: Efficient handling of large programs

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🆘 Support

For issues and questions:
1. Check the documentation
2. Review existing issues
3. Create a new issue with detailed information

## 🔮 Future Enhancements

- [ ] Support for more COBOL dialects
- [ ] Integration with IDEs
- [ ] Batch processing capabilities
- [ ] Custom conversion templates
- [ ] Performance optimization
- [ ] Additional output formats
- [ ] Real-time collaboration features
- [ ] Advanced code analysis tools

---

**Note**: This project requires a Groq API key and will incur costs based on API usage. Please review Groq's pricing before use.
