# COBOL to Java Conversion using LangGraph

A sophisticated multi-agent system built with LangGraph that converts COBOL code to Java using a collaborative workflow of specialized AI agents powered by Groq.

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

## 📋 Prerequisites

- Python 3.8+
- Groq API key
- Required Python packages (see `requirements.txt`)

## 🛠️ Installation

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd CobolToJavaConversion
   ```

2. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

3. **Set up environment variables**:
   Create a `.env` file in the project root:
   ```env
   GROQ_API_KEY=your_groq_api_key_here
   GROQ_MODEL=llama3-70b-8192
   GROQ_TEMPERATURE=0.1
   MAX_REVIEW_ITERATIONS=5
   OUTPUT_DIR=output
   ```

## 🎯 Usage

### Basic Usage

```python
import asyncio
from main import convert_cobol_to_java

async def main():
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
           DISPLAY 'Hello, World!'
           STOP RUN.
    """
    
    prior_knowledge = "Simple COBOL program that displays a greeting."
    
    result = await convert_cobol_to_java(cobol_code, prior_knowledge)
    
    print(f"Java Code:\n{result['final_java_code']}")
    print(f"Summary:\n{result['summary']}")

asyncio.run(main())
```

### Running Examples

1. **Simple Example**:
   ```bash
   python main.py
   ```

2. **Complex Example** (Payroll System):
   ```bash
   python examples/complex_cobol_example.py
   ```

### Command Line Interface

```bash
# Convert a COBOL file
python cli.py --input sample.cbl --output HelloWorld.java

# Convert with prior knowledge
python cli.py --input payroll.cbl --knowledge "Banking application with file I/O"

# Convert and save all results
python cli.py --input complex.cbl --save-all --output-dir results/

# Interactive mode
python cli.py --interactive
```

## 📁 Project Structure

```
CobolToJavaConversion/
├── main.py                          # Main application entry point
├── config.py                        # Configuration settings
├── cli.py                          # Command-line interface
├── requirements.txt                 # Python dependencies
├── README.md                       # This file
├── env_template.txt                # Environment variables template
├── test_simple_conversion.py       # Basic tests
├── examples/                       # Example scripts
│   └── complex_cobol_example.py    # Complex COBOL conversion example
├── utils/                          # Utility functions
│   └── file_utils.py               # File operations and result management
└── output/                         # Generated output files (auto-created)
    ├── *.java                      # Generated Java files
    ├── *_pseudo.txt               # Pseudo code analysis
    ├── *_summary.txt              # Conversion summaries
    └── *_complete.json            # Complete conversion results
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

---

**Note**: This project requires a Groq API key and will incur costs based on API usage. Please review Groq's pricing before use.
