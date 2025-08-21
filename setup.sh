#!/bin/bash

echo "🚀 Setting up COBOL to Java Conversion System"
echo "=============================================="

# Check if Python 3.8+ is installed
python_version=$(python3 --version 2>&1 | sed -E 's/.*Python ([0-9]+\.[0-9]+).*/\1/' | head -1)
required_version="3.8"

if [ "$(printf '%s\n' "$required_version" "$python_version" | sort -V | head -n1)" = "$required_version" ]; then
    echo "✅ Python $python_version is installed"
else
    echo "❌ Python 3.8+ is required. Current version: $python_version"
    exit 1
fi

# Create virtual environment
echo "📦 Creating virtual environment..."
python3 -m venv venv
source venv/bin/activate

# Install Python dependencies
echo "📦 Installing Python dependencies..."
pip install --upgrade pip
pip install -r requirements.txt

# Install Node.js dependencies for frontend
echo "📦 Installing Node.js dependencies..."
cd frontend
npm install
cd ..

# Create necessary directories
echo "📁 Creating output directories..."
mkdir -p output/java
mkdir -p output/reports

# Set up environment file
if [ ! -f .env ]; then
    echo "📝 Creating .env file..."
    cp env_template.txt .env
    echo "⚠️  Please edit .env file and add your GROQ_API_KEY"
fi

echo ""
echo "✅ Setup completed successfully!"
echo ""
echo "🚀 To start the system:"
echo "   1. Edit .env file and add your GROQ_API_KEY"
echo "   2. Run: ./start.sh"
echo ""
echo "📚 Available commands:"
echo "   ./start.sh          - Start the full system (API + Frontend)"
echo "   ./start_api.sh      - Start only the FastAPI backend"
echo "   ./start_frontend.sh - Start only the Vite frontend"
echo "   python main.py      - Run simple conversion"
echo "   python run_banking_conversion.py - Run banking system conversion"
