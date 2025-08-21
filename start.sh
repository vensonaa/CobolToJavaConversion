#!/bin/bash

echo "🚀 Starting COBOL to Java Conversion System"
echo "==========================================="

# Activate virtual environment
source venv/bin/activate

# Check if .env file exists
if [ ! -f .env ]; then
    echo "❌ .env file not found. Please run ./setup.sh first"
    exit 1
fi

# Check if GROQ_API_KEY is set
if ! grep -q "GROQ_API_KEY=" .env || grep -q "your_groq_api_key_here" .env; then
    echo "❌ Please set your GROQ_API_KEY in the .env file"
    exit 1
fi

# Start FastAPI backend in background
echo "🔧 Starting FastAPI backend..."
cd api
python main.py &
API_PID=$!
cd ..

# Wait a moment for API to start
sleep 3

# Start Vite frontend
echo "🎨 Starting Vite frontend..."
cd frontend
npm run dev &
FRONTEND_PID=$!
cd ..

echo ""
echo "✅ System started successfully!"
echo ""
echo "🌐 Frontend: http://localhost:5173"
echo "🔧 API: http://localhost:8000"
echo "📚 API Docs: http://localhost:8000/docs"
echo ""
echo "Press Ctrl+C to stop all services"

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "🛑 Stopping services..."
    kill $API_PID 2>/dev/null
    kill $FRONTEND_PID 2>/dev/null
    echo "✅ Services stopped"
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Wait for background processes
wait
