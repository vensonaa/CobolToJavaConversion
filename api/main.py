"""
FastAPI Backend for COBOL to Java Conversion System
"""

from fastapi import FastAPI, HTTPException, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from pydantic import BaseModel
from typing import List, Dict, Any, Optional
import asyncio
import os
import sys
import tempfile
import zipfile
from pathlib import Path

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import the conversion function from the main module
import importlib.util
spec = importlib.util.spec_from_file_location("conversion_module", os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "main.py"))
conversion_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(conversion_module)

from utils.java_file_generator import JavaFileGenerator
from utils.file_utils import save_conversion_results, create_conversion_report

# Initialize FastAPI app
app = FastAPI(
    title="COBOL to Java Conversion API",
    description="A multi-agent LangGraph system for converting COBOL code to Java",
    version="2.0.0"
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173", "http://localhost:3000", "http://127.0.0.1:5173"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Pydantic models
class ConversionRequest(BaseModel):
    cobol_code: str
    prior_knowledge: Optional[str] = ""
    generate_separate_files: bool = True

class ConversionResponse(BaseModel):
    status: str
    message: str
    conversion_id: str
    total_chunks: int
    java_classes_generated: int
    download_url: Optional[str] = None

class ConversionStatus(BaseModel):
    conversion_id: str
    status: str
    progress: int
    message: str
    result: Optional[Dict[str, Any]] = None

# Storage for conversion results (in production, use a database)
conversion_results = {}

@app.get("/")
async def root():
    """Root endpoint with API information."""
    return {
        "message": "COBOL to Java Conversion API",
        "version": "2.0.0",
        "endpoints": {
            "convert": "/api/convert",
            "status": "/api/status/{conversion_id}",
            "download": "/api/download/{conversion_id}",
            "health": "/api/health"
        }
    }

@app.get("/api/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "healthy", "service": "cobol-to-java-converter"}

@app.post("/api/convert", response_model=ConversionResponse)
async def convert_cobol(request: ConversionRequest):
    """
    Convert COBOL code to Java using the multi-agent LangGraph system.
    """
    try:
        # Validate input
        if not request.cobol_code.strip():
            raise HTTPException(status_code=400, detail="COBOL code cannot be empty")
        
        # Generate conversion ID
        import uuid
        conversion_id = str(uuid.uuid4())
        
        # Store initial status
        conversion_results[conversion_id] = {
            "status": "processing",
            "progress": 0,
            "message": "Starting conversion...",
            "result": None
        }
        
        # Run conversion asynchronously
        asyncio.create_task(run_conversion(conversion_id, request))
        
        return ConversionResponse(
            status="processing",
            message="Conversion started successfully",
            conversion_id=conversion_id,
            total_chunks=0,
            java_classes_generated=0
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Conversion failed: {str(e)}")

async def run_conversion(conversion_id: str, request: ConversionRequest):
    """Run the conversion process asynchronously."""
    try:
        # Update status
        conversion_results[conversion_id]["message"] = "Initializing conversion..."
        conversion_results[conversion_id]["progress"] = 10
        
        # Run the conversion
        result = await conversion_module.convert_cobol_to_java(
            request.cobol_code, 
            request.prior_knowledge
        )
        
        # Update status
        conversion_results[conversion_id]["message"] = "Generating Java files..."
        conversion_results[conversion_id]["progress"] = 80
        
        # Generate separate Java files if requested
        java_files = []
        if request.generate_separate_files:
            generator = JavaFileGenerator(f"output/java/{conversion_id}")
            java_files = generator.generate_java_files(result)
        
        # Save results
        file_paths = save_conversion_results(result, f"conversion_{conversion_id}", "output")
        
        # Create comprehensive report
        report = create_conversion_report(result, file_paths)
        report_path = f"output/conversion_{conversion_id}_report.txt"
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write(report)
        
        # Update final status
        conversion_results[conversion_id].update({
            "status": "completed",
            "progress": 100,
            "message": "Conversion completed successfully",
            "result": {
                **result,
                "java_files": java_files,
                "file_paths": file_paths,
                "report_path": report_path
            }
        })
        
    except Exception as e:
        conversion_results[conversion_id].update({
            "status": "failed",
            "progress": 0,
            "message": f"Conversion failed: {str(e)}",
            "result": None
        })

@app.get("/api/status/{conversion_id}", response_model=ConversionStatus)
async def get_conversion_status(conversion_id: str):
    """Get the status of a conversion."""
    if conversion_id not in conversion_results:
        raise HTTPException(status_code=404, detail="Conversion not found")
    
    result = conversion_results[conversion_id]
    
    return ConversionStatus(
        conversion_id=conversion_id,
        status=result["status"],
        progress=result["progress"],
        message=result["message"],
        result=result.get("result")
    )

@app.get("/api/download/{conversion_id}")
async def download_conversion_results(conversion_id: str):
    """Download conversion results as a ZIP file."""
    if conversion_id not in conversion_results:
        raise HTTPException(status_code=404, detail="Conversion not found")
    
    result = conversion_results[conversion_id]
    if result["status"] != "completed":
        raise HTTPException(status_code=400, detail="Conversion not completed")
    
    # Create ZIP file
    zip_path = f"output/conversion_{conversion_id}.zip"
    
    with zipfile.ZipFile(zip_path, 'w') as zipf:
        # Add Java files
        java_dir = f"output/java/{conversion_id}"
        if os.path.exists(java_dir):
            for root, dirs, files in os.walk(java_dir):
                for file in files:
                    file_path = os.path.join(root, file)
                    arc_name = os.path.relpath(file_path, java_dir)
                    zipf.write(file_path, f"java/{arc_name}")
        
        # Add other result files
        result_data = result["result"]
        if result_data and "file_paths" in result_data:
            for file_type, file_path in result_data["file_paths"].items():
                if os.path.exists(file_path):
                    zipf.write(file_path, f"{file_type}/{os.path.basename(file_path)}")
        
        # Add report
        if result_data and "report_path" in result_data:
            report_path = result_data["report_path"]
            if os.path.exists(report_path):
                zipf.write(report_path, "report.txt")
    
    return FileResponse(
        zip_path,
        media_type="application/zip",
        filename=f"conversion_{conversion_id}.zip"
    )

@app.post("/api/upload")
async def upload_cobol_file(file: UploadFile = File(...)):
    """Upload a COBOL file for conversion."""
    try:
        # Validate file
        if not file.filename.endswith(('.cbl', '.cob', '.cobol')):
            raise HTTPException(
                status_code=400, 
                detail="File must be a COBOL file (.cbl, .cob, .cobol)"
            )
        
        # Read file content
        content = await file.read()
        cobol_code = content.decode('utf-8')
        
        # Generate conversion ID
        import uuid
        conversion_id = str(uuid.uuid4())
        
        # Store initial status
        conversion_results[conversion_id] = {
            "status": "processing",
            "progress": 0,
            "message": "Starting conversion from uploaded file...",
            "result": None
        }
        
        # Run conversion asynchronously
        asyncio.create_task(run_conversion(conversion_id, ConversionRequest(
            cobol_code=cobol_code,
            prior_knowledge=f"Uploaded file: {file.filename}",
            generate_separate_files=True
        )))
        
        return {
            "status": "processing",
            "message": "File uploaded and conversion started",
            "conversion_id": conversion_id,
            "filename": file.filename
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"File upload failed: {str(e)}")

@app.get("/api/conversions")
async def list_conversions():
    """List all conversions."""
    return {
        "conversions": [
            {
                "id": conv_id,
                "status": data["status"],
                "progress": data["progress"],
                "message": data["message"],
                "created_at": data.get("created_at")
            }
            for conv_id, data in conversion_results.items()
        ]
    }

@app.delete("/api/conversions/{conversion_id}")
async def delete_conversion(conversion_id: str):
    """Delete a conversion and its files."""
    if conversion_id not in conversion_results:
        raise HTTPException(status_code=404, detail="Conversion not found")
    
    # Remove files
    try:
        import shutil
        
        # Remove Java files directory
        java_dir = f"output/java/{conversion_id}"
        if os.path.exists(java_dir):
            shutil.rmtree(java_dir)
        
        # Remove other files
        result = conversion_results[conversion_id]
        if result.get("result") and "file_paths" in result["result"]:
            for file_path in result["result"]["file_paths"].values():
                if os.path.exists(file_path):
                    os.remove(file_path)
        
        # Remove ZIP file
        zip_path = f"output/conversion_{conversion_id}.zip"
        if os.path.exists(zip_path):
            os.remove(zip_path)
        
    except Exception as e:
        print(f"Error cleaning up files for {conversion_id}: {e}")
    
    # Remove from memory
    del conversion_results[conversion_id]
    
    return {"message": "Conversion deleted successfully"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
