"""
FastAPI Backend for COBOL to Java Conversion System
"""

from fastapi import FastAPI, HTTPException, UploadFile, File, Depends, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from pydantic import BaseModel
from typing import List, Dict, Any, Optional
import asyncio
import os
import sys
import tempfile
import zipfile
import json
import uuid
from pathlib import Path
from datetime import datetime
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, desc, func
from database import ConversionHistory, get_session, init_database

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

@app.on_event("startup")
async def startup_event():
    """Initialize database on startup"""
    await init_database()

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

class DashboardResponse(BaseModel):
    conversions: List[Dict[str, Any]]
    total: int
    page: int
    limit: int
    total_pages: int

class ConversionStats(BaseModel):
    total_conversions: int
    successful_conversions: int
    failed_conversions: int
    total_java_files: int

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
        
        # Store initial status in memory
        conversion_results[conversion_id] = {
            "status": "processing",
            "progress": 0,
            "message": "Starting conversion...",
            "result": None
        }
        
        # Create initial database record
        async for db in get_session():
            conversion_record = ConversionHistory(
                conversion_id=conversion_id,
                cobol_code=request.cobol_code,
                prior_knowledge=request.prior_knowledge,
                status="processing",
                message="Starting conversion...",
                progress=0,
                java_code="",
                pseudo_code="",
                summary="",
                java_files_count=0,
                total_chunks=0
            )
            db.add(conversion_record)
            await db.commit()
            break
        
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
        # Update status in memory
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
        
        # Update final status in memory
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
        
        # Update database record
        async for db in get_session():
            query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
            result_db = await db.execute(query)
            conversion_record = result_db.scalars().first()
            
            if conversion_record:
                conversion_record.status = "completed"
                conversion_record.message = "Conversion completed successfully"
                conversion_record.progress = 100
                conversion_record.java_code = result.get("final_java_code", "")
                conversion_record.pseudo_code = result.get("pseudo_code", "")
                conversion_record.summary = result.get("summary", "")
                conversion_record.java_files_count = len(java_files)
                conversion_record.total_chunks = result.get("total_chunks", 0)
                conversion_record.updated_at = datetime.utcnow()
                
                await db.commit()
            break
        
    except Exception as e:
        error_message = f"Conversion failed: {str(e)}"
        
        # Update memory status
        conversion_results[conversion_id].update({
            "status": "failed",
            "progress": 0,
            "message": error_message,
            "result": None
        })
        
        # Update database record
        try:
            async for db in get_session():
                query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
                result_db = await db.execute(query)
                conversion_record = result_db.scalars().first()
                
                if conversion_record:
                    conversion_record.status = "failed"
                    conversion_record.message = error_message
                    conversion_record.progress = 0
                    conversion_record.updated_at = datetime.utcnow()
                    
                    await db.commit()
                break
        except Exception as db_error:
            print(f"Failed to update database for failed conversion {conversion_id}: {db_error}")

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
async def download_conversion_results(conversion_id: str, db: AsyncSession = Depends(get_session)):
    """Download conversion results as a ZIP file."""
    try:
        # Check if conversion exists in database
        query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
        result = await db.execute(query)
        conversion_record = result.scalars().first()
        
        if not conversion_record:
            raise HTTPException(status_code=404, detail="Conversion not found")
        
        if conversion_record.status != "completed":
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
            
            # Add other output files
            output_pattern = f"output/conversion_{conversion_id}_*"
            import glob
            for file_path in glob.glob(output_pattern):
                if os.path.exists(file_path):
                    file_type = os.path.splitext(os.path.basename(file_path))[1][1:]  # Remove dot
                    zipf.write(file_path, f"{file_type}/{os.path.basename(file_path)}")
        
        return FileResponse(
            zip_path,
            media_type="application/zip",
            filename=f"conversion_{conversion_id}.zip"
        )
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Download failed: {str(e)}")

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
        
        # Store initial status in memory
        conversion_results[conversion_id] = {
            "status": "processing",
            "progress": 0,
            "message": "Starting conversion from uploaded file...",
            "result": None
        }
        
        # Create initial database record
        async for db in get_session():
            conversion_record = ConversionHistory(
                conversion_id=conversion_id,
                cobol_code=cobol_code,
                prior_knowledge=f"Uploaded file: {file.filename}",
                status="processing",
                message="Starting conversion from uploaded file...",
                progress=0,
                java_code="",
                pseudo_code="",
                summary="",
                java_files_count=0,
                total_chunks=0
            )
            db.add(conversion_record)
            await db.commit()
            break
        
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
async def delete_conversion(conversion_id: str, db: AsyncSession = Depends(get_session)):
    """Delete a conversion and its files."""
    try:
        # Check if conversion exists in database
        query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
        result = await db.execute(query)
        conversion_record = result.scalars().first()
        
        if not conversion_record:
            raise HTTPException(status_code=404, detail="Conversion not found")
        
        # Remove files
        try:
            import shutil
            
            # Remove Java files directory
            java_dir = f"output/java/{conversion_id}"
            if os.path.exists(java_dir):
                shutil.rmtree(java_dir)
            
            # Remove ZIP file
            zip_path = f"output/conversion_{conversion_id}.zip"
            if os.path.exists(zip_path):
                os.remove(zip_path)
            
            # Remove other output files
            output_pattern = f"output/conversion_{conversion_id}_*"
            import glob
            for file_path in glob.glob(output_pattern):
                if os.path.exists(file_path):
                    os.remove(file_path)
            
        except Exception as e:
            print(f"Error cleaning up files for {conversion_id}: {e}")
        
        # Remove from memory if exists
        if conversion_id in conversion_results:
            del conversion_results[conversion_id]
        
        # Remove from database
        await db.delete(conversion_record)
        await db.commit()
        
        return {"message": "Conversion deleted successfully"}
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to delete conversion: {str(e)}")

# Dashboard endpoints
@app.get("/api/dashboard", response_model=DashboardResponse)
async def get_dashboard(
    page: int = Query(1, ge=1, description="Page number"),
    limit: int = Query(10, ge=1, le=100, description="Items per page"),
    status: Optional[str] = Query(None, description="Filter by status"),
    db: AsyncSession = Depends(get_session)
):
    """Get paginated conversion history for dashboard"""
    try:
        # Build query
        query = select(ConversionHistory)
        
        if status:
            query = query.where(ConversionHistory.status == status)
        
        # Get total count
        count_query = select(func.count(ConversionHistory.id))
        if status:
            count_query = count_query.where(ConversionHistory.status == status)
        
        total_result = await db.execute(count_query)
        total = total_result.scalar()
        
        # Get paginated results
        query = query.order_by(desc(ConversionHistory.created_at))
        query = query.offset((page - 1) * limit).limit(limit)
        
        result = await db.execute(query)
        conversions = result.scalars().all()
        
        # Convert to dict
        conversions_dict = [conv.to_dict() for conv in conversions]
        
        total_pages = (total + limit - 1) // limit
        
        return DashboardResponse(
            conversions=conversions_dict,
            total=total,
            page=page,
            limit=limit,
            total_pages=total_pages
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to fetch dashboard data: {str(e)}")

@app.get("/api/dashboard/stats", response_model=ConversionStats)
async def get_conversion_stats(db: AsyncSession = Depends(get_session)):
    """Get conversion statistics for dashboard"""
    try:
        # Total conversions
        total_query = select(func.count(ConversionHistory.id))
        total_result = await db.execute(total_query)
        total_conversions = total_result.scalar()
        
        # Successful conversions
        success_query = select(func.count(ConversionHistory.id)).where(
            ConversionHistory.status == "completed"
        )
        success_result = await db.execute(success_query)
        successful_conversions = success_result.scalar()
        
        # Failed conversions
        failed_query = select(func.count(ConversionHistory.id)).where(
            ConversionHistory.status == "failed"
        )
        failed_result = await db.execute(failed_query)
        failed_conversions = failed_result.scalar()
        
        # Total Java files
        java_files_query = select(func.sum(ConversionHistory.java_files_count)).where(
            ConversionHistory.status == "completed"
        )
        java_files_result = await db.execute(java_files_query)
        total_java_files = java_files_result.scalar() or 0
        
        return ConversionStats(
            total_conversions=total_conversions,
            successful_conversions=successful_conversions,
            failed_conversions=failed_conversions,
            total_java_files=total_java_files
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to fetch stats: {str(e)}")

@app.get("/api/conversions/{conversion_id}/details")
async def get_conversion_details(conversion_id: str, db: AsyncSession = Depends(get_session)):
    """Get detailed information about a specific conversion"""
    try:
        query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
        result = await db.execute(query)
        conversion = result.scalars().first()
        
        if not conversion:
            raise HTTPException(status_code=404, detail="Conversion not found")
        
        # Convert to dict and format for frontend compatibility
        conversion_dict = conversion.to_dict()
        
        # Add the fields that ResultsViewer expects directly to the main object
        if conversion.status == "completed":
            conversion_dict["final_java_code"] = conversion.java_code
            conversion_dict["pseudo_code"] = conversion.pseudo_code
            conversion_dict["summary"] = conversion.summary
            conversion_dict["total_chunks"] = conversion.total_chunks
            conversion_dict["java_files"] = []  # Empty array for now, can be populated later if needed
            
            # Debug logging
            print(f"Conversion {conversion_id} - Java code length: {len(conversion.java_code or '')}")
            print(f"Conversion {conversion_id} - Pseudo code length: {len(conversion.pseudo_code or '')}")
            print(f"Conversion {conversion_id} - Summary length: {len(conversion.summary or '')}")
        
        return conversion_dict
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to fetch conversion details: {str(e)}")

@app.get("/api/debug/conversion/{conversion_id}")
async def debug_conversion(conversion_id: str, db: AsyncSession = Depends(get_session)):
    """Debug endpoint to see raw database data"""
    try:
        query = select(ConversionHistory).where(ConversionHistory.conversion_id == conversion_id)
        result = await db.execute(query)
        conversion = result.scalars().first()
        
        if not conversion:
            raise HTTPException(status_code=404, detail="Conversion not found")
        
        return {
            "id": conversion.id,
            "conversion_id": conversion.conversion_id,
            "status": conversion.status,
            "java_code_length": len(conversion.java_code or ""),
            "pseudo_code_length": len(conversion.pseudo_code or ""),
            "summary_length": len(conversion.summary or ""),
            "java_code_preview": (conversion.java_code or "")[:200] + "..." if conversion.java_code else "None",
            "pseudo_code_preview": (conversion.pseudo_code or "")[:200] + "..." if conversion.pseudo_code else "None",
            "summary_preview": (conversion.summary or "")[:200] + "..." if conversion.summary else "None"
        }
        
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Debug failed: {str(e)}")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
