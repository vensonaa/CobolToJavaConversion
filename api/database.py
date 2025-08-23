"""
Database models and configuration for the COBOL to Java conversion system.
"""

from sqlalchemy import Column, Integer, String, Text, DateTime, Float, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.orm import sessionmaker
from datetime import datetime
import json
import os

Base = declarative_base()

class ConversionHistory(Base):
    """Model for storing conversion history"""
    __tablename__ = "conversion_history"
    
    id = Column(Integer, primary_key=True, index=True)
    conversion_id = Column(String(36), unique=True, index=True)
    status = Column(String(20), default="pending")  # pending, processing, completed, failed
    message = Column(Text, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    completed_at = Column(DateTime, nullable=True)
    
    # Input data
    cobol_code = Column(Text, nullable=True)
    prior_knowledge = Column(Text, nullable=True)
    
    # Processing details
    total_chunks = Column(Integer, default=1)
    progress = Column(Float, default=0.0)
    
    # Results
    java_code = Column(Text, nullable=True)
    pseudo_code = Column(Text, nullable=True)
    summary = Column(Text, nullable=True)
    java_files_count = Column(Integer, default=0)
    
    # Mainframe Analysis Document
    mad_analysis = Column(Text, nullable=True)
    
    # Error handling
    error_message = Column(Text, nullable=True)
    
    def to_dict(self):
        """Convert model to dictionary"""
        result = {
            "id": self.id,
            "conversion_id": self.conversion_id,
            "status": self.status,
            "message": self.message,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "cobol_code": self.cobol_code,
            "prior_knowledge": self.prior_knowledge,
            "total_chunks": self.total_chunks,
            "progress": self.progress,
            "java_code": self.java_code,
            "pseudo_code": self.pseudo_code,
            "summary": self.summary,
            "java_files_count": self.java_files_count,
            "mad_analysis": self.mad_analysis,
            "error_message": self.error_message
        }
            
        return result

# Database configuration
DATABASE_URL = "sqlite+aiosqlite:///./conversions.db"

engine = create_async_engine(
    DATABASE_URL,
    echo=False,
    future=True
)

AsyncSessionLocal = sessionmaker(
    engine,
    class_=AsyncSession,
    expire_on_commit=False
)

async def init_database():
    """Initialize the database tables"""
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

async def get_session():
    """Get database session"""
    session = AsyncSessionLocal()
    try:
        yield session
    finally:
        await session.close()
