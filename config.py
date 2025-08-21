"""
Configuration settings for the COBOL to Java conversion LangGraph project.
"""

import os
from typing import Dict, Any
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

class Config:
    """Configuration class for the COBOL to Java conversion system."""
    
    # Groq Configuration
    GROQ_API_KEY = os.getenv("GROQ_API_KEY")
    GROQ_MODEL = os.getenv("GROQ_MODEL", "llama3-70b-8192")
    GROQ_TEMPERATURE = float(os.getenv("GROQ_TEMPERATURE", "0.1"))
    GROQ_MAX_TOKENS = int(os.getenv("GROQ_MAX_TOKENS", "4000"))
    
    # Workflow Configuration
    MAX_REVIEW_ITERATIONS = int(os.getenv("MAX_REVIEW_ITERATIONS", "5"))
    ENABLE_DEBUG_LOGGING = os.getenv("ENABLE_DEBUG_LOGGING", "false").lower() == "true"
    
    # Output Configuration
    OUTPUT_DIR = os.getenv("OUTPUT_DIR", "output")
    SAVE_INTERMEDIATE_RESULTS = os.getenv("SAVE_INTERMEDIATE_RESULTS", "true").lower() == "true"
    
    # Agent Configuration
    AGENT_CONFIGS = {
        "planner": {
            "temperature": 0.1,
            "max_tokens": 2000,
            "system_prompt": "You are a COBOL to Java conversion planner with expertise in both languages."
        },
        "executor": {
            "temperature": 0.1,
            "max_tokens": 1500,
            "system_prompt": "You are an executor agent that coordinates the conversion workflow."
        },
        "pseudo_generator": {
            "temperature": 0.1,
            "max_tokens": 3000,
            "system_prompt": "You are an expert COBOL analyst who generates clear pseudo code."
        },
        "java_generator": {
            "temperature": 0.1,
            "max_tokens": 4000,
            "system_prompt": "You are an expert Java developer specializing in COBOL to Java conversion."
        },
        "reviewer": {
            "temperature": 0.1,
            "max_tokens": 2000,
            "system_prompt": "You are a senior Java code reviewer with expertise in code quality."
        },
        "fixer": {
            "temperature": 0.1,
            "max_tokens": 4000,
            "system_prompt": "You are a Java code fixer who addresses review comments effectively."
        },
        "summarizer": {
            "temperature": 0.1,
            "max_tokens": 2000,
            "system_prompt": "You are a conversion process summarizer who provides comprehensive reports."
        }
    }
    
    # COBOL to Java Mapping Configuration
    COBOL_JAVA_MAPPINGS = {
        "data_types": {
            "PIC X": "String",
            "PIC 9": "int",
            "PIC 9(3)": "int",
            "PIC 9(5)": "int",
            "PIC 9(10)": "long",
            "PIC 9(15)": "BigInteger",
            "PIC S9": "int",
            "PIC S9(10)": "long",
            "PIC 9(7)V99": "BigDecimal",
            "PIC S9(7)V99": "BigDecimal"
        },
        "control_structures": {
            "IF-THEN-ELSE": "if-else",
            "PERFORM": "method call",
            "PERFORM UNTIL": "while loop",
            "PERFORM VARYING": "for loop",
            "EVALUATE": "switch-case"
        },
        "file_operations": {
            "OPEN INPUT": "FileInputStream",
            "OPEN OUTPUT": "FileOutputStream",
            "READ": "BufferedReader.readLine()",
            "WRITE": "PrintWriter.println()",
            "CLOSE": "close()"
        }
    }
    
    @classmethod
    def validate(cls) -> bool:
        """Validate the configuration settings."""
        if not cls.GROQ_API_KEY:
            raise ValueError("GROQ_API_KEY is required. Please set it in your .env file.")
        
        if cls.MAX_REVIEW_ITERATIONS < 1:
            raise ValueError("MAX_REVIEW_ITERATIONS must be at least 1.")
        
        return True
    
    @classmethod
    def get_agent_config(cls, agent_name: str) -> Dict[str, Any]:
        """Get configuration for a specific agent."""
        return cls.AGENT_CONFIGS.get(agent_name, {})
    
    @classmethod
    def get_cobol_java_mapping(cls, category: str, key: str) -> str:
        """Get COBOL to Java mapping for a specific category and key."""
        return cls.COBOL_JAVA_MAPPINGS.get(category, {}).get(key, "")

# Validate configuration on import
Config.validate()
