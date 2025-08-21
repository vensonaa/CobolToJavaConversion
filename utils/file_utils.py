"""
Utility functions for file operations and result management.
"""

import os
import json
from datetime import datetime
from typing import Dict, Any, Optional
from pathlib import Path

def ensure_directory(directory_path: str) -> str:
    """
    Ensure a directory exists, create it if it doesn't.
    
    Args:
        directory_path: Path to the directory
        
    Returns:
        The absolute path to the directory
    """
    path = Path(directory_path)
    path.mkdir(parents=True, exist_ok=True)
    return str(path.absolute())

def save_conversion_results(
    results: Dict[str, Any], 
    base_filename: str, 
    output_dir: str = "output"
) -> Dict[str, str]:
    """
    Save conversion results to multiple files.
    
    Args:
        results: Dictionary containing conversion results
        base_filename: Base name for the output files
        output_dir: Directory to save files in
        
    Returns:
        Dictionary mapping file types to their paths
    """
    # Ensure output directory exists
    output_path = ensure_directory(output_dir)
    
    # Generate timestamp for unique filenames
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    file_paths = {}
    
    # Save Java code
    if 'final_java_code' in results:
        java_file = os.path.join(output_path, f"{base_filename}_{timestamp}.java")
        with open(java_file, 'w', encoding='utf-8') as f:
            f.write(results['final_java_code'])
        file_paths['java'] = java_file
    
    # Save pseudo code
    if 'pseudo_code' in results:
        pseudo_file = os.path.join(output_path, f"{base_filename}_{timestamp}_pseudo.txt")
        with open(pseudo_file, 'w', encoding='utf-8') as f:
            f.write(results['pseudo_code'])
        file_paths['pseudo'] = pseudo_file
    
    # Save summary
    if 'summary' in results:
        summary_file = os.path.join(output_path, f"{base_filename}_{timestamp}_summary.txt")
        with open(summary_file, 'w', encoding='utf-8') as f:
            f.write(results['summary'])
        file_paths['summary'] = summary_file
    
    # Save complete results as JSON
    json_file = os.path.join(output_path, f"{base_filename}_{timestamp}_complete.json")
    with open(json_file, 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    file_paths['json'] = json_file
    
    return file_paths

def load_cobol_file(file_path: str) -> str:
    """
    Load COBOL code from a file.
    
    Args:
        file_path: Path to the COBOL file
        
    Returns:
        Content of the COBOL file
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return f.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"COBOL file not found: {file_path}")
    except Exception as e:
        raise Exception(f"Error reading COBOL file: {e}")

def save_cobol_file(content: str, file_path: str) -> str:
    """
    Save COBOL code to a file.
    
    Args:
        content: COBOL code content
        file_path: Path to save the file
        
    Returns:
        Absolute path to the saved file
    """
    try:
        # Ensure directory exists
        directory = os.path.dirname(file_path)
        if directory:
            ensure_directory(directory)
        
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        return os.path.abspath(file_path)
    except Exception as e:
        raise Exception(f"Error saving COBOL file: {e}")

def get_file_info(file_path: str) -> Dict[str, Any]:
    """
    Get information about a file.
    
    Args:
        file_path: Path to the file
        
    Returns:
        Dictionary containing file information
    """
    try:
        stat = os.stat(file_path)
        return {
            'path': file_path,
            'size': stat.st_size,
            'modified': datetime.fromtimestamp(stat.st_mtime),
            'created': datetime.fromtimestamp(stat.st_ctime),
            'exists': True
        }
    except FileNotFoundError:
        return {
            'path': file_path,
            'exists': False
        }
    except Exception as e:
        return {
            'path': file_path,
            'error': str(e),
            'exists': False
        }

def format_file_size(size_bytes: int) -> str:
    """
    Format file size in human-readable format.
    
    Args:
        size_bytes: Size in bytes
        
    Returns:
        Formatted size string
    """
    if size_bytes == 0:
        return "0B"
    
    size_names = ["B", "KB", "MB", "GB"]
    i = 0
    while size_bytes >= 1024 and i < len(size_names) - 1:
        size_bytes /= 1024.0
        i += 1
    
    return f"{size_bytes:.1f}{size_names[i]}"

def create_conversion_report(results: Dict[str, Any], file_paths: Dict[str, str]) -> str:
    """
    Create a comprehensive conversion report.
    
    Args:
        results: Conversion results
        file_paths: Paths to saved files
        
    Returns:
        Formatted report string
    """
    report = []
    report.append("=" * 60)
    report.append("COBOL TO JAVA CONVERSION REPORT")
    report.append("=" * 60)
    report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    report.append("")
    
    # Conversion statistics
    report.append("CONVERSION STATISTICS:")
    report.append("-" * 30)
    report.append(f"Status: {results.get('status', 'Unknown')}")
    report.append(f"Review Iterations: {results.get('iterations', 0)}")
    report.append(f"Original COBOL Size: {format_file_size(len(results.get('original_cobol', '')))}")
    report.append(f"Final Java Size: {format_file_size(len(results.get('final_java_code', '')))}")
    report.append("")
    
    # Review comments
    if results.get('review_comments'):
        report.append("REVIEW COMMENTS:")
        report.append("-" * 30)
        for i, comment in enumerate(results['review_comments'], 1):
            report.append(f"{i}. {comment}")
        report.append("")
    
    # Generated files
    report.append("GENERATED FILES:")
    report.append("-" * 30)
    for file_type, file_path in file_paths.items():
        file_info = get_file_info(file_path)
        if file_info['exists']:
            report.append(f"{file_type.upper()}: {file_path}")
            report.append(f"  Size: {format_file_size(file_info['size'])}")
            report.append(f"  Modified: {file_info['modified'].strftime('%Y-%m-%d %H:%M:%S')}")
        else:
            report.append(f"{file_type.upper()}: {file_path} (Error: {file_info.get('error', 'Unknown')})")
        report.append("")
    
    # Summary
    if results.get('summary'):
        report.append("CONVERSION SUMMARY:")
        report.append("-" * 30)
        report.append(results['summary'])
    
    return "\n".join(report)
