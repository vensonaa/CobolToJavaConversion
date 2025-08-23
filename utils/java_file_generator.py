"""
Java File Generator Utility
Generates separate Java files from COBOL to Java conversion results.
"""

import os
import re
from typing import Dict, List, Any
from pathlib import Path
from datetime import datetime

class JavaFileGenerator:
    """Generates separate Java files from conversion results."""
    
    def __init__(self, output_dir: str = "output/java"):
        self.output_dir = output_dir
        self.ensure_output_dir()
    
    def ensure_output_dir(self):
        """Ensure the output directory exists."""
        Path(self.output_dir).mkdir(parents=True, exist_ok=True)
    
    def extract_java_classes(self, java_code: str) -> List[Dict[str, str]]:
        """
        Extract individual Java classes from the combined Java code.
        
        Args:
            java_code: Combined Java code from conversion
            
        Returns:
            List of dictionaries containing class name and code
        """
        classes = []
        
        # Split by chunk separators first
        chunks = java_code.split("// ===== CHUNK SEPARATOR =====\n\n")
        
        for chunk_idx, chunk in enumerate(chunks):
            if not chunk.strip():
                continue
                
            # Extract classes from this chunk
            chunk_classes = self._extract_classes_from_chunk(chunk, chunk_idx)
            classes.extend(chunk_classes)
        
        return classes
    
    def _extract_classes_from_chunk(self, chunk: str, chunk_idx: int) -> List[Dict[str, str]]:
        """Extract classes from a single chunk."""
        classes = []
        
        # Pattern to match Java class definitions
        class_pattern = r'(public\s+)?(class|interface|enum)\s+(\w+)(\s+extends\s+\w+)?(\s+implements\s+[\w\s,]+)?\s*\{'
        
        # Find all class matches
        matches = list(re.finditer(class_pattern, chunk, re.MULTILINE | re.DOTALL))
        
        for i, match in enumerate(matches):
            class_name = match.group(3)
            class_type = match.group(2)
            
            # Find the start and end of this class
            start_pos = match.start()
            end_pos = self._find_class_end(chunk, start_pos)
            
            if end_pos > start_pos:
                class_code = chunk[start_pos:end_pos].strip()
                
                # Add package declaration if missing
                if not class_code.startswith('package'):
                    class_code = f"package com.banking.system;\n\n{class_code}"
                
                classes.append({
                    'name': class_name,
                    'type': class_type,
                    'code': class_code,
                    'chunk': chunk_idx + 1
                })
        
        # If no classes found, create a default class
        if not classes and chunk.strip():
            default_class = self._create_default_class(chunk, chunk_idx)
            classes.append(default_class)
        
        return classes
    
    def _find_class_end(self, code: str, start_pos: int) -> int:
        """Find the end of a Java class."""
        brace_count = 0
        in_string = False
        escape_next = False
        
        for i in range(start_pos, len(code)):
            char = code[i]
            
            if escape_next:
                escape_next = False
                continue
            
            if char == '\\':
                escape_next = True
                continue
            
            if char == '"' and not escape_next:
                in_string = not in_string
                continue
            
            if not in_string:
                if char == '{':
                    brace_count += 1
                elif char == '}':
                    brace_count -= 1
                    if brace_count == 0:
                        return i + 1
        
        return len(code)
    
    def _create_default_class(self, chunk: str, chunk_idx: int) -> Dict[str, str]:
        """Create a default class when no class is found."""
        class_name = f"Chunk{chunk_idx + 1}Code"
        
        default_code = f"""package com.banking.system;

/**
 * Generated from COBOL chunk {chunk_idx + 1}
 * This class contains code converted from COBOL.
 */
public class {class_name} {{
    
    /**
     * Main method for testing the converted code.
     */
    public static void main(String[] args) {{
        System.out.println("Running converted COBOL code from chunk {chunk_idx + 1}");
        
        // Converted COBOL code:
        {chunk}
    }}
}}"""
        
        return {
            'name': class_name,
            'type': 'class',
            'code': default_code,
            'chunk': chunk_idx + 1
        }
    
    def generate_java_files(self, conversion_result: Dict[str, Any]) -> List[str]:
        """
        Generate separate Java files from conversion results.
        
        Args:
            conversion_result: Result from COBOL to Java conversion
            
        Returns:
            List of generated file paths
        """
        java_code = conversion_result.get('final_java_code', '')
        generated_files = []
        
        print(f"=== JAVA FILE GENERATOR DEBUG ===")
        print(f"Input Java code length: {len(java_code)}")
        print(f"Input Java code preview: {java_code[:300]}...")
        
        # If no Java code, create a basic file
        if not java_code.strip():
            print("No Java code provided, creating basic file")
            basic_file = self._generate_basic_java_file(conversion_result)
            if basic_file:
                generated_files.append(basic_file)
            return generated_files
        
        # Extract classes
        classes = self.extract_java_classes(java_code)
        print(f"Extracted {len(classes)} classes")
        
        # Generate files for each class
        for class_info in classes:
            file_path = self._generate_java_file(class_info)
            if file_path:
                generated_files.append(file_path)
        
        # Generate main application class
        main_file = self._generate_main_class(conversion_result, classes)
        if main_file:
            generated_files.append(main_file)
        
        # Generate README
        readme_file = self._generate_readme(conversion_result, classes)
        if readme_file:
            generated_files.append(readme_file)
        
        print(f"Generated {len(generated_files)} total files")
        print(f"=== END JAVA FILE GENERATOR DEBUG ===")
        
        return generated_files
    
    def _generate_java_file(self, class_info: Dict[str, str]) -> str:
        """Generate a single Java file."""
        class_name = class_info['name']
        class_code = class_info['code']
        
        # Create filename
        filename = f"{class_name}.java"
        file_path = os.path.join(self.output_dir, filename)
        
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(class_code)
            
            print(f"✅ Generated: {file_path}")
            return file_path
        except Exception as e:
            print(f"❌ Error generating {file_path}: {e}")
            return ""
    
    def _generate_main_class(self, conversion_result: Dict[str, Any], classes: List[Dict[str, str]]) -> str:
        """Generate main application class."""
        main_code = f"""package com.banking.system;

import java.util.List;
import java.util.ArrayList;

/**
 * Main application class for the Banking System
 * Generated from COBOL conversion on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 */
public class BankingSystemMain {{
    
    public static void main(String[] args) {{
        System.out.println("===============================================");
        System.out.println("           BANKING SYSTEM v2.0");
        System.out.println("===============================================");
        System.out.println("Generated from COBOL conversion");
        System.out.println("Date: {datetime.now().strftime('%Y-%m-%d')}");
        System.out.println("Time: {datetime.now().strftime('%H:%M:%S')}");
        System.out.println();
        
        // Display conversion statistics
        System.out.println("Conversion Statistics:");
        System.out.println("- Original COBOL Size: {len(conversion_result.get('original_cobol', '')):,} characters");
        System.out.println("- Total Chunks Processed: {conversion_result.get('total_chunks', 1)}");
        System.out.println("- Java Classes Generated: {len(classes)}");
        System.out.println();
        
        // List generated classes
        System.out.println("Generated Classes:");
        for (ClassInfo classInfo : getGeneratedClasses()) {{
            System.out.println("- " + classInfo.getName() + " (" + classInfo.getType() + ")");
        }}
        
        System.out.println();
        System.out.println("Banking System initialized successfully!");
    }}
    
    private static List<ClassInfo> getGeneratedClasses() {{
        List<ClassInfo> classes = new ArrayList<>();
"""
        
        for class_info in classes:
            main_code += f"""
        classes.add(new ClassInfo("{class_info['name']}", "{class_info['type']}", {class_info['chunk']}));"""
        
        main_code += """
        
        return classes;
    }
    
    /**
     * Information about generated classes
     */
    public static class ClassInfo {
        private String name;
        private String type;
        private int chunk;
        
        public ClassInfo(String name, String type, int chunk) {
            this.name = name;
            this.type = type;
            this.chunk = chunk;
        }
        
        public String getName() { return name; }
        public String getType() { return type; }
        public int getChunk() { return chunk; }
    }
}
"""
        
        file_path = os.path.join(self.output_dir, "BankingSystemMain.java")
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(main_code)
            
            print(f"✅ Generated: {file_path}")
            return file_path
        except Exception as e:
            print(f"❌ Error generating {file_path}: {e}")
            return ""
    
    def _generate_basic_java_file(self, conversion_result: Dict[str, Any]) -> str:
        """Generate a basic Java file when no Java code is provided."""
        basic_code = f"""package com.banking.system;

/**
 * Basic Java file generated from COBOL conversion
 * Generated on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
 * 
 * Note: No Java code was generated from the COBOL conversion.
 * This may indicate an issue with the conversion process.
 */
public class BasicConversion {{
    
    public static void main(String[] args) {{
        System.out.println("Basic conversion file generated");
        System.out.println("No Java code was generated from COBOL");
        System.out.println("Please check the conversion process");
    }}
    
    /**
     * Placeholder method for converted COBOL functionality
     */
    public void placeholderMethod() {{
        // This method would contain converted COBOL code
        System.out.println("Placeholder for converted COBOL code");
    }}
}}"""
        
        file_path = os.path.join(self.output_dir, "BasicConversion.java")
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(basic_code)
            
            print(f"✅ Generated basic file: {file_path}")
            return file_path
        except Exception as e:
            print(f"❌ Error generating basic file {file_path}: {e}")
            return ""
    
    def _generate_readme(self, conversion_result: Dict[str, Any], classes: List[Dict[str, str]]) -> str:
        """Generate README file for the Java project."""
        readme_content = f"""# Banking System - Java Implementation

This Java project was automatically generated from COBOL code using a LangGraph multi-agent conversion system.

## Conversion Details

- **Original COBOL Size**: {len(conversion_result.get('original_cobol', '')):,} characters
- **Total Chunks Processed**: {conversion_result.get('total_chunks', 1)}
- **Java Classes Generated**: {len(classes)}
- **Conversion Date**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Generated Classes

"""
        
        for class_info in classes:
            readme_content += f"- **{class_info['name']}** ({class_info['type']}) - Generated from chunk {class_info['chunk']}\n"
        
        readme_content += f"""

## Project Structure

```
{self.output_dir}/
├── BankingSystemMain.java    # Main application entry point
"""
        
        for class_info in classes:
            readme_content += f"├── {class_info['name']}.java\n"
        
        readme_content += """└── README.md                 # This file
```

## Running the Application

```bash
# Compile all Java files
javac -d bin *.java

# Run the main application
java -cp bin com.banking.system.BankingSystemMain
```

## Features

- Customer Management
- Account Management  
- Transaction Processing
- Loan Management
- Report Generation
- File I/O Operations

## Notes

This code was automatically generated and may require manual review and refinement for production use.
"""
        
        file_path = os.path.join(self.output_dir, "README.md")
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(readme_content)
            
            print(f"✅ Generated: {file_path}")
            return file_path
        except Exception as e:
            print(f"❌ Error generating {file_path}: {e}")
            return ""
