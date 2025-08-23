"""
Mainframe Analysis Document (MAD) Generator

This module generates comprehensive analysis documents for COBOL code,
providing insights into the structure, dependencies, and modernization requirements.
"""

import re
from typing import Dict, List, Any
from datetime import datetime

class MADGenerator:
    """Generates Mainframe Analysis Documents for COBOL code"""
    
    def __init__(self):
        self.cobol_keywords = {
            'IDENTIFICATION', 'DIVISION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'DATE-COMPILED', 'SECURITY',
            'WORKING-STORAGE', 'LINKAGE', 'FILE', 'FD', 'SD', 'RD', '01', '77',
            'PIC', 'PICTURE', 'COMP', 'COMP-3', 'VALUE', 'OCCURS', 'REDEFINES',
            'COPY', 'INCLUDE', 'CALL', 'PERFORM', 'IF', 'ELSE', 'END-IF',
            'EVALUATE', 'WHEN', 'END-EVALUATE', 'MOVE', 'ADD', 'SUBTRACT',
            'MULTIPLY', 'DIVIDE', 'COMPUTE', 'READ', 'WRITE', 'OPEN', 'CLOSE',
            'ACCEPT', 'DISPLAY', 'STOP', 'EXIT', 'GOBACK', 'RETURN'
        }
        
    def generate_mad(self, cobol_code: str, prior_knowledge: str = "") -> str:
        """
        Generate a comprehensive Mainframe Analysis Document
        
        Args:
            cobol_code: The COBOL source code to analyze
            prior_knowledge: Additional context or requirements
            
        Returns:
            A formatted MAD document as a string
        """
        analysis = self._analyze_cobol_code(cobol_code)
        
        mad_document = self._format_mad_document(analysis, prior_knowledge)
        
        return mad_document
    
    def _analyze_cobol_code(self, cobol_code: str) -> Dict[str, Any]:
        """Analyze COBOL code and extract key information"""
        lines = cobol_code.split('\n')
        
        analysis = {
            'program_info': self._extract_program_info(lines),
            'data_structures': self._extract_data_structures(lines),
            'file_operations': self._extract_file_operations(lines),
            'business_logic': self._extract_business_logic(lines),
            'dependencies': self._extract_dependencies(lines),
            'complexity_metrics': self._calculate_complexity_metrics(lines),
            'modernization_risks': self._identify_modernization_risks(lines),
            'recommendations': []
        }
        
        # Generate recommendations based on analysis
        analysis['recommendations'] = self._generate_recommendations(analysis)
        
        return analysis
    
    def _extract_program_info(self, lines: List[str]) -> Dict[str, Any]:
        """Extract basic program information"""
        program_info = {
            'name': 'Unknown',
            'author': 'Unknown',
            'date_written': 'Unknown',
            'description': 'No description available'
        }
        
        for line in lines:
            line_upper = line.upper().strip()
            
            if line_upper.startswith('PROGRAM-ID'):
                match = re.search(r'PROGRAM-ID\.\s*(\w+)', line_upper)
                if match:
                    program_info['name'] = match.group(1)
            
            elif line_upper.startswith('AUTHOR'):
                match = re.search(r'AUTHOR\.\s*(.+)', line)
                if match:
                    program_info['author'] = match.group(1).strip()
            
            elif line_upper.startswith('DATE-WRITTEN'):
                match = re.search(r'DATE-WRITTEN\.\s*(.+)', line)
                if match:
                    program_info['date_written'] = match.group(1).strip()
        
        return program_info
    
    def _extract_data_structures(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract data structure information"""
        data_structures = []
        current_structure = None
        
        for line_num, line in enumerate(lines, 1):
            line_upper = line.upper().strip()
            
            # Look for level numbers (01, 77, etc.)
            level_match = re.match(r'^(\d{2})\s+(\w+)', line_upper)
            if level_match:
                level = level_match.group(1)
                name = level_match.group(2)
                
                # Extract picture clause
                pic_match = re.search(r'PIC\w*\s+([A-Z9X]+)', line_upper)
                picture = pic_match.group(1) if pic_match else 'Unknown'
                
                # Extract value clause
                value_match = re.search(r'VALUE\s+([^\.]+)', line_upper)
                value = value_match.group(1).strip() if value_match else None
                
                structure = {
                    'level': level,
                    'name': name,
                    'picture': picture,
                    'value': value,
                    'line_number': line_num
                }
                
                if level == '01':
                    current_structure = structure
                    data_structures.append(structure)
                elif current_structure:
                    if 'sub_fields' not in current_structure:
                        current_structure['sub_fields'] = []
                    current_structure['sub_fields'].append(structure)
        
        return data_structures
    
    def _extract_file_operations(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract file operation information"""
        file_operations = []
        
        for line_num, line in enumerate(lines, 1):
            line_upper = line.upper().strip()
            
            if line_upper.startswith('FD ') or line_upper.startswith('SELECT '):
                # Extract file name
                file_match = re.search(r'(?:FD|SELECT)\s+(\w+)', line_upper)
                if file_match:
                    file_name = file_match.group(1)
                    file_operations.append({
                        'name': file_name,
                        'type': 'FD' if line_upper.startswith('FD ') else 'SELECT',
                        'line_number': line_num
                    })
        
        return file_operations
    
    def _extract_business_logic(self, lines: List[str]) -> Dict[str, Any]:
        """Extract business logic patterns"""
        logic_patterns = {
            'perform_statements': 0,
            'if_statements': 0,
            'evaluate_statements': 0,
            'call_statements': 0,
            'arithmetic_operations': 0,
            'file_operations': 0
        }
        
        for line in lines:
            line_upper = line.upper().strip()
            
            if 'PERFORM' in line_upper:
                logic_patterns['perform_statements'] += 1
            if line_upper.startswith('IF ') or 'END-IF' in line_upper:
                logic_patterns['if_statements'] += 1
            if 'EVALUATE' in line_upper or 'WHEN' in line_upper:
                logic_patterns['evaluate_statements'] += 1
            if 'CALL' in line_upper:
                logic_patterns['call_statements'] += 1
            if any(op in line_upper for op in ['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE']):
                logic_patterns['arithmetic_operations'] += 1
            if any(op in line_upper for op in ['READ', 'WRITE', 'OPEN', 'CLOSE']):
                logic_patterns['file_operations'] += 1
        
        return logic_patterns
    
    def _extract_dependencies(self, lines: List[str]) -> List[str]:
        """Extract program dependencies"""
        dependencies = []
        
        for line in lines:
            line_upper = line.upper().strip()
            
            if 'COPY' in line_upper:
                copy_match = re.search(r'COPY\s+(\w+)', line_upper)
                if copy_match:
                    dependencies.append(f"COPY {copy_match.group(1)}")
            
            elif 'CALL' in line_upper:
                call_match = re.search(r'CALL\s+[\'"]?(\w+)[\'"]?', line_upper)
                if call_match:
                    dependencies.append(f"CALL {call_match.group(1)}")
        
        return list(set(dependencies))  # Remove duplicates
    
    def _calculate_complexity_metrics(self, lines: List[str]) -> Dict[str, Any]:
        """Calculate code complexity metrics"""
        total_lines = len(lines)
        code_lines = len([line for line in lines if line.strip() and not line.strip().startswith('*')])
        comment_lines = len([line for line in lines if line.strip().startswith('*')])
        
        # Calculate cyclomatic complexity (simplified)
        decision_points = 0
        for line in lines:
            line_upper = line.upper().strip()
            if any(keyword in line_upper for keyword in ['IF ', 'EVALUATE', 'PERFORM UNTIL', 'PERFORM VARYING']):
                decision_points += 1
        
        return {
            'total_lines': total_lines,
            'code_lines': code_lines,
            'comment_lines': comment_lines,
            'comment_ratio': comment_lines / total_lines if total_lines > 0 else 0,
            'cyclomatic_complexity': decision_points + 1,
            'complexity_level': 'Low' if decision_points <= 5 else 'Medium' if decision_points <= 10 else 'High'
        }
    
    def _identify_modernization_risks(self, lines: List[str]) -> List[Dict[str, str]]:
        """Identify potential modernization risks"""
        risks = []
        
        for line_num, line in enumerate(lines, 1):
            line_upper = line.upper().strip()
            
            # Check for deprecated features
            if 'COMP-3' in line_upper:
                risks.append({
                    'type': 'Deprecated Data Type',
                    'description': 'COMP-3 usage may need conversion to modern numeric types',
                    'line': line_num,
                    'severity': 'Medium'
                })
            
            if 'GO TO' in line_upper:
                risks.append({
                    'type': 'Unstructured Control Flow',
                    'description': 'GO TO statements should be replaced with structured programming constructs',
                    'line': line_num,
                    'severity': 'High'
                })
            
            if 'ALTER' in line_upper:
                risks.append({
                    'type': 'Self-Modifying Code',
                    'description': 'ALTER statements create self-modifying code that is difficult to maintain',
                    'line': line_num,
                    'severity': 'High'
                })
        
        return risks
    
    def _generate_recommendations(self, analysis: Dict[str, Any]) -> List[str]:
        """Generate modernization recommendations"""
        recommendations = []
        
        # Complexity-based recommendations
        complexity = analysis['complexity_metrics']['complexity_level']
        if complexity == 'High':
            recommendations.append("Consider breaking down the program into smaller, more manageable modules")
        
        # Risk-based recommendations
        if analysis['modernization_risks']:
            recommendations.append("Address high-severity modernization risks before conversion")
        
        # Structure-based recommendations
        if len(analysis['data_structures']) > 10:
            recommendations.append("Consider creating separate data definition modules")
        
        # Dependency-based recommendations
        if len(analysis['dependencies']) > 5:
            recommendations.append("Review and potentially consolidate external dependencies")
        
        # General recommendations
        recommendations.extend([
            "Implement comprehensive unit testing for converted Java code",
            "Consider using modern Java frameworks for better maintainability",
            "Document business rules and requirements clearly",
            "Plan for gradual migration rather than big-bang conversion"
        ])
        
        return recommendations
    
    def _format_mad_document(self, analysis: Dict[str, Any], prior_knowledge: str) -> str:
        """Format the analysis into a comprehensive MAD document"""
        current_date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        mad = f"""
MAINFRAME ANALYSIS DOCUMENT (MAD)
Generated on: {current_date}
=====================================

1. PROGRAM OVERVIEW
-------------------
Program Name: {analysis['program_info']['name']}
Author: {analysis['program_info']['author']}
Date Written: {analysis['program_info']['date_written']}

2. CODE COMPLEXITY ANALYSIS
---------------------------
Total Lines: {analysis['complexity_metrics']['total_lines']}
Code Lines: {analysis['complexity_metrics']['code_lines']}
Comment Lines: {analysis['complexity_metrics']['comment_lines']}
Comment Ratio: {analysis['complexity_metrics']['comment_ratio']:.1%}
Cyclomatic Complexity: {analysis['complexity_metrics']['cyclomatic_complexity']}
Complexity Level: {analysis['complexity_metrics']['complexity_level']}

3. DATA STRUCTURES
------------------
Found {len(analysis['data_structures'])} main data structures:
"""
        
        for i, structure in enumerate(analysis['data_structures'][:5], 1):  # Show first 5
            mad += f"{i}. {structure['name']} (Level {structure['level']}, PIC: {structure['picture']})\n"
        
        if len(analysis['data_structures']) > 5:
            mad += f"... and {len(analysis['data_structures']) - 5} more structures\n"
        
        mad += f"""
4. BUSINESS LOGIC PATTERNS
--------------------------
Perform Statements: {analysis['business_logic']['perform_statements']}
If Statements: {analysis['business_logic']['if_statements']}
Evaluate Statements: {analysis['business_logic']['evaluate_statements']}
Call Statements: {analysis['business_logic']['call_statements']}
Arithmetic Operations: {analysis['business_logic']['arithmetic_operations']}
File Operations: {analysis['business_logic']['file_operations']}

5. DEPENDENCIES
---------------
Found {len(analysis['dependencies'])} dependencies:
"""
        
        for dep in analysis['dependencies']:
            mad += f"  - {dep}\n"
        
        mad += f"""
6. MODERNIZATION RISKS
----------------------
Found {len(analysis['modernization_risks'])} potential risks:
"""
        
        for risk in analysis['modernization_risks']:
            mad += f"  - {risk['type']} (Line {risk['line']}, Severity: {risk['severity']})\n"
            mad += f"    Description: {risk['description']}\n"
        
        mad += f"""
7. RECOMMENDATIONS
------------------
"""
        
        for i, rec in enumerate(analysis['recommendations'], 1):
            mad += f"{i}. {rec}\n"
        
        if prior_knowledge:
            mad += f"""
8. ADDITIONAL CONTEXT
---------------------
Prior Knowledge/Requirements:
{prior_knowledge}

9. CONVERSION STRATEGY
----------------------
Based on the analysis above, consider the following conversion approach:
- Phase 1: Address high-severity risks and dependencies
- Phase 2: Create Java data models and utility classes
- Phase 3: Implement core business logic
- Phase 4: Add comprehensive testing and documentation
- Phase 5: Performance optimization and deployment

10. ESTIMATED EFFORT
--------------------
Based on complexity metrics:
- Development Time: {'2-4 weeks' if analysis['complexity_metrics']['complexity_level'] == 'Low' else '4-8 weeks' if analysis['complexity_metrics']['complexity_level'] == 'Medium' else '8-16 weeks'}
- Testing Time: {'1-2 weeks' if analysis['complexity_metrics']['complexity_level'] == 'Low' else '2-4 weeks' if analysis['complexity_metrics']['complexity_level'] == 'Medium' else '4-8 weeks'}
- Documentation: {'1 week' if analysis['complexity_metrics']['complexity_level'] == 'Low' else '1-2 weeks' if analysis['complexity_metrics']['complexity_level'] == 'Medium' else '2-4 weeks'}

=====================================
End of Mainframe Analysis Document
"""
        
        return mad.strip()
