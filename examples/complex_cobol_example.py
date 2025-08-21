#!/usr/bin/env python3
"""
Complex COBOL to Java Conversion Example
This example demonstrates the conversion of a more complex COBOL program
that includes file handling, calculations, and business logic.
"""

import asyncio
import sys
import os

# Add the parent directory to the path to import main module
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from main import convert_cobol_to_java

async def run_complex_example():
    """Run a complex COBOL to Java conversion example."""
    
    # Complex COBOL program - Employee Payroll Processing
    complex_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-PAYROLL.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'employees.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-FILE-STATUS.
           
           SELECT PAYROLL-FILE ASSIGN TO 'payroll.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID           PIC 9(6).
           05  EMP-NAME         PIC X(30).
           05  EMP-HOURLY-RATE  PIC 9(3)V99.
           05  EMP-HOURS-WORKED PIC 9(3)V99.
           05  EMP-DEPT         PIC X(10).
       
       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD.
           05  PAY-EMP-ID       PIC 9(6).
           05  PAY-EMP-NAME     PIC X(30).
           05  PAY-GROSS-PAY    PIC 9(6)V99.
           05  PAY-TAX-AMOUNT   PIC 9(6)V99.
           05  PAY-NET-PAY      PIC 9(6)V99.
           05  PAY-DEPT         PIC X(10).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS       PIC XX.
       01  WS-EOF-FLAG          PIC X VALUE 'N'.
           88  WS-EOF           VALUE 'Y'.
       01  WS-TAX-RATE          PIC V999 VALUE 0.25.
       01  WS-TOTAL-EMPLOYEES   PIC 9(4) VALUE 0.
       01  WS-TOTAL-PAYROLL     PIC 9(8)V99 VALUE 0.
       
       01  WS-DISPLAY-LINE.
           05  FILLER           PIC X(10) VALUE 'Employee: '.
           05  WS-DISP-NAME     PIC X(30).
           05  FILLER           PIC X(10) VALUE ' Gross: $'.
           05  WS-DISP-GROSS    PIC ZZZ,ZZ9.99.
           05  FILLER           PIC X(10) VALUE ' Net: $'.
           05  WS-DISP-NET      PIC ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM PROCESS-EMPLOYEES UNTIL WS-EOF
           PERFORM FINALIZE-PROGRAM
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           OPEN INPUT EMPLOYEE-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening employee file: ' WS-FILE-STATUS
               STOP RUN
           END-IF
           
           OPEN OUTPUT PAYROLL-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening payroll file: ' WS-FILE-STATUS
               STOP RUN
           END-IF
           
           READ EMPLOYEE-FILE
               AT END SET WS-EOF TO TRUE
           END-READ.
       
       PROCESS-EMPLOYEES.
           PERFORM CALCULATE-PAYROLL
           PERFORM WRITE-PAYROLL-RECORD
           PERFORM DISPLAY-EMPLOYEE-INFO
           READ EMPLOYEE-FILE
               AT END SET WS-EOF TO TRUE
           END-READ.
       
       CALCULATE-PAYROLL.
           COMPUTE PAY-GROSS-PAY = EMP-HOURLY-RATE * EMP-HOURS-WORKED
           COMPUTE PAY-TAX-AMOUNT = PAY-GROSS-PAY * WS-TAX-RATE
           COMPUTE PAY-NET-PAY = PAY-GROSS-PAY - PAY-TAX-AMOUNT
           
           MOVE EMP-ID TO PAY-EMP-ID
           MOVE EMP-NAME TO PAY-EMP-NAME
           MOVE EMP-DEPT TO PAY-DEPT
           
           ADD 1 TO WS-TOTAL-EMPLOYEES
           ADD PAY-NET-PAY TO WS-TOTAL-PAYROLL.
       
       WRITE-PAYROLL-RECORD.
           WRITE PAYROLL-RECORD
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error writing payroll record: ' WS-FILE-STATUS
               STOP RUN
           END-IF.
       
       DISPLAY-EMPLOYEE-INFO.
           MOVE EMP-NAME TO WS-DISP-NAME
           MOVE PAY-GROSS-PAY TO WS-DISP-GROSS
           MOVE PAY-NET-PAY TO WS-DISP-NET
           DISPLAY WS-DISPLAY-LINE.
       
       FINALIZE-PROGRAM.
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-FILE
           
           DISPLAY ' '
           DISPLAY 'Payroll Processing Complete'
           DISPLAY 'Total Employees Processed: ' WS-TOTAL-EMPLOYEES
           DISPLAY 'Total Payroll Amount: $' WS-TOTAL-PAYROLL.
    """
    
    # Prior knowledge for the complex conversion
    prior_knowledge = """
    This is a complex COBOL payroll processing program that:
    1. Reads employee data from a sequential file
    2. Calculates gross pay, tax deductions, and net pay
    3. Writes payroll records to an output file
    4. Maintains running totals and displays processing information
    
    Key conversion challenges:
    - File handling (sequential file I/O)
    - Numeric calculations with decimal precision
    - Record structures and data mapping
    - Error handling for file operations
    - Business logic for payroll calculations
    
    Requirements for Java conversion:
    - Use modern Java file I/O (BufferedReader, PrintWriter)
    - Implement proper exception handling
    - Use BigDecimal for precise financial calculations
    - Create appropriate data classes/records
    - Maintain the same business logic and calculations
    - Provide clear console output for monitoring
    """
    
    print("🔄 Starting Complex COBOL to Java Conversion...")
    print("📊 Processing Employee Payroll System")
    print("=" * 60)
    
    try:
        result = await convert_cobol_to_java(complex_cobol, prior_knowledge)
        
        print("\n" + "=" * 60)
        print("🎯 COMPLEX CONVERSION RESULTS")
        print("=" * 60)
        print(f"✅ Status: {result['status']}")
        print(f"🔄 Review Iterations: {result['iterations']}")
        
        print(f"\n📋 PSEUDO CODE ANALYSIS:")
        print("-" * 40)
        print(result['pseudo_code'])
        
        print(f"\n☕ FINAL JAVA CODE:")
        print("-" * 40)
        print(result['final_java_code'])
        
        if result['review_comments']:
            print(f"\n⚠️  REVIEW COMMENTS:")
            print("-" * 40)
            for comment in result['review_comments']:
                print(f"• {comment}")
        
        print(f"\n📝 CONVERSION SUMMARY:")
        print("-" * 40)
        print(result['summary'])
        
        # Save results to file
        save_results_to_file(result, "complex_payroll_conversion")
        
    except Exception as e:
        print(f"❌ Error during conversion: {e}")
        raise

def save_results_to_file(result: dict, filename: str):
    """Save conversion results to files."""
    import os
    
    # Create output directory if it doesn't exist
    os.makedirs("output", exist_ok=True)
    
    # Save Java code
    java_file = f"output/{filename}.java"
    with open(java_file, 'w') as f:
        f.write(result['final_java_code'])
    
    # Save pseudo code
    pseudo_file = f"output/{filename}_pseudo.txt"
    with open(pseudo_file, 'w') as f:
        f.write(result['pseudo_code'])
    
    # Save summary
    summary_file = f"output/{filename}_summary.txt"
    with open(summary_file, 'w') as f:
        f.write(result['summary'])
    
    print(f"\n💾 Results saved to:")
    print(f"   Java Code: {java_file}")
    print(f"   Pseudo Code: {pseudo_file}")
    print(f"   Summary: {summary_file}")

if __name__ == "__main__":
    asyncio.run(run_complex_example())
