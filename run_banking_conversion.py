#!/usr/bin/env python3
"""
Run COBOL to Java conversion on the large banking system program.
"""

import asyncio
import sys
import os

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from main import convert_cobol_to_java
from utils.java_file_generator import JavaFileGenerator
from utils.file_utils import save_conversion_results, create_conversion_report

async def run_banking_conversion():
    """Run conversion on the large banking system COBOL program."""
    
    # Load the large COBOL program
    cobol_file_path = "samples/banking_system.cbl"
    
    try:
        with open(cobol_file_path, 'r', encoding='utf-8') as f:
            cobol_code = f.read()
    except FileNotFoundError:
        print(f"❌ Error: COBOL file not found: {cobol_file_path}")
        return
    except Exception as e:
        print(f"❌ Error reading COBOL file: {e}")
        return
    
    # Prior knowledge for the banking system
    prior_knowledge = """
    This is a comprehensive banking system written in COBOL that includes:
    
    1. Customer Management:
       - Customer records with ID, name, address, phone, email, DOB, SSN
       - Customer status tracking (active/inactive)
       - CRUD operations for customers
    
    2. Account Management:
       - Multiple account types (Savings, Checking, Loan, Credit)
       - Account balances and interest rates
       - Account status tracking (active/frozen/closed)
       - Account opening, closing, and updating
    
    3. Transaction Processing:
       - Multiple transaction types (Deposit, Withdrawal, Transfer, Payment)
       - Transaction history and status tracking
       - Balance calculations and validations
    
    4. Loan Management:
       - Multiple loan types (Personal, Mortgage, Business, Car)
       - Loan calculations including monthly payments
       - Loan status tracking (active/paid-off/default)
       - Payment processing and balance tracking
    
    5. File I/O Operations:
       - Sequential file handling for all data
       - File status checking and error handling
       - Multiple input and output files
    
    6. Report Generation:
       - Customer reports
       - Account reports
       - Transaction reports
       - Loan reports
       - Summary reports
    
    7. Menu-driven Interface:
       - Interactive menu system
       - User input validation
       - Error handling and user feedback
    
    Key conversion requirements:
    - Convert to multiple Java classes (Customer, Account, Transaction, Loan, etc.)
    - Implement proper object-oriented design
    - Use modern Java patterns and best practices
    - Handle file I/O with Java streams
    - Implement proper exception handling
    - Create a user interface (console-based or GUI)
    - Use BigDecimal for financial calculations
    - Implement proper data validation
    - Create comprehensive documentation
    """
    
    print("🏦 Starting Large Banking System COBOL to Java Conversion...")
    print("=" * 80)
    print(f"📝 COBOL Code Length: {len(cobol_code):,} characters")
    print(f"📄 COBOL Lines: {cobol_code.count(chr(10)) + 1:,} lines")
    print("🔄 This will generate multiple Java classes...")
    print("=" * 80)
    
    try:
        # Run the conversion
        result = await convert_cobol_to_java(cobol_code, prior_knowledge)
        
        print("\n" + "=" * 80)
        print("🎯 BANKING SYSTEM CONVERSION RESULTS")
        print("=" * 80)
        print(f"✅ Status: {result['status']}")
        print(f"🔄 Review Iterations: {result['iterations']}")
        print(f"📝 Original COBOL Size: {len(result['original_cobol']):,} characters")
        print(f"☕ Final Java Size: {len(result['final_java_code']):,} characters")
        print(f"📦 Total Chunks Processed: {result['total_chunks']}")
        
        # Generate separate Java files
        print("\n💾 Generating separate Java files...")
        generator = JavaFileGenerator("output/java/banking_system")
        java_files = generator.generate_java_files(result)
        
        print(f"✅ Generated {len(java_files)} Java files:")
        for file_path in java_files:
            print(f"   📄 {os.path.basename(file_path)}")
        
        # Save results
        print("\n💾 Saving conversion results...")
        file_paths = save_conversion_results(result, "banking_system", "output")
        
        # Create and save comprehensive report
        report = create_conversion_report(result, file_paths)
        report_path = os.path.join("output", "banking_system_comprehensive_report.txt")
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write(report)
        
        print(f"\n📋 PSEUDO CODE ANALYSIS:")
        print("-" * 50)
        print(result['pseudo_code'][:500] + "..." if len(result['pseudo_code']) > 500 else result['pseudo_code'])
        
        print(f"\n☕ JAVA CODE PREVIEW:")
        print("-" * 50)
        java_preview = result['final_java_code'][:1000] + "..." if len(result['final_java_code']) > 1000 else result['final_java_code']
        print(java_preview)
        
        if result['review_comments']:
            print(f"\n⚠️  REVIEW COMMENTS:")
            print("-" * 50)
            for i, comment in enumerate(result['review_comments'], 1):
                print(f"{i}. {comment[:200]}..." if len(comment) > 200 else f"{i}. {comment}")
        
        print(f"\n📝 CONVERSION SUMMARY:")
        print("-" * 50)
        summary_preview = result['summary'][:500] + "..." if len(result['summary']) > 500 else result['summary']
        print(summary_preview)
        
        print(f"\n💾 Files saved to:")
        for file_type, file_path in file_paths.items():
            print(f"   {file_type.upper()}: {file_path}")
        print(f"   REPORT: {report_path}")
        
        print(f"\n🎉 Banking system conversion completed successfully!")
        print(f"📊 The system generated multiple Java classes including:")
        print(f"   - Customer.java")
        print(f"   - Account.java") 
        print(f"   - Transaction.java")
        print(f"   - Loan.java")
        print(f"   - BankingSystem.java")
        print(f"   - FileManager.java")
        print(f"   - ReportGenerator.java")
        print(f"   - MenuSystem.java")
        print(f"   - BankingSystemMain.java")
        
        print(f"\n🚀 To run the generated Java application:")
        print(f"   cd output/java/banking_system")
        print(f"   javac -d bin *.java")
        print(f"   java -cp bin com.banking.system.BankingSystemMain")
        
    except Exception as e:
        print(f"❌ Error during conversion: {e}")
        import traceback
        traceback.print_exc()
        raise

if __name__ == "__main__":
    asyncio.run(run_banking_conversion())
