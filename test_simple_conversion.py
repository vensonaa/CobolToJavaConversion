#!/usr/bin/env python3
"""
Simple test for COBOL to Java conversion functionality.
"""

import asyncio
import sys
import os

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from main import convert_cobol_to_java

async def test_simple_conversion():
    """Test simple COBOL to Java conversion."""
    
    # Simple COBOL program
    cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-COUNTER PIC 9(3) VALUE 0.
           01 WS-MESSAGE PIC X(20) VALUE 'Hello from COBOL!'.
       
       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE
           ADD 1 TO WS-COUNTER
           DISPLAY 'Counter: ' WS-COUNTER
           STOP RUN.
    """
    
    prior_knowledge = "Simple COBOL program with counter and display operations."
    
    print("🧪 Testing Simple COBOL to Java Conversion...")
    print("=" * 50)
    
    try:
        result = await convert_cobol_to_java(cobol_code, prior_knowledge)
        
        print("✅ Test completed successfully!")
        print(f"Status: {result['status']}")
        print(f"Iterations: {result['iterations']}")
        
        print("\n📋 Generated Java Code:")
        print("-" * 30)
        print(result['final_java_code'])
        
        print("\n📝 Summary:")
        print("-" * 30)
        print(result['summary'][:200] + "...")
        
        return True
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        return False

async def test_error_handling():
    """Test error handling with invalid input."""
    
    print("\n🧪 Testing Error Handling...")
    print("=" * 50)
    
    try:
        # Test with empty COBOL code
        result = await convert_cobol_to_java("", "Empty COBOL code test.")
        print("❌ Should have failed with empty COBOL code")
        return False
        
    except Exception as e:
        print(f"✅ Correctly handled error: {e}")
        return True

async def main():
    """Run all tests."""
    print("🚀 Starting COBOL to Java Conversion Tests")
    print("=" * 60)
    
    # Test 1: Simple conversion
    test1_passed = await test_simple_conversion()
    
    # Test 2: Error handling
    test2_passed = await test_error_handling()
    
    # Summary
    print("\n" + "=" * 60)
    print("📊 TEST RESULTS")
    print("=" * 60)
    print(f"Simple Conversion Test: {'✅ PASSED' if test1_passed else '❌ FAILED'}")
    print(f"Error Handling Test: {'✅ PASSED' if test2_passed else '❌ FAILED'}")
    
    if test1_passed and test2_passed:
        print("\n🎉 All tests passed!")
        return 0
    else:
        print("\n⚠️  Some tests failed!")
        return 1

if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
