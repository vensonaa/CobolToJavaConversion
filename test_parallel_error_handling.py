#!/usr/bin/env python3
"""
Test script to verify parallel processing with error handling
"""

import asyncio
import time
from main import convert_cobol_to_java

# Create a simple COBOL program to test
test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-TEST.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-COUNTER      PIC 9(3) VALUE 0.
           01 WS-RESULT       PIC 9(6).
           01 WS-MESSAGE      PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM PROCESS-DATA
           PERFORM DISPLAY-RESULTS
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           MOVE 'Program initialized successfully' TO WS-MESSAGE
           DISPLAY WS-MESSAGE.
       
       PROCESS-DATA.
           ADD 1 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 10
           DISPLAY 'Processing: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       DISPLAY-RESULTS.
           DISPLAY 'Final counter value: ' WS-COUNTER
           DISPLAY 'Final result value: ' WS-RESULT
           DISPLAY 'Program completed successfully'.
"""

async def test_parallel_error_handling():
    """Test the parallel processing with error handling."""
    print("🧪 Testing Parallel Processing with Error Handling")
    print("=" * 60)
    
    start_time = time.time()
    
    try:
        # Run the conversion
        result = await convert_cobol_to_java(test_cobol, "Test conversion with error handling")
        
        end_time = time.time()
        total_time = end_time - start_time
        
        print("\n✅ Test Results:")
        print(f"📊 Total chunks processed: {result.get('total_chunks', 0)}")
        print(f"⏱️  Total processing time: {total_time:.2f} seconds")
        print(f"📝 Java code length: {len(result.get('final_java_code', ''))} characters")
        print(f"📝 Pseudo code length: {len(result.get('pseudo_code', ''))} characters")
        print(f"📝 Summary: {result.get('summary', '')[:100]}...")
        
        # Show chunk results
        chunk_results = result.get('chunk_results', [])
        if chunk_results:
            print(f"\n📦 Chunk Processing Details:")
            for chunk in chunk_results:
                status = "✅" if "ERROR" not in chunk['java_code'] else "❌"
                print(f"   {status} Chunk {chunk['chunk_number']}: {len(chunk['java_code'])} chars")
        
        print("\n🎉 Parallel processing with error handling test completed successfully!")
        
    except Exception as e:
        print(f"❌ Test failed: {str(e)}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_parallel_error_handling())
