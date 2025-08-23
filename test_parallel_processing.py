#!/usr/bin/env python3
"""
Test script to verify parallel chunk processing
"""

import asyncio
import time
from main import convert_cobol_to_java

# Create a large COBOL program with multiple sections to test chunking
test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PARALLEL-PROCESSING.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-COUNTER      PIC 9(3) VALUE 0.
           01 WS-RESULT       PIC 9(6).
           01 WS-MESSAGE      PIC X(50).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM PROCESS-DATA-1
           PERFORM PROCESS-DATA-2
           PERFORM PROCESS-DATA-3
           PERFORM PROCESS-DATA-4
           PERFORM PROCESS-DATA-5
           PERFORM DISPLAY-RESULTS
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           MOVE 'Program initialized successfully' TO WS-MESSAGE
           DISPLAY WS-MESSAGE.
       
       PROCESS-DATA-1.
           ADD 1 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 10
           DISPLAY 'Processing 1: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       PROCESS-DATA-2.
           ADD 2 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 20
           DISPLAY 'Processing 2: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       PROCESS-DATA-3.
           ADD 3 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 30
           DISPLAY 'Processing 3: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       PROCESS-DATA-4.
           ADD 4 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 40
           DISPLAY 'Processing 4: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       PROCESS-DATA-5.
           ADD 5 TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * 50
           DISPLAY 'Processing 5: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
       
       DISPLAY-RESULTS.
           DISPLAY 'Final counter value: ' WS-COUNTER
           DISPLAY 'Final result value: ' WS-RESULT
           DISPLAY 'Program completed successfully'.
"""

# Add more sections to ensure multiple chunks
for i in range(6, 21):
    test_cobol += f"""
       PROCESS-DATA-{i}.
           ADD {i} TO WS-COUNTER
           COMPUTE WS-RESULT = WS-COUNTER * {i * 10}
           DISPLAY 'Processing {i}: Counter = ' WS-COUNTER ' Result = ' WS-RESULT.
"""

async def test_parallel_processing():
    """Test the parallel processing functionality."""
    print("🧪 Testing Parallel Chunk Processing")
    print("=" * 50)
    
    start_time = time.time()
    
    try:
        # Run the conversion
        result = await convert_cobol_to_java(test_cobol, "Test conversion with parallel processing")
        
        end_time = time.time()
        total_time = end_time - start_time
        
        print("\n✅ Test Results:")
        print(f"📊 Total chunks processed: {result.get('total_chunks', 0)}")
        print(f"⏱️  Total processing time: {total_time:.2f} seconds")
        print(f"📝 Java code length: {len(result.get('final_java_code', ''))} characters")
        print(f"📝 Pseudo code length: {len(result.get('pseudo_code', ''))} characters")
        
        # Show chunk results
        chunk_results = result.get('chunk_results', [])
        if chunk_results:
            print(f"\n📦 Chunk Processing Details:")
            for chunk in chunk_results:
                print(f"   Chunk {chunk['chunk_number']}: {len(chunk['java_code'])} chars")
        
        print("\n🎉 Parallel processing test completed successfully!")
        
    except Exception as e:
        print(f"❌ Test failed: {str(e)}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_parallel_processing())
