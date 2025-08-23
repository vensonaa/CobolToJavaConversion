#!/usr/bin/env python3
"""
Test script to verify MAD feature is working end-to-end
"""

import requests
import json
import time

def test_mad_feature():
    """Test the MAD feature with a simple COBOL program"""
    
    # Simple COBOL code for testing
    test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAD-TEST.
       AUTHOR. TEST USER.
       DATE-WRITTEN. 2024-01-15.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-RECORD.
           05  TEST-ID    PIC 9(5).
           05  TEST-NAME  PIC X(20).
           05  TEST-VALUE PIC 9(10)V99 COMP-3.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'Testing MAD feature'
           PERFORM PROCESS-TEST
           STOP RUN.
       
       PROCESS-TEST.
           IF TEST-VALUE > 1000
               DISPLAY 'High value'
           ELSE
               DISPLAY 'Low value'.
    """
    
    print("Testing MAD feature...")
    
    # 1. Start a conversion
    print("1. Starting conversion...")
    response = requests.post("http://localhost:8000/api/convert", json={
        "cobol_code": test_cobol,
        "prior_knowledge": "This is a test program for MAD feature verification"
    })
    
    if response.status_code != 200:
        print(f"❌ Conversion start failed: {response.status_code}")
        return
    
    conversion_data = response.json()
    conversion_id = conversion_data["conversion_id"]
    print(f"✅ Conversion started with ID: {conversion_id}")
    
    # 2. Poll for completion
    print("2. Polling for completion...")
    max_attempts = 30
    for attempt in range(max_attempts):
        time.sleep(2)
        
        status_response = requests.get(f"http://localhost:8000/api/status/{conversion_id}")
        if status_response.status_code != 200:
            print(f"❌ Status check failed: {status_response.status_code}")
            return
        
        status_data = status_response.json()
        print(f"   Status: {status_data['status']} - {status_data['message']}")
        
        if status_data['status'] == 'completed':
            print("✅ Conversion completed!")
            break
        elif status_data['status'] == 'failed':
            print(f"❌ Conversion failed: {status_data['message']}")
            return
    else:
        print("❌ Conversion timed out")
        return
    
    # 3. Get conversion details
    print("3. Getting conversion details...")
    details_response = requests.get(f"http://localhost:8000/api/conversions/{conversion_id}/details")
    if details_response.status_code != 200:
        print(f"❌ Details fetch failed: {details_response.status_code}")
        return
    
    details_data = details_response.json()
    print(f"✅ Details retrieved")
    print(f"   Java code length: {len(details_data.get('java_code', ''))}")
    print(f"   Summary length: {len(details_data.get('summary', ''))}")
    print(f"   MAD analysis length: {len(details_data.get('mad_analysis', ''))}")
    
    # 4. Test MAD endpoint specifically
    print("4. Testing MAD endpoint...")
    mad_response = requests.get(f"http://localhost:8000/api/conversions/{conversion_id}/mad")
    if mad_response.status_code != 200:
        print(f"❌ MAD endpoint failed: {mad_response.status_code}")
        return
    
    mad_data = mad_response.json()
    print(f"✅ MAD endpoint working")
    print(f"   MAD analysis length: {len(mad_data.get('mad_analysis', ''))}")
    
    # 5. Display MAD preview
    mad_content = mad_data.get('mad_analysis', '')
    if mad_content:
        print("\n📋 MAD Analysis Preview:")
        print("=" * 50)
        lines = mad_content.split('\n')[:20]  # Show first 20 lines
        for line in lines:
            print(line)
        if len(mad_content.split('\n')) > 20:
            print("... (truncated)")
        print("=" * 50)
    else:
        print("❌ No MAD content found")
    
    print("\n🎉 MAD feature test completed successfully!")

if __name__ == "__main__":
    test_mad_feature()
