       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NAME         PIC X(20) VALUE 'John Doe'.
           01 WS-AGE          PIC 9(3)  VALUE 25.
           01 WS-SALARY       PIC 9(6)V99 VALUE 50000.00.
           01 WS-MESSAGE      PIC X(50).
           01 WS-COUNTER      PIC 9(3)  VALUE 0.
       
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
           IF WS-AGE > 18
               DISPLAY 'Adult: ' WS-NAME
           ELSE
               DISPLAY 'Minor: ' WS-NAME
           END-IF
           
           IF WS-SALARY > 40000
               DISPLAY 'High earner: $' WS-SALARY
           ELSE
               DISPLAY 'Standard salary: $' WS-SALARY
           END-IF.
       
       DISPLAY-RESULTS.
           DISPLAY 'Final counter value: ' WS-COUNTER
           DISPLAY 'Program completed successfully'.
