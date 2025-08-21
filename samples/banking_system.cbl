       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'customers.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-CUSTOMER-STATUS.
           
           SELECT ACCOUNT-FILE ASSIGN TO 'accounts.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-ACCOUNT-STATUS.
           
           SELECT TRANSACTION-FILE ASSIGN TO 'transactions.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-TRANSACTION-STATUS.
           
           SELECT LOAN-FILE ASSIGN TO 'loans.dat'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-LOAN-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO 'banking_report.txt'
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-REPORT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID           PIC 9(8).
           05  CUST-NAME         PIC X(50).
           05  CUST-ADDRESS      PIC X(100).
           05  CUST-PHONE        PIC X(15).
           05  CUST-EMAIL        PIC X(50).
           05  CUST-DOB          PIC 9(8).
           05  CUST-SSN          PIC 9(9).
           05  CUST-STATUS       PIC X(1).
               88  CUST-ACTIVE   VALUE 'A'.
               88  CUST-INACTIVE VALUE 'I'.
           05  CUST-CREATE-DATE  PIC 9(8).
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACC-ID            PIC 9(12).
           05  ACC-CUST-ID       PIC 9(8).
           05  ACC-TYPE          PIC X(2).
               88  ACC-SAVINGS   VALUE 'SV'.
               88  ACC-CHECKING  VALUE 'CH'.
               88  ACC-LOAN      VALUE 'LN'.
               88  ACC-CREDIT    VALUE 'CR'.
           05  ACC-BALANCE       PIC 9(10)V99.
           05  ACC-INTEREST-RATE PIC 9(3)V99.
           05  ACC-STATUS        PIC X(1).
               88  ACC-ACTIVE    VALUE 'A'.
               88  ACC-FROZEN    VALUE 'F'.
               88  ACC-CLOSED    VALUE 'C'.
           05  ACC-OPEN-DATE     PIC 9(8).
           05  ACC-LAST-TRANS    PIC 9(8).
       
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TRANS-ID          PIC 9(12).
           05  TRANS-ACC-ID      PIC 9(12).
           05  TRANS-TYPE        PIC X(2).
               88  TRANS-DEPOSIT VALUE 'DP'.
               88  TRANS-WITHDRAW VALUE 'WD'.
               88  TRANS-TRANSFER VALUE 'TR'.
               88  TRANS-PAYMENT VALUE 'PM'.
           05  TRANS-AMOUNT      PIC 9(10)V99.
           05  TRANS-DATE        PIC 9(8).
           05  TRANS-TIME        PIC 9(6).
           05  TRANS-DESCRIPTION PIC X(50).
           05  TRANS-STATUS      PIC X(1).
               88  TRANS-PENDING VALUE 'P'.
               88  TRANS-COMPLETED VALUE 'C'.
               88  TRANS-FAILED  VALUE 'F'.
       
       FD  LOAN-FILE.
       01  LOAN-RECORD.
           05  LOAN-ID           PIC 9(12).
           05  LOAN-CUST-ID      PIC 9(8).
           05  LOAN-TYPE         PIC X(2).
               88  LOAN-PERSONAL VALUE 'PL'.
               88  LOAN-MORTGAGE VALUE 'MG'.
               88  LOAN-BUSINESS VALUE 'BL'.
               88  LOAN-CAR      VALUE 'CL'.
           05  LOAN-AMOUNT       PIC 9(10)V99.
           05  LOAN-INTEREST-RATE PIC 9(3)V99.
           05  LOAN-TERM         PIC 9(3).
           05  LOAN-MONTHLY-PAYMENT PIC 9(8)V99.
           05  LOAN-BALANCE      PIC 9(10)V99.
           05  LOAN-STATUS       PIC X(1).
               88  LOAN-ACTIVE   VALUE 'A'.
               88  LOAN-PAID-OFF VALUE 'P'.
               88  LOAN-DEFAULT  VALUE 'D'.
           05  LOAN-OPEN-DATE    PIC 9(8).
           05  LOAN-DUE-DATE     PIC 9(8).
       
       FD  REPORT-FILE.
       01  REPORT-RECORD         PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-CUSTOMER-STATUS    PIC XX.
           05  WS-ACCOUNT-STATUS     PIC XX.
           05  WS-TRANSACTION-STATUS PIC XX.
           05  WS-LOAN-STATUS        PIC XX.
           05  WS-REPORT-STATUS      PIC XX.
       
       01  WS-EOF-FLAGS.
           05  WS-CUSTOMER-EOF       PIC X VALUE 'N'.
               88  WS-CUSTOMER-EOF-YES VALUE 'Y'.
           05  WS-ACCOUNT-EOF        PIC X VALUE 'N'.
               88  WS-ACCOUNT-EOF-YES VALUE 'Y'.
           05  WS-TRANSACTION-EOF    PIC X VALUE 'N'.
               88  WS-TRANSACTION-EOF-YES VALUE 'Y'.
           05  WS-LOAN-EOF           PIC X VALUE 'N'.
               88  WS-LOAN-EOF-YES VALUE 'Y'.
       
       01  WS-COUNTERS.
           05  WS-CUSTOMER-COUNT     PIC 9(6) VALUE 0.
           05  WS-ACCOUNT-COUNT      PIC 9(6) VALUE 0.
           05  WS-TRANSACTION-COUNT  PIC 9(6) VALUE 0.
           05  WS-LOAN-COUNT         PIC 9(6) VALUE 0.
           05  WS-TOTAL-BALANCE      PIC 9(12)V99 VALUE 0.
           05  WS-TOTAL-LOANS        PIC 9(12)V99 VALUE 0.
       
       01  WS-CURRENT-DATE          PIC 9(8).
       01  WS-CURRENT-TIME          PIC 9(6).
       01  WS-REPORT-DATE           PIC X(10).
       
       01  WS-DISPLAY-LINE.
           05  FILLER               PIC X(20) VALUE 'Processing: '.
           05  WS-DISPLAY-COUNT     PIC ZZZ,ZZ9.
           05  FILLER               PIC X(10) VALUE ' records'.
       
       01  WS-ERROR-MESSAGE         PIC X(100).
       01  WS-SUCCESS-MESSAGE       PIC X(100).
       
       01  WS-MENU-CHOICE           PIC 9(1).
       01  WS-CONTINUE-FLAG         PIC X VALUE 'Y'.
           88  WS-CONTINUE-YES      VALUE 'Y'.
           88  WS-CONTINUE-NO       VALUE 'N'.
       
       01  WS-SEARCH-CRITERIA.
           05  WS-SEARCH-CUST-ID    PIC 9(8).
           05  WS-SEARCH-ACC-ID     PIC 9(12).
           05  WS-SEARCH-LOAN-ID    PIC 9(12).
           05  WS-SEARCH-DATE-FROM  PIC 9(8).
           05  WS-SEARCH-DATE-TO    PIC 9(8).
       
       01  WS-CALCULATION-FIELDS.
           05  WS-INTEREST-AMOUNT   PIC 9(8)V99.
           05  WS-NEW-BALANCE       PIC 9(10)V99.
           05  WS-PAYMENT-AMOUNT    PIC 9(8)V99.
           05  WS-REMAINING-BALANCE PIC 9(10)V99.
           05  WS-MONTHLY-INTEREST  PIC 9(6)V99.
       
       01  WS-REPORT-HEADERS.
           05  WS-REPORT-TITLE      PIC X(50) VALUE 'BANKING SYSTEM REPORT'.
           05  WS-REPORT-SUBTITLE   PIC X(50) VALUE 'Generated on: '.
           05  WS-REPORT-LINE       PIC X(132) VALUE ALL '-'.
       
       01  WS-STATISTICS.
           05  WS-TOTAL-CUSTOMERS   PIC 9(6) VALUE 0.
           05  WS-ACTIVE-ACCOUNTS   PIC 9(6) VALUE 0.
           05  WS-TOTAL-TRANSACTIONS PIC 9(8) VALUE 0.
           05  WS-ACTIVE-LOANS      PIC 9(6) VALUE 0.
           05  WS-AVG-ACCOUNT-BALANCE PIC 9(8)V99.
           05  WS-AVG-LOAN-AMOUNT   PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM DISPLAY-MENU
           PERFORM PROCESS-MENU-CHOICE UNTIL WS-CONTINUE-NO
           PERFORM FINALIZE-PROGRAM
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           PERFORM OPEN-FILES
           PERFORM GET-CURRENT-DATE-TIME
           PERFORM DISPLAY-WELCOME-MESSAGE
           PERFORM LOAD-STATISTICS.
       
       OPEN-FILES.
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUSTOMER-STATUS NOT = '00'
               MOVE 'Error opening customer file' TO WS-ERROR-MESSAGE
               PERFORM DISPLAY-ERROR
               STOP RUN
           END-IF
           
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCOUNT-STATUS NOT = '00'
               MOVE 'Error opening account file' TO WS-ERROR-MESSAGE
               PERFORM DISPLAY-ERROR
               STOP RUN
           END-IF
           
           OPEN INPUT TRANSACTION-FILE
           IF WS-TRANSACTION-STATUS NOT = '00'
               MOVE 'Error opening transaction file' TO WS-ERROR-MESSAGE
               PERFORM DISPLAY-ERROR
               STOP RUN
           END-IF
           
           OPEN INPUT LOAN-FILE
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'Error opening loan file' TO WS-ERROR-MESSAGE
               PERFORM DISPLAY-ERROR
               STOP RUN
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE 'Error opening report file' TO WS-ERROR-MESSAGE
               PERFORM DISPLAY-ERROR
               STOP RUN
           END-IF.
       
       GET-CURRENT-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-TIME TO WS-CURRENT-TIME.
       
       DISPLAY-WELCOME-MESSAGE.
           DISPLAY ' '
           DISPLAY '==============================================='
           DISPLAY '           BANKING SYSTEM v2.0'
           DISPLAY '==============================================='
           DISPLAY 'Date: ' WS-CURRENT-DATE
           DISPLAY 'Time: ' WS-CURRENT-TIME
           DISPLAY ' '.
       
       LOAD-STATISTICS.
           PERFORM COUNT-CUSTOMERS
           PERFORM COUNT-ACCOUNTS
           PERFORM COUNT-TRANSACTIONS
           PERFORM COUNT-LOANS
           PERFORM CALCULATE-AVERAGES.
       
       COUNT-CUSTOMERS.
           PERFORM UNTIL WS-CUSTOMER-EOF-YES
               READ CUSTOMER-FILE
                   AT END SET WS-CUSTOMER-EOF-YES TO TRUE
                   NOT AT END
                       IF CUST-ACTIVE
                           ADD 1 TO WS-TOTAL-CUSTOMERS
                       END-IF
               END-READ
           END-PERFORM.
       
       COUNT-ACCOUNTS.
           PERFORM UNTIL WS-ACCOUNT-EOF-YES
               READ ACCOUNT-FILE
                   AT END SET WS-ACCOUNT-EOF-YES TO TRUE
                   NOT AT END
                       IF ACC-ACTIVE
                           ADD 1 TO WS-ACTIVE-ACCOUNTS
                           ADD ACC-BALANCE TO WS-TOTAL-BALANCE
                       END-IF
               END-READ
           END-PERFORM.
       
       COUNT-TRANSACTIONS.
           PERFORM UNTIL WS-TRANSACTION-EOF-YES
               READ TRANSACTION-FILE
                   AT END SET WS-TRANSACTION-EOF-YES TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TOTAL-TRANSACTIONS
               END-READ
           END-PERFORM.
       
       COUNT-LOANS.
           PERFORM UNTIL WS-LOAN-EOF-YES
               READ LOAN-FILE
                   AT END SET WS-LOAN-EOF-YES TO TRUE
                   NOT AT END
                       IF LOAN-ACTIVE
                           ADD 1 TO WS-ACTIVE-LOANS
                           ADD LOAN-BALANCE TO WS-TOTAL-LOANS
                       END-IF
               END-READ
           END-PERFORM.
       
       CALCULATE-AVERAGES.
           IF WS-ACTIVE-ACCOUNTS > 0
               COMPUTE WS-AVG-ACCOUNT-BALANCE = WS-TOTAL-BALANCE / WS-ACTIVE-ACCOUNTS
           END-IF
           
           IF WS-ACTIVE-LOANS > 0
               COMPUTE WS-AVG-LOAN-AMOUNT = WS-TOTAL-LOANS / WS-ACTIVE-LOANS
           END-IF.
       
       DISPLAY-MENU.
           DISPLAY ' '
           DISPLAY '=== BANKING SYSTEM MENU ==='
           DISPLAY '1. Customer Management'
           DISPLAY '2. Account Management'
           DISPLAY '3. Transaction Processing'
           DISPLAY '4. Loan Management'
           DISPLAY '5. Generate Reports'
           DISPLAY '6. System Statistics'
           DISPLAY '7. Exit'
           DISPLAY ' '
           DISPLAY 'Enter your choice (1-7): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE.
       
       PROCESS-MENU-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM CUSTOMER-MANAGEMENT
               WHEN 2 PERFORM ACCOUNT-MANAGEMENT
               WHEN 3 PERFORM TRANSACTION-PROCESSING
               WHEN 4 PERFORM LOAN-MANAGEMENT
               WHEN 5 PERFORM GENERATE-REPORTS
               WHEN 6 PERFORM DISPLAY-STATISTICS
               WHEN 7 SET WS-CONTINUE-NO TO TRUE
               WHEN OTHER
                   DISPLAY 'Invalid choice. Please try again.'
           END-EVALUATE
           
           IF WS-CONTINUE-YES
               DISPLAY 'Press Enter to continue...'
               ACCEPT WS-CONTINUE-FLAG
           END-IF.
       
       CUSTOMER-MANAGEMENT.
           DISPLAY ' '
           DISPLAY '=== CUSTOMER MANAGEMENT ==='
           DISPLAY '1. Add New Customer'
           DISPLAY '2. Search Customer'
           DISPLAY '3. Update Customer'
           DISPLAY '4. Delete Customer'
           DISPLAY '5. List All Customers'
           DISPLAY ' '
           DISPLAY 'Enter choice (1-5): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM ADD-CUSTOMER
               WHEN 2 PERFORM SEARCH-CUSTOMER
               WHEN 3 PERFORM UPDATE-CUSTOMER
               WHEN 4 PERFORM DELETE-CUSTOMER
               WHEN 5 PERFORM LIST-CUSTOMERS
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE.
       
       ADD-CUSTOMER.
           DISPLAY ' '
           DISPLAY '=== ADD NEW CUSTOMER ==='
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING
           ACCEPT CUST-ID
           DISPLAY 'Enter Customer Name: ' WITH NO ADVANCING
           ACCEPT CUST-NAME
           DISPLAY 'Enter Address: ' WITH NO ADVANCING
           ACCEPT CUST-ADDRESS
           DISPLAY 'Enter Phone: ' WITH NO ADVANCING
           ACCEPT CUST-PHONE
           DISPLAY 'Enter Email: ' WITH NO ADVANCING
           ACCEPT CUST-EMAIL
           DISPLAY 'Enter Date of Birth (YYYYMMDD): ' WITH NO ADVANCING
           ACCEPT CUST-DOB
           DISPLAY 'Enter SSN: ' WITH NO ADVANCING
           ACCEPT CUST-SSN
           
           MOVE 'A' TO CUST-STATUS
           MOVE WS-CURRENT-DATE TO CUST-CREATE-DATE
           
           DISPLAY 'Customer added successfully!'.
       
       SEARCH-CUSTOMER.
           DISPLAY ' '
           DISPLAY '=== SEARCH CUSTOMER ==='
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-CUST-ID
           
           PERFORM SEARCH-CUSTOMER-BY-ID
           
           IF CUST-ID = WS-SEARCH-CUST-ID
               PERFORM DISPLAY-CUSTOMER-DETAILS
           ELSE
               DISPLAY 'Customer not found.'
           END-IF.
       
       SEARCH-CUSTOMER-BY-ID.
           REWIND CUSTOMER-FILE
           PERFORM UNTIL WS-CUSTOMER-EOF-YES
               READ CUSTOMER-FILE
                   AT END SET WS-CUSTOMER-EOF-YES TO TRUE
                   NOT AT END
                       IF CUST-ID = WS-SEARCH-CUST-ID
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
       
       DISPLAY-CUSTOMER-DETAILS.
           DISPLAY ' '
           DISPLAY 'Customer Details:'
           DISPLAY 'ID: ' CUST-ID
           DISPLAY 'Name: ' CUST-NAME
           DISPLAY 'Address: ' CUST-ADDRESS
           DISPLAY 'Phone: ' CUST-PHONE
           DISPLAY 'Email: ' CUST-EMAIL
           DISPLAY 'DOB: ' CUST-DOB
           DISPLAY 'SSN: ' CUST-SSN
           DISPLAY 'Status: ' CUST-STATUS
           DISPLAY 'Create Date: ' CUST-CREATE-DATE.
       
       UPDATE-CUSTOMER.
           DISPLAY ' '
           DISPLAY '=== UPDATE CUSTOMER ==='
           DISPLAY 'Enter Customer ID to update: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-CUST-ID
           
           PERFORM SEARCH-CUSTOMER-BY-ID
           
           IF CUST-ID = WS-SEARCH-CUST-ID
               DISPLAY 'Current Name: ' CUST-NAME
               DISPLAY 'Enter new name (or press Enter to keep current): ' WITH NO ADVANCING
               ACCEPT CUST-NAME
               DISPLAY 'Customer updated successfully!'
           ELSE
               DISPLAY 'Customer not found.'
           END-IF.
       
       DELETE-CUSTOMER.
           DISPLAY ' '
           DISPLAY '=== DELETE CUSTOMER ==='
           DISPLAY 'Enter Customer ID to delete: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-CUST-ID
           
           PERFORM SEARCH-CUSTOMER-BY-ID
           
           IF CUST-ID = WS-SEARCH-CUST-ID
               MOVE 'I' TO CUST-STATUS
               DISPLAY 'Customer marked as inactive.'
           ELSE
               DISPLAY 'Customer not found.'
           END-IF.
       
       LIST-CUSTOMERS.
           DISPLAY ' '
           DISPLAY '=== ALL CUSTOMERS ==='
           REWIND CUSTOMER-FILE
           PERFORM UNTIL WS-CUSTOMER-EOF-YES
               READ CUSTOMER-FILE
                   AT END SET WS-CUSTOMER-EOF-YES TO TRUE
                   NOT AT END
                       IF CUST-ACTIVE
                           DISPLAY CUST-ID ' - ' CUST-NAME ' - ' CUST-STATUS
                       END-IF
               END-READ
           END-PERFORM.
       
       ACCOUNT-MANAGEMENT.
           DISPLAY ' '
           DISPLAY '=== ACCOUNT MANAGEMENT ==='
           DISPLAY '1. Open New Account'
           DISPLAY '2. Search Account'
           DISPLAY '3. Update Account'
           DISPLAY '4. Close Account'
           DISPLAY '5. List All Accounts'
           DISPLAY ' '
           DISPLAY 'Enter choice (1-5): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM OPEN-ACCOUNT
               WHEN 2 PERFORM SEARCH-ACCOUNT
               WHEN 3 PERFORM UPDATE-ACCOUNT
               WHEN 4 PERFORM CLOSE-ACCOUNT
               WHEN 5 PERFORM LIST-ACCOUNTS
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE.
       
       OPEN-ACCOUNT.
           DISPLAY ' '
           DISPLAY '=== OPEN NEW ACCOUNT ==='
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING
           ACCEPT ACC-CUST-ID
           DISPLAY 'Enter Account Type (SV/CH/LN/CR): ' WITH NO ADVANCING
           ACCEPT ACC-TYPE
           DISPLAY 'Enter Initial Balance: ' WITH NO ADVANCING
           ACCEPT ACC-BALANCE
           DISPLAY 'Enter Interest Rate: ' WITH NO ADVANCING
           ACCEPT ACC-INTEREST-RATE
           
           MOVE 'A' TO ACC-STATUS
           MOVE WS-CURRENT-DATE TO ACC-OPEN-DATE
           MOVE WS-CURRENT-DATE TO ACC-LAST-TRANS
           
           DISPLAY 'Account opened successfully!'.
       
       SEARCH-ACCOUNT.
           DISPLAY ' '
           DISPLAY '=== SEARCH ACCOUNT ==='
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID
               PERFORM DISPLAY-ACCOUNT-DETAILS
           ELSE
               DISPLAY 'Account not found.'
           END-IF.
       
       SEARCH-ACCOUNT-BY-ID.
           REWIND ACCOUNT-FILE
           PERFORM UNTIL WS-ACCOUNT-EOF-YES
               READ ACCOUNT-FILE
                   AT END SET WS-ACCOUNT-EOF-YES TO TRUE
                   NOT AT END
                       IF ACC-ID = WS-SEARCH-ACC-ID
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
       
       DISPLAY-ACCOUNT-DETAILS.
           DISPLAY ' '
           DISPLAY 'Account Details:'
           DISPLAY 'ID: ' ACC-ID
           DISPLAY 'Customer ID: ' ACC-CUST-ID
           DISPLAY 'Type: ' ACC-TYPE
           DISPLAY 'Balance: $' ACC-BALANCE
           DISPLAY 'Interest Rate: ' ACC-INTEREST-RATE '%'
           DISPLAY 'Status: ' ACC-STATUS
           DISPLAY 'Open Date: ' ACC-OPEN-DATE
           DISPLAY 'Last Transaction: ' ACC-LAST-TRANS.
       
       UPDATE-ACCOUNT.
           DISPLAY ' '
           DISPLAY '=== UPDATE ACCOUNT ==='
           DISPLAY 'Enter Account ID to update: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID
               DISPLAY 'Current Balance: $' ACC-BALANCE
               DISPLAY 'Enter new balance: ' WITH NO ADVANCING
               ACCEPT ACC-BALANCE
               DISPLAY 'Account updated successfully!'
           ELSE
               DISPLAY 'Account not found.'
           END-IF.
       
       CLOSE-ACCOUNT.
           DISPLAY ' '
           DISPLAY '=== CLOSE ACCOUNT ==='
           DISPLAY 'Enter Account ID to close: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID
               MOVE 'C' TO ACC-STATUS
               DISPLAY 'Account closed successfully.'
           ELSE
               DISPLAY 'Account not found.'
           END-IF.
       
       LIST-ACCOUNTS.
           DISPLAY ' '
           DISPLAY '=== ALL ACCOUNTS ==='
           REWIND ACCOUNT-FILE
           PERFORM UNTIL WS-ACCOUNT-EOF-YES
               READ ACCOUNT-FILE
                   AT END SET WS-ACCOUNT-EOF-YES TO TRUE
                   NOT AT END
                       IF ACC-ACTIVE
                           DISPLAY ACC-ID ' - ' ACC-CUST-ID ' - ' ACC-TYPE ' - $' ACC-BALANCE
                       END-IF
               END-READ
           END-PERFORM.
       
       TRANSACTION-PROCESSING.
           DISPLAY ' '
           DISPLAY '=== TRANSACTION PROCESSING ==='
           DISPLAY '1. Process Deposit'
           DISPLAY '2. Process Withdrawal'
           DISPLAY '3. Process Transfer'
           DISPLAY '4. Process Payment'
           DISPLAY '5. View Transaction History'
           DISPLAY ' '
           DISPLAY 'Enter choice (1-5): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM PROCESS-DEPOSIT
               WHEN 2 PERFORM PROCESS-WITHDRAWAL
               WHEN 3 PERFORM PROCESS-TRANSFER
               WHEN 4 PERFORM PROCESS-PAYMENT
               WHEN 5 PERFORM VIEW-TRANSACTION-HISTORY
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE.
       
       PROCESS-DEPOSIT.
           DISPLAY ' '
           DISPLAY '=== PROCESS DEPOSIT ==='
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           DISPLAY 'Enter Amount: ' WITH NO ADVANCING
           ACCEPT TRANS-AMOUNT
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID AND ACC-ACTIVE
               ADD TRANS-AMOUNT TO ACC-BALANCE
               MOVE WS-CURRENT-DATE TO ACC-LAST-TRANS
               DISPLAY 'Deposit processed successfully!'
               DISPLAY 'New balance: $' ACC-BALANCE
           ELSE
               DISPLAY 'Account not found or inactive.'
           END-IF.
       
       PROCESS-WITHDRAWAL.
           DISPLAY ' '
           DISPLAY '=== PROCESS WITHDRAWAL ==='
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           DISPLAY 'Enter Amount: ' WITH NO ADVANCING
           ACCEPT TRANS-AMOUNT
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID AND ACC-ACTIVE
               IF ACC-BALANCE >= TRANS-AMOUNT
                   SUBTRACT TRANS-AMOUNT FROM ACC-BALANCE
                   MOVE WS-CURRENT-DATE TO ACC-LAST-TRANS
                   DISPLAY 'Withdrawal processed successfully!'
                   DISPLAY 'New balance: $' ACC-BALANCE
               ELSE
                   DISPLAY 'Insufficient funds.'
               END-IF
           ELSE
               DISPLAY 'Account not found or inactive.'
           END-IF.
       
       PROCESS-TRANSFER.
           DISPLAY ' '
           DISPLAY '=== PROCESS TRANSFER ==='
           DISPLAY 'Enter From Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           DISPLAY 'Enter To Account ID: ' WITH NO ADVANCING
           ACCEPT TRANS-ACC-ID
           DISPLAY 'Enter Amount: ' WITH NO ADVANCING
           ACCEPT TRANS-AMOUNT
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID AND ACC-ACTIVE
               IF ACC-BALANCE >= TRANS-AMOUNT
                   SUBTRACT TRANS-AMOUNT FROM ACC-BALANCE
                   MOVE WS-CURRENT-DATE TO ACC-LAST-TRANS
                   DISPLAY 'Transfer processed successfully!'
                   DISPLAY 'New balance: $' ACC-BALANCE
               ELSE
                   DISPLAY 'Insufficient funds.'
               END-IF
           ELSE
               DISPLAY 'Account not found or inactive.'
           END-IF.
       
       PROCESS-PAYMENT.
           DISPLAY ' '
           DISPLAY '=== PROCESS PAYMENT ==='
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           DISPLAY 'Enter Payment Amount: ' WITH NO ADVANCING
           ACCEPT TRANS-AMOUNT
           
           PERFORM SEARCH-ACCOUNT-BY-ID
           
           IF ACC-ID = WS-SEARCH-ACC-ID AND ACC-ACTIVE
               IF ACC-BALANCE >= TRANS-AMOUNT
                   SUBTRACT TRANS-AMOUNT FROM ACC-BALANCE
                   MOVE WS-CURRENT-DATE TO ACC-LAST-TRANS
                   DISPLAY 'Payment processed successfully!'
                   DISPLAY 'New balance: $' ACC-BALANCE
               ELSE
                   DISPLAY 'Insufficient funds.'
               END-IF
           ELSE
               DISPLAY 'Account not found or inactive.'
           END-IF.
       
       VIEW-TRANSACTION-HISTORY.
           DISPLAY ' '
           DISPLAY '=== TRANSACTION HISTORY ==='
           DISPLAY 'Enter Account ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACC-ID
           
           REWIND TRANSACTION-FILE
           PERFORM UNTIL WS-TRANSACTION-EOF-YES
               READ TRANSACTION-FILE
                   AT END SET WS-TRANSACTION-EOF-YES TO TRUE
                   NOT AT END
                       IF TRANS-ACC-ID = WS-SEARCH-ACC-ID
                           DISPLAY TRANS-ID ' - ' TRANS-TYPE ' - $' TRANS-AMOUNT ' - ' TRANS-DATE
                       END-IF
               END-READ
           END-PERFORM.
       
       LOAN-MANAGEMENT.
           DISPLAY ' '
           DISPLAY '=== LOAN MANAGEMENT ==='
           DISPLAY '1. Apply for Loan'
           DISPLAY '2. Search Loan'
           DISPLAY '3. Make Loan Payment'
           DISPLAY '4. View Loan Details'
           DISPLAY '5. List All Loans'
           DISPLAY ' '
           DISPLAY 'Enter choice (1-5): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM APPLY-LOAN
               WHEN 2 PERFORM SEARCH-LOAN
               WHEN 3 PERFORM MAKE-LOAN-PAYMENT
               WHEN 4 PERFORM VIEW-LOAN-DETAILS
               WHEN 5 PERFORM LIST-LOANS
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE.
       
       APPLY-LOAN.
           DISPLAY ' '
           DISPLAY '=== APPLY FOR LOAN ==='
           DISPLAY 'Enter Customer ID: ' WITH NO ADVANCING
           ACCEPT LOAN-CUST-ID
           DISPLAY 'Enter Loan Type (PL/MG/BL/CL): ' WITH NO ADVANCING
           ACCEPT LOAN-TYPE
           DISPLAY 'Enter Loan Amount: ' WITH NO ADVANCING
           ACCEPT LOAN-AMOUNT
           DISPLAY 'Enter Interest Rate: ' WITH NO ADVANCING
           ACCEPT LOAN-INTEREST-RATE
           DISPLAY 'Enter Term (months): ' WITH NO ADVANCING
           ACCEPT LOAN-TERM
           
           PERFORM CALCULATE-MONTHLY-PAYMENT
           
           MOVE 'A' TO LOAN-STATUS
           MOVE WS-CURRENT-DATE TO LOAN-OPEN-DATE
           MOVE LOAN-AMOUNT TO LOAN-BALANCE
           
           DISPLAY 'Loan application submitted successfully!'
           DISPLAY 'Monthly payment: $' LOAN-MONTHLY-PAYMENT.
       
       CALCULATE-MONTHLY-PAYMENT.
           COMPUTE WS-MONTHLY-INTEREST = LOAN-INTEREST-RATE / 1200
           COMPUTE LOAN-MONTHLY-PAYMENT = LOAN-AMOUNT * 
               (WS-MONTHLY-INTEREST * (1 + WS-MONTHLY-INTEREST) ** LOAN-TERM) /
               ((1 + WS-MONTHLY-INTEREST) ** LOAN-TERM - 1).
       
       SEARCH-LOAN.
           DISPLAY ' '
           DISPLAY '=== SEARCH LOAN ==='
           DISPLAY 'Enter Loan ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-LOAN-ID
           
           PERFORM SEARCH-LOAN-BY-ID
           
           IF LOAN-ID = WS-SEARCH-LOAN-ID
               PERFORM DISPLAY-LOAN-DETAILS
           ELSE
               DISPLAY 'Loan not found.'
           END-IF.
       
       SEARCH-LOAN-BY-ID.
           REWIND LOAN-FILE
           PERFORM UNTIL WS-LOAN-EOF-YES
               READ LOAN-FILE
                   AT END SET WS-LOAN-EOF-YES TO TRUE
                   NOT AT END
                       IF LOAN-ID = WS-SEARCH-LOAN-ID
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
       
       DISPLAY-LOAN-DETAILS.
           DISPLAY ' '
           DISPLAY 'Loan Details:'
           DISPLAY 'ID: ' LOAN-ID
           DISPLAY 'Customer ID: ' LOAN-CUST-ID
           DISPLAY 'Type: ' LOAN-TYPE
           DISPLAY 'Amount: $' LOAN-AMOUNT
           DISPLAY 'Interest Rate: ' LOAN-INTEREST-RATE '%'
           DISPLAY 'Term: ' LOAN-TERM ' months'
           DISPLAY 'Monthly Payment: $' LOAN-MONTHLY-PAYMENT
           DISPLAY 'Balance: $' LOAN-BALANCE
           DISPLAY 'Status: ' LOAN-STATUS
           DISPLAY 'Open Date: ' LOAN-OPEN-DATE
           DISPLAY 'Due Date: ' LOAN-DUE-DATE.
       
       MAKE-LOAN-PAYMENT.
           DISPLAY ' '
           DISPLAY '=== MAKE LOAN PAYMENT ==='
           DISPLAY 'Enter Loan ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-LOAN-ID
           DISPLAY 'Enter Payment Amount: ' WITH NO ADVANCING
           ACCEPT WS-PAYMENT-AMOUNT
           
           PERFORM SEARCH-LOAN-BY-ID
           
           IF LOAN-ID = WS-SEARCH-LOAN-ID AND LOAN-ACTIVE
               IF WS-PAYMENT-AMOUNT <= LOAN-BALANCE
                   SUBTRACT WS-PAYMENT-AMOUNT FROM LOAN-BALANCE
                   IF LOAN-BALANCE = 0
                       MOVE 'P' TO LOAN-STATUS
                       DISPLAY 'Loan paid off completely!'
                   ELSE
                       DISPLAY 'Payment processed successfully!'
                       DISPLAY 'Remaining balance: $' LOAN-BALANCE
                   END-IF
               ELSE
                   DISPLAY 'Payment amount exceeds loan balance.'
               END-IF
           ELSE
               DISPLAY 'Loan not found or not active.'
           END-IF.
       
       VIEW-LOAN-DETAILS.
           DISPLAY ' '
           DISPLAY '=== LOAN DETAILS ==='
           DISPLAY 'Enter Loan ID: ' WITH NO ADVANCING
           ACCEPT WS-SEARCH-LOAN-ID
           
           PERFORM SEARCH-LOAN-BY-ID
           
           IF LOAN-ID = WS-SEARCH-LOAN-ID
               PERFORM DISPLAY-LOAN-DETAILS
           ELSE
               DISPLAY 'Loan not found.'
           END-IF.
       
       LIST-LOANS.
           DISPLAY ' '
           DISPLAY '=== ALL LOANS ==='
           REWIND LOAN-FILE
           PERFORM UNTIL WS-LOAN-EOF-YES
               READ LOAN-FILE
                   AT END SET WS-LOAN-EOF-YES TO TRUE
                   NOT AT END
                       IF LOAN-ACTIVE
                           DISPLAY LOAN-ID ' - ' LOAN-CUST-ID ' - ' LOAN-TYPE ' - $' LOAN-BALANCE
                       END-IF
               END-READ
           END-PERFORM.
       
       GENERATE-REPORTS.
           DISPLAY ' '
           DISPLAY '=== GENERATE REPORTS ==='
           DISPLAY '1. Customer Report'
           DISPLAY '2. Account Report'
           DISPLAY '3. Transaction Report'
           DISPLAY '4. Loan Report'
           DISPLAY '5. Summary Report'
           DISPLAY ' '
           DISPLAY 'Enter choice (1-5): ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM GENERATE-CUSTOMER-REPORT
               WHEN 2 PERFORM GENERATE-ACCOUNT-REPORT
               WHEN 3 PERFORM GENERATE-TRANSACTION-REPORT
               WHEN 4 PERFORM GENERATE-LOAN-REPORT
               WHEN 5 PERFORM GENERATE-SUMMARY-REPORT
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE.
       
       GENERATE-CUSTOMER-REPORT.
           DISPLAY 'Generating Customer Report...'
           WRITE REPORT-RECORD FROM WS-REPORT-TITLE
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           WRITE REPORT-RECORD FROM 'CUSTOMER REPORT'
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           
           REWIND CUSTOMER-FILE
           PERFORM UNTIL WS-CUSTOMER-EOF-YES
               READ CUSTOMER-FILE
                   AT END SET WS-CUSTOMER-EOF-YES TO TRUE
                   NOT AT END
                       IF CUST-ACTIVE
                           MOVE CUST-ID TO REPORT-RECORD
                           WRITE REPORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           DISPLAY 'Customer report generated successfully!'.
       
       GENERATE-ACCOUNT-REPORT.
           DISPLAY 'Generating Account Report...'
           WRITE REPORT-RECORD FROM WS-REPORT-TITLE
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           WRITE REPORT-RECORD FROM 'ACCOUNT REPORT'
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           
           REWIND ACCOUNT-FILE
           PERFORM UNTIL WS-ACCOUNT-EOF-YES
               READ ACCOUNT-FILE
                   AT END SET WS-ACCOUNT-EOF-YES TO TRUE
                   NOT AT END
                       IF ACC-ACTIVE
                           MOVE ACC-ID TO REPORT-RECORD
                           WRITE REPORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           DISPLAY 'Account report generated successfully!'.
       
       GENERATE-TRANSACTION-REPORT.
           DISPLAY 'Generating Transaction Report...'
           WRITE REPORT-RECORD FROM WS-REPORT-TITLE
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           WRITE REPORT-RECORD FROM 'TRANSACTION REPORT'
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           
           REWIND TRANSACTION-FILE
           PERFORM UNTIL WS-TRANSACTION-EOF-YES
               READ TRANSACTION-FILE
                   AT END SET WS-TRANSACTION-EOF-YES TO TRUE
                   NOT AT END
                       MOVE TRANS-ID TO REPORT-RECORD
                       WRITE REPORT-RECORD
               END-READ
           END-PERFORM
           
           DISPLAY 'Transaction report generated successfully!'.
       
       GENERATE-LOAN-REPORT.
           DISPLAY 'Generating Loan Report...'
           WRITE REPORT-RECORD FROM WS-REPORT-TITLE
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           WRITE REPORT-RECORD FROM 'LOAN REPORT'
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           
           REWIND LOAN-FILE
           PERFORM UNTIL WS-LOAN-EOF-YES
               READ LOAN-FILE
                   AT END SET WS-LOAN-EOF-YES TO TRUE
                   NOT AT END
                       IF LOAN-ACTIVE
                           MOVE LOAN-ID TO REPORT-RECORD
                           WRITE REPORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           DISPLAY 'Loan report generated successfully!'.
       
       GENERATE-SUMMARY-REPORT.
           DISPLAY 'Generating Summary Report...'
           WRITE REPORT-RECORD FROM WS-REPORT-TITLE
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           WRITE REPORT-RECORD FROM 'SUMMARY REPORT'
           WRITE REPORT-RECORD FROM WS-REPORT-LINE
           
           MOVE WS-TOTAL-CUSTOMERS TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE WS-ACTIVE-ACCOUNTS TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE WS-TOTAL-TRANSACTIONS TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE WS-ACTIVE-LOANS TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           DISPLAY 'Summary report generated successfully!'.
       
       DISPLAY-STATISTICS.
           DISPLAY ' '
           DISPLAY '=== SYSTEM STATISTICS ==='
           DISPLAY 'Total Customers: ' WS-TOTAL-CUSTOMERS
           DISPLAY 'Active Accounts: ' WS-ACTIVE-ACCOUNTS
           DISPLAY 'Total Transactions: ' WS-TOTAL-TRANSACTIONS
           DISPLAY 'Active Loans: ' WS-ACTIVE-LOANS
           DISPLAY 'Total Account Balance: $' WS-TOTAL-BALANCE
           DISPLAY 'Total Loan Balance: $' WS-TOTAL-LOANS
           DISPLAY 'Average Account Balance: $' WS-AVG-ACCOUNT-BALANCE
           DISPLAY 'Average Loan Amount: $' WS-AVG-LOAN-AMOUNT.
       
       DISPLAY-ERROR.
           DISPLAY 'ERROR: ' WS-ERROR-MESSAGE.
       
       FINALIZE-PROGRAM.
           CLOSE CUSTOMER-FILE
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE
           CLOSE LOAN-FILE
           CLOSE REPORT-FILE
           
           DISPLAY ' '
           DISPLAY '==============================================='
           DISPLAY '           BANKING SYSTEM SHUTDOWN'
           DISPLAY '==============================================='
           DISPLAY 'All files closed successfully.'
           DISPLAY 'Program terminated normally.'.
