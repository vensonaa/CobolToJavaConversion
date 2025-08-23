       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENTERPRISE-INVENTORY-SYSTEM.
       AUTHOR. SYSTEM-DEVELOPER.
       DATE-WRITTEN. 2024-12-19.
       DATE-COMPILED. 2024-12-19.
       SECURITY. CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "$".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCT-MASTER ASSIGN TO "products.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS PRODUCT-MASTER-KEY
                  FILE STATUS IS WS-PRODUCT-MASTER-STATUS.

           SELECT INVENTORY-FILE ASSIGN TO "inventory.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS INVENTORY-FILE-KEY
                  FILE STATUS IS WS-INVENTORY-FILE-STATUS.

           SELECT SUPPLIER-FILE ASSIGN TO "suppliers.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS SUPPLIER-FILE-KEY
                  FILE STATUS IS WS-SUPPLIER-FILE-STATUS.

           SELECT PURCHASE-ORDER ASSIGN TO "purchase_orders.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS PURCHASE-ORDER-KEY
                  FILE STATUS IS WS-PURCHASE-ORDER-STATUS.

           SELECT SALES-ORDER ASSIGN TO "sales_orders.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS SALES-ORDER-KEY
                  FILE STATUS IS WS-SALES-ORDER-STATUS.

           SELECT CUSTOMER-FILE ASSIGN TO "customers.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CUSTOMER-FILE-KEY
                  FILE STATUS IS WS-CUSTOMER-FILE-STATUS.

           SELECT WAREHOUSE-FILE ASSIGN TO "warehouses.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS WAREHOUSE-FILE-KEY
                  FILE STATUS IS WS-WAREHOUSE-FILE-STATUS.

           SELECT CATEGORY-FILE ASSIGN TO "categories.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CATEGORY-FILE-KEY
                  FILE STATUS IS WS-CATEGORY-FILE-STATUS.

           SELECT PRICE-HISTORY ASSIGN TO "price_history.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS PRICE-HISTORY-KEY
                  FILE STATUS IS WS-PRICE-HISTORY-STATUS.

           SELECT TRANSACTION-LOG ASSIGN TO "transactions.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS TRANSACTION-LOG-KEY
                  FILE STATUS IS WS-TRANSACTION-LOG-STATUS.

           SELECT REPORT-FILE ASSIGN TO "inventory_report.txt"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS REPORT-FILE-KEY
                  FILE STATUS IS WS-REPORT-FILE-STATUS.

           SELECT ERROR-LOG ASSIGN TO "error_log.txt"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ERROR-LOG-KEY
                  FILE STATUS IS WS-ERROR-LOG-STATUS.

           SELECT BACKUP-FILE ASSIGN TO "backup.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS BACKUP-FILE-KEY
                  FILE STATUS IS WS-BACKUP-FILE-STATUS.

           SELECT AUDIT-TRAIL ASSIGN TO "audit_trail.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS AUDIT-TRAIL-KEY
                  FILE STATUS IS WS-AUDIT-TRAIL-STATUS.

           SELECT CONFIG-FILE ASSIGN TO "config.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CONFIG-FILE-KEY
                  FILE STATUS IS WS-CONFIG-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  PRODUCT-MASTER.
       01  PRODUCT-MASTER-RECORD.
           05  PRODUCT-MASTER-KEY        PIC X(20).
           05  PRODUCT-MASTER-DATA       PIC X(200).
           05  PRODUCT-MASTER-TIMESTAMP  PIC 9(14).

       FD  INVENTORY-FILE.
       01  INVENTORY-FILE-RECORD.
           05  INVENTORY-FILE-KEY        PIC X(20).
           05  INVENTORY-FILE-DATA       PIC X(200).
           05  INVENTORY-FILE-TIMESTAMP  PIC 9(14).

       FD  SUPPLIER-FILE.
       01  SUPPLIER-FILE-RECORD.
           05  SUPPLIER-FILE-KEY        PIC X(20).
           05  SUPPLIER-FILE-DATA       PIC X(200).
           05  SUPPLIER-FILE-TIMESTAMP  PIC 9(14).

       FD  PURCHASE-ORDER.
       01  PURCHASE-ORDER-RECORD.
           05  PURCHASE-ORDER-KEY        PIC X(20).
           05  PURCHASE-ORDER-DATA       PIC X(200).
           05  PURCHASE-ORDER-TIMESTAMP  PIC 9(14).

       FD  SALES-ORDER.
       01  SALES-ORDER-RECORD.
           05  SALES-ORDER-KEY        PIC X(20).
           05  SALES-ORDER-DATA       PIC X(200).
           05  SALES-ORDER-TIMESTAMP  PIC 9(14).

       FD  CUSTOMER-FILE.
       01  CUSTOMER-FILE-RECORD.
           05  CUSTOMER-FILE-KEY        PIC X(20).
           05  CUSTOMER-FILE-DATA       PIC X(200).
           05  CUSTOMER-FILE-TIMESTAMP  PIC 9(14).

       FD  WAREHOUSE-FILE.
       01  WAREHOUSE-FILE-RECORD.
           05  WAREHOUSE-FILE-KEY        PIC X(20).
           05  WAREHOUSE-FILE-DATA       PIC X(200).
           05  WAREHOUSE-FILE-TIMESTAMP  PIC 9(14).

       FD  CATEGORY-FILE.
       01  CATEGORY-FILE-RECORD.
           05  CATEGORY-FILE-KEY        PIC X(20).
           05  CATEGORY-FILE-DATA       PIC X(200).
           05  CATEGORY-FILE-TIMESTAMP  PIC 9(14).

       FD  PRICE-HISTORY.
       01  PRICE-HISTORY-RECORD.
           05  PRICE-HISTORY-KEY        PIC X(20).
           05  PRICE-HISTORY-DATA       PIC X(200).
           05  PRICE-HISTORY-TIMESTAMP  PIC 9(14).

       FD  TRANSACTION-LOG.
       01  TRANSACTION-LOG-RECORD.
           05  TRANSACTION-LOG-KEY        PIC X(20).
           05  TRANSACTION-LOG-DATA       PIC X(200).
           05  TRANSACTION-LOG-TIMESTAMP  PIC 9(14).

       FD  REPORT-FILE.
       01  REPORT-FILE-RECORD.
           05  REPORT-FILE-KEY        PIC X(20).
           05  REPORT-FILE-DATA       PIC X(200).
           05  REPORT-FILE-TIMESTAMP  PIC 9(14).

       FD  ERROR-LOG.
       01  ERROR-LOG-RECORD.
           05  ERROR-LOG-KEY        PIC X(20).
           05  ERROR-LOG-DATA       PIC X(200).
           05  ERROR-LOG-TIMESTAMP  PIC 9(14).

       FD  BACKUP-FILE.
       01  BACKUP-FILE-RECORD.
           05  BACKUP-FILE-KEY        PIC X(20).
           05  BACKUP-FILE-DATA       PIC X(200).
           05  BACKUP-FILE-TIMESTAMP  PIC 9(14).

       FD  AUDIT-TRAIL.
       01  AUDIT-TRAIL-RECORD.
           05  AUDIT-TRAIL-KEY        PIC X(20).
           05  AUDIT-TRAIL-DATA       PIC X(200).
           05  AUDIT-TRAIL-TIMESTAMP  PIC 9(14).

       FD  CONFIG-FILE.
       01  CONFIG-FILE-RECORD.
           05  CONFIG-FILE-KEY        PIC X(20).
           05  CONFIG-FILE-DATA       PIC X(200).
           05  CONFIG-FILE-TIMESTAMP  PIC 9(14).

       WORKING-STORAGE SECTION.

       01  WS-SYSTEM-CONSTANTS.
           05  WS-MAX-PRODUCTS        PIC 9(6) VALUE 999999.
           05  WS-MAX-SUPPLIERS       PIC 9(4) VALUE 9999.
           05  WS-MAX-CUSTOMERS       PIC 9(6) VALUE 999999.
           05  WS-MAX-WAREHOUSES      PIC 9(3) VALUE 999.
           05  WS-MAX-CATEGORIES      PIC 9(3) VALUE 999.
           05  WS-MAX-ORDERS          PIC 9(8) VALUE 99999999.
           05  WS-MAX-TRANSACTIONS    PIC 9(10) VALUE 9999999999.

       01  WS-PRODUCT-STRUCTURE.
           05  WS-PRODUCT-ID          PIC 9(8).
           05  WS-PRODUCT-NAME        PIC X(50).
           05  WS-PRODUCT-DESCRIPTION PIC X(200).
           05  WS-PRODUCT-CATEGORY    PIC 9(3).
           05  WS-PRODUCT-SUPPLIER    PIC 9(4).
           05  WS-PRODUCT-UOM         PIC X(10).
           05  WS-PRODUCT-COST        PIC 9(8)V99.
           05  WS-PRODUCT-PRICE       PIC 9(8)V99.
           05  WS-PRODUCT-MIN-STOCK   PIC 9(6).
           05  WS-PRODUCT-MAX-STOCK   PIC 9(6).
           05  WS-PRODUCT-REORDER-PT  PIC 9(6).
           05  WS-PRODUCT-STATUS      PIC X(1).
               88  WS-PRODUCT-ACTIVE    VALUE "A".
               88  WS-PRODUCT-INACTIVE  VALUE "I".
               88  WS-PRODUCT-DISCONT   VALUE "D".
           05  WS-PRODUCT-CREATE-DATE PIC 9(8).
           05  WS-PRODUCT-LAST-UPDATE PIC 9(8).

       01  WS-INVENTORY-STRUCTURE.
           05  WS-INV-PRODUCT-ID      PIC 9(8).
           05  WS-INV-WAREHOUSE-ID    PIC 9(3).
           05  WS-INV-QUANTITY        PIC 9(8).
           05  WS-INV-ALLOCATED       PIC 9(8).
           05  WS-INV-AVAILABLE       PIC 9(8).
           05  WS-INV-LAST-RECEIPT    PIC 9(8).
           05  WS-INV-LAST-ISSUE      PIC 9(8).
           05  WS-INV-UNIT-COST       PIC 9(8)V99.
           05  WS-INV-TOTAL-VALUE     PIC 9(12)V99.

       01  WS-SUPPLIER-STRUCTURE.
           05  WS-SUPPLIER-ID         PIC 9(4).
           05  WS-SUPPLIER-NAME       PIC X(50).
           05  WS-SUPPLIER-ADDRESS    PIC X(100).
           05  WS-SUPPLIER-CITY       PIC X(30).
           05  WS-SUPPLIER-STATE      PIC X(2).
           05  WS-SUPPLIER-ZIP        PIC X(10).
           05  WS-SUPPLIER-PHONE      PIC X(15).
           05  WS-SUPPLIER-EMAIL      PIC X(50).
           05  WS-SUPPLIER-CONTACT    PIC X(50).
           05  WS-SUPPLIER-TERMS      PIC 9(3).
           05  WS-SUPPLIER-RATING     PIC 9(1).
           05  WS-SUPPLIER-STATUS     PIC X(1).
               88  WS-SUPPLIER-ACTIVE   VALUE "A".
               88  WS-SUPPLIER-INACTIVE VALUE "I".

       01  WS-CUSTOMER-STRUCTURE.
           05  WS-CUSTOMER-ID         PIC 9(6).
           05  WS-CUSTOMER-NAME       PIC X(50).
           05  WS-CUSTOMER-ADDRESS    PIC X(100).
           05  WS-CUSTOMER-CITY       PIC X(30).
           05  WS-CUSTOMER-STATE      PIC X(2).
           05  WS-CUSTOMER-ZIP        PIC X(10).
           05  WS-CUSTOMER-PHONE      PIC X(15).
           05  WS-CUSTOMER-EMAIL      PIC X(50).
           05  WS-CUSTOMER-CREDIT-LIMIT PIC 9(8)V99.
           05  WS-CUSTOMER-BALANCE    PIC 9(8)V99.
           05  WS-CUSTOMER-TERMS      PIC 9(3).
           05  WS-CUSTOMER-STATUS     PIC X(1).
               88  WS-CUSTOMER-ACTIVE   VALUE "A".
               88  WS-CUSTOMER-INACTIVE VALUE "I".
               88  WS-CUSTOMER-HOLD     VALUE "H".

       01  WS-PURCHASE-ORDER-STRUCTURE.
           05  WS-PO-NUMBER           PIC 9(8).
           05  WS-PO-SUPPLIER-ID      PIC 9(4).
           05  WS-PO-DATE             PIC 9(8).
           05  WS-PO-REQUIRED-DATE    PIC 9(8).
           05  WS-PO-TOTAL-AMOUNT     PIC 9(10)V99.
           05  WS-PO-STATUS           PIC X(1).
               88  WS-PO-DRAFT         VALUE "D".
               88  WS-PO-SENT          VALUE "S".
               88  WS-PO-RECEIVED      VALUE "R".
               88  WS-PO-CANCELLED     VALUE "C".
           05  WS-PO-APPROVED-BY      PIC X(20).
           05  WS-PO-APPROVED-DATE    PIC 9(8).

       01  WS-SALES-ORDER-STRUCTURE.
           05  WS-SO-NUMBER           PIC 9(8).
           05  WS-SO-CUSTOMER-ID      PIC 9(6).
           05  WS-SO-DATE             PIC 9(8).
           05  WS-SO-REQUIRED-DATE    PIC 9(8).
           05  WS-SO-TOTAL-AMOUNT     PIC 9(10)V99.
           05  WS-SO-STATUS           PIC X(1).
               88  WS-SO-DRAFT         VALUE "D".
               88  WS-SO-CONFIRMED     VALUE "C".
               88  WS-SO-SHIPPED       VALUE "S".
               88  WS-SO-DELIVERED     VALUE "L".
               88  WS-SO-CANCELLED     VALUE "X".
           05  WS-SO-SHIP-TO          PIC X(100).
           05  WS-SO-SHIP-VIA         PIC X(20).

       01  WS-TRANSACTION-STRUCTURE.
           05  WS-TRANS-ID            PIC 9(10).
           05  WS-TRANS-TYPE          PIC X(2).
               88  WS-TRANS-RECEIPT    VALUE "RC".
               88  WS-TRANS-ISSUE      VALUE "IS".
               88  WS-TRANS-ADJUSTMENT VALUE "AD".
               88  WS-TRANS-TRANSFER   VALUE "TR".
           05  WS-TRANS-PRODUCT-ID    PIC 9(8).
           05  WS-TRANS-WAREHOUSE-ID  PIC 9(3).
           05  WS-TRANS-QUANTITY      PIC 9(8).
           05  WS-TRANS-UNIT-COST     PIC 9(8)V99.
           05  WS-TRANS-TOTAL-COST    PIC 9(10)V99).
           05  WS-TRANS-DATE          PIC 9(8).
           05  WS-TRANS-TIME          PIC 9(6).
           05  WS-TRANS-REFERENCE     PIC X(20).
           05  WS-TRANS-USER-ID       PIC X(20).
           05  WS-TRANS-DESCRIPTION   PIC X(100).

       01  WS-WAREHOUSE-STRUCTURE.
           05  WS-WAREHOUSE-ID        PIC 9(3).
           05  WS-WAREHOUSE-NAME      PIC X(30).
           05  WS-WAREHOUSE-ADDRESS   PIC X(100).
           05  WS-WAREHOUSE-CITY      PIC X(30).
           05  WS-WAREHOUSE-STATE     PIC X(2).
           05  WS-WAREHOUSE-ZIP       PIC X(10).
           05  WS-WAREHOUSE-PHONE     PIC X(15).
           05  WS-WAREHOUSE-MANAGER   PIC X(30).
           05  WS-WAREHOUSE-STATUS    PIC X(1).
               88  WS-WAREHOUSE-ACTIVE   VALUE "A".
               88  WS-WAREHOUSE-INACTIVE VALUE "I".

       01  WS-CATEGORY-STRUCTURE.
           05  WS-CATEGORY-ID         PIC 9(3).
           05  WS-CATEGORY-NAME       PIC X(30).
           05  WS-CATEGORY-DESCRIPTION PIC X(100).
           05  WS-CATEGORY-PARENT     PIC 9(3).
           05  WS-CATEGORY-LEVEL      PIC 9(1).
           05  WS-CATEGORY-STATUS     PIC X(1).
               88  WS-CATEGORY-ACTIVE   VALUE "A".
               88  WS-CATEGORY-INACTIVE VALUE "I".

       01  WS-PRICE-HISTORY-STRUCTURE.
           05  WS-PH-PRODUCT-ID       PIC 9(8).
           05  WS-PH-EFFECTIVE-DATE   PIC 9(8).
           05  WS-PH-END-DATE         PIC 9(8).
           05  WS-PH-COST-PRICE       PIC 9(8)V99.
           05  WS-PH-SELLING-PRICE    PIC 9(8)V99.
           05  WS-PH-REASON-CODE      PIC X(2).
           05  WS-PH-APPROVED-BY      PIC X(20).

       01  WS-SYSTEM-VARIABLES.
           05  WS-CURRENT-DATE        PIC 9(8).
           05  WS-CURRENT-TIME        PIC 9(6).
           05  WS-USER-ID             PIC X(20).
           05  WS-PROGRAM-ID          PIC X(30).
           05  WS-SESSION-ID          PIC X(20).
           05  WS-TRANSACTION-COUNT   PIC 9(6).
           05  WS-ERROR-COUNT         PIC 9(4).
           05  WS-WARNING-COUNT       PIC 9(4).

       01  WS-ERROR-HANDLING.
           05  WS-ERROR-CODE          PIC X(4).
           05  WS-ERROR-MESSAGE       PIC X(100).
           05  WS-ERROR-SEVERITY      PIC X(1).
               88  WS-ERROR-INFO       VALUE "I".
               88  WS-ERROR-WARNING    VALUE "W".
               88  WS-ERROR-ERROR      VALUE "E".
               88  WS-ERROR-FATAL      VALUE "F".
           05  WS-ERROR-LOCATION      PIC X(50).
           05  WS-ERROR-TIMESTAMP     PIC 9(14).

       01  WS-MENU-OPTIONS.
           05  WS-MENU-CHOICE         PIC 9(1).
               88  WS-MENU-PRODUCT     VALUE 1.
               88  WS-MENU-INVENTORY   VALUE 2.
               88  WS-MENU-SUPPLIER    VALUE 3.
               88  WS-MENU-CUSTOMER    VALUE 4.
               88  WS-MENU-PURCHASE    VALUE 5.
               88  WS-MENU-SALES       VALUE 6.
               88  WS-MENU-REPORTS     VALUE 7.
               88  WS-MENU-MAINTENANCE VALUE 8.
               88  WS-MENU-EXIT        VALUE 9.

       01  WS-PRODUCT-MASTER-STATUS        PIC X(2).
       01  WS-INVENTORY-FILE-STATUS        PIC X(2).
       01  WS-SUPPLIER-FILE-STATUS        PIC X(2).
       01  WS-PURCHASE-ORDER-STATUS        PIC X(2).
       01  WS-SALES-ORDER-STATUS        PIC X(2).
       01  WS-CUSTOMER-FILE-STATUS        PIC X(2).
       01  WS-WAREHOUSE-FILE-STATUS        PIC X(2).
       01  WS-CATEGORY-FILE-STATUS        PIC X(2).
       01  WS-PRICE-HISTORY-STATUS        PIC X(2).
       01  WS-TRANSACTION-LOG-STATUS        PIC X(2).
       01  WS-REPORT-FILE-STATUS        PIC X(2).
       01  WS-ERROR-LOG-STATUS        PIC X(2).
       01  WS-BACKUP-FILE-STATUS        PIC X(2).
       01  WS-AUDIT-TRAIL-STATUS        PIC X(2).
       01  WS-CONFIG-FILE-STATUS        PIC X(2).

       PROCEDURE DIVISION.

       0000-MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-MAIN-MENU UNTIL WS-MENU-EXIT
           PERFORM 9000-CLEANUP
           STOP RUN.
