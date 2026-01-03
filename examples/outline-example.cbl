        IDENTIFICATION DIVISION.
        PROGRAM-ID. OUTLINE-EXAMPLE.
        AUTHOR. COBOL LSP Extension.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID        PIC 9(8).
            05  CUSTOMER-NAME      PIC X(50).
            05  CUSTOMER-ADDRESS.
                10  STREET         PIC X(50).
                10  CITY           PIC X(30).
                10  ZIP-CODE       PIC 9(5).
            05  ACCOUNT-BALANCE    PIC 9(10)V99.
        
        01  STATUS-CODE            PIC 99.
            88  STATUS-OK          VALUE 00.
            88  STATUS-ERROR       VALUE 99.
        
        01  COUNTERS.
            05  TOTAL-PROCESSED    PIC 9(6) VALUE ZERO.
            05  TOTAL-ERRORS       PIC 9(6) VALUE ZERO.
        
        PROCEDURE DIVISION.
        MAIN-SECTION SECTION.
        
        MAIN-PARAGRAPH.
            DISPLAY "Starting program".
            PERFORM INITIALIZE-DATA.
            PERFORM PROCESS-CUSTOMER.
            PERFORM DISPLAY-STATISTICS.
            PERFORM CLEANUP.
            STOP RUN.
        
        INITIALIZE-DATA.
            MOVE ZEROS TO CUSTOMER-ID.
            MOVE SPACES TO CUSTOMER-NAME.
            MOVE ZEROS TO ACCOUNT-BALANCE.
            MOVE ZERO TO TOTAL-PROCESSED.
            MOVE ZERO TO TOTAL-ERRORS.
        
        PROCESS-CUSTOMER.
            DISPLAY "Processing customer".
            MOVE 12345678 TO CUSTOMER-ID.
            MOVE "John Doe" TO CUSTOMER-NAME.
            MOVE "123 Main St" TO STREET.
            MOVE "New York" TO CITY.
            MOVE 10001 TO ZIP-CODE.
            MOVE 1000.50 TO ACCOUNT-BALANCE.
            ADD 1 TO TOTAL-PROCESSED.
        
        DISPLAY-STATISTICS.
            DISPLAY "Total Processed: " TOTAL-PROCESSED.
            DISPLAY "Total Errors: " TOTAL-ERRORS.
        
        CLEANUP.
            DISPLAY "Cleanup complete".
