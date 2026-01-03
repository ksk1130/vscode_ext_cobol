        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-88-LEVEL-FIX.
        AUTHOR. COBOL-LSP-TEAM.
        
       *****************************************************************
       * This test file verifies that 88-level condition names appear
       * only ONCE in the outline view, not duplicated.
       *****************************************************************
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  WS-FILE-STATUS          PIC XX.
            88  FILE-OK             VALUE '00'.
            88  FILE-EOF            VALUE '10'.
            88  FILE-ERROR          VALUE '99'.
        
        01  WS-CUSTOMER-STATUS      PIC X.
            88  STATUS-ACTIVE       VALUE 'A'.
            88  STATUS-INACTIVE     VALUE 'I'.
            88  STATUS-SUSPENDED    VALUE 'S'.
        
        01  WS-PROCESSING-FLAG      PIC X.
            88  PROCESSING-COMPLETE VALUE 'Y'.
            88  PROCESSING-PENDING  VALUE 'N'.
        
        01  WS-COUNTERS.
            05  WS-TOTAL-COUNT      PIC 9(5) VALUE ZERO.
            05  WS-ERROR-COUNT      PIC 9(5) VALUE ZERO.
        
        PROCEDURE DIVISION.
        MAIN-PROCEDURE.
            DISPLAY "Testing 88-level outline fix".
            
            SET FILE-OK TO TRUE.
            IF FILE-OK
                DISPLAY "File status is OK"
            END-IF.
            
            SET STATUS-ACTIVE TO TRUE.
            IF STATUS-ACTIVE
                DISPLAY "Customer is active"
            END-IF.
            
            SET PROCESSING-COMPLETE TO TRUE.
            IF PROCESSING-COMPLETE
                DISPLAY "Processing complete"
            END-IF.
            
            STOP RUN.
        
        END PROGRAM TEST-88-LEVEL-FIX.
