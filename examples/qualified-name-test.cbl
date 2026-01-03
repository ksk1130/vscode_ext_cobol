        IDENTIFICATION DIVISION.
        PROGRAM-ID. QUALIFIED-NAME-TEST.
        AUTHOR. Test Program.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID    PIC 9(8).
            05  CUSTOMER-NAME  PIC X(50).
        
        01  WS-VARS.
            05  WS-ID          PIC 9(3).
            05  WS-NAME        PIC X(10).
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
       *    These use qualified names with periods
            MOVE CUSTOMER-RECORD.CUSTOMER-ID TO WS-ID.
            MOVE CUSTOMER-RECORD.CUSTOMER-NAME TO WS-NAME.
            
            STOP RUN.
