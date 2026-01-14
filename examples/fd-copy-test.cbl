       IDENTIFICATION DIVISION.
       PROGRAM-ID. FD-COPY-TEST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO "CUSTOMER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-FILE.
       COPY CUSTOMER-DATA.
       
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG        PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT CUST-FILE
           PERFORM READ-CUSTOMER UNTIL WS-EOF-FLAG = 'Y'
           CLOSE CUST-FILE
           STOP RUN.
       
       READ-CUSTOMER.
           READ CUST-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
           END-READ.
