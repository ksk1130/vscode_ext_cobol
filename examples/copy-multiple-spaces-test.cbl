       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPY-TEST.
      *================================================================
      * Test program to verify COPY statement detection with multiple
      * spaces between COPY and copybook name
      *================================================================
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Test case 1: COPY with single space (should work)
       COPY TEST-COPY.
       
      * Test case 2: COPY with 2 spaces (should work)
       COPY  TEST-COPY.
       
      * Test case 3: COPY with 6 spaces (should work)
       COPY      TEST-COPY.
       
      * Test case 4: Variable starting with COPY (should NOT be treated as COPY statement)
       01  COPYBOOK-FILE.
           05  COPY-COUNT          PIC 9(4).
           05  COPY-NAME           PIC X(30).
       
      * Test case 5: Variable name COPY-FILE (should NOT be treated as COPY statement)  
       01  COPY-FILE-RECORD        PIC X(80).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 1 TO CUSTOMER-ID
           MOVE "TEST CUSTOMER" TO CUSTOMER-NAME
           MOVE "123 MAIN ST" TO CUSTOMER-ADDRESS
           DISPLAY "Customer ID: " CUSTOMER-ID
           DISPLAY "Customer Name: " CUSTOMER-NAME
           STOP RUN.
