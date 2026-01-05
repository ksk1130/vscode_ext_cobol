       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-COPY-TEST.
      *================================================================
      * Test program to verify handling of duplicate variable names
      * across multiple copybooks
      *================================================================
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Reference CUSTOMER-DATA copybook
       COPY CUSTOMER-DATA.
       
      * Reference PRODUCT-DATA copybook
       COPY PRODUCT-DATA.
       
      * Both copybooks have RECORD-ID and RECORD-NAME variables
      * but they should be resolved to their respective copybooks
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 12345678 TO RECORD-ID OF CUSTOMER-RECORD
           MOVE "JOHN DOE" TO RECORD-NAME OF CUSTOMER-RECORD
           MOVE "AC" TO STATUS-CODE
           
           MOVE 87654321 TO RECORD-ID OF PRODUCT-RECORD
           MOVE "LAPTOP" TO RECORD-NAME OF PRODUCT-RECORD
           MOVE 999.99 TO PRICE
           
           DISPLAY "Customer ID: " RECORD-ID OF CUSTOMER-RECORD
           DISPLAY "Customer Name: " RECORD-NAME OF CUSTOMER-RECORD
           DISPLAY "Product ID: " RECORD-ID OF PRODUCT-RECORD
           DISPLAY "Product Name: " RECORD-NAME OF PRODUCT-RECORD
           STOP RUN.
