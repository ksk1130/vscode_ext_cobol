       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIAL-CONSTANTS-TEST.
       AUTHOR. Test Program.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-VARS.
           05  NUM-VAR        PIC 9(3).
           05  ALPHA-VAR      PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *    These should NOT generate warnings (special constants)
           MOVE ZEROS TO NUM-VAR.
           MOVE ZERO TO NUM-VAR.
           MOVE SPACES TO ALPHA-VAR.
           MOVE HIGH-VALUE TO ALPHA-VAR.
           MOVE LOW-VALUE TO ALPHA-VAR.
           
      *    These should also NOT generate warnings (literals)
           MOVE 123 TO NUM-VAR.
           MOVE "HELLO" TO ALPHA-VAR.
           
           STOP RUN.
