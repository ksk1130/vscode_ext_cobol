       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTILINE-CALL-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-DATA          PIC X(100).
       01  WS-OUTPUT-DATA-1       PIC X(100).
       01  WS-OUTPUT-DATA-2       PIC X(100).
       01  WS-PROGRAM-NAME        PIC X(30).
       
       PROCEDURE DIVISION.
           MOVE 'SAMPLE-INPUT' TO WS-INPUT-DATA.
           MOVE 'SAMPLE-OUTPUT-1' TO WS-OUTPUT-DATA-1.
           MOVE 'SAMPLE-OUTPUT-2' TO WS-OUTPUT-DATA-2.
           MOVE 'TEST-SUBPROG' TO WS-PROGRAM-NAME.
           
           CALL 'TEST-SUBPROG' USING WS-INPUT-DATA
                                     WS-OUTPUT-DATA-1
           END-CALL.
           
           CALL 'ANOTHER-PROG' 
               USING WS-INPUT-DATA
                     WS-OUTPUT-DATA-1
                     WS-OUTPUT-DATA-2
           END-CALL.
           
           CALL 'THREE-LINE-CALL'
               USING WS-INPUT-DATA
                     WS-OUTPUT-DATA-1
                     WS-OUTPUT-DATA-2.
           
           CALL WS-PROGRAM-NAME
               USING WS-INPUT-DATA
           END-CALL.
           
           STOP RUN.
