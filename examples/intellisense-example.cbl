        IDENTIFICATION DIVISION.
        PROGRAM-ID. INTELLISENSE-DEMO.
        AUTHOR. COBOL LSP Extension.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  WS-EMPLOYEE-RECORD.
            05  WS-EMP-ID          PIC 9(6).
            05  WS-EMP-NAME        PIC X(30).
            05  WS-EMP-SALARY      PIC 9(8)V99.
            05  WS-EMP-STATUS      PIC X(1).
                88  STATUS-ACTIVE  VALUE 'A'.
                88  STATUS-INACTIVE VALUE 'I'.
        
        01  WS-COUNTER             PIC 9(4) VALUE ZERO.
        01  WS-RESULT              PIC 9(10)V99.
        
        PROCEDURE DIVISION.
        MAIN-SECTION SECTION.
        
        MAIN-PARAGRAPH.
       *    IntelliSense Demo:
       *    1. Type "MOV" and press Ctrl+Space to see MOVE keyword
       *    2. Type variable names to see completions
       *    3. Type "PERFORM " to see paragraph/section completions
       *    4. Type "CALL " to see program name completions
            DISPLAY "IntelliSense Demo Program".
            PERFORM INIT-VARIABLES.
            PERFORM CALCULATE-SALARY.
            PERFORM DISPLAY-RESULTS.
            STOP RUN.
        
        INIT-VARIABLES.
       *    Try typing WS- to see all WS- variables
            MOVE 100001 TO WS-EMP-ID.
            MOVE "Jane Smith" TO WS-EMP-NAME.
            MOVE 50000.00 TO WS-EMP-SALARY.
            SET STATUS-ACTIVE TO TRUE.
            MOVE 0 TO WS-COUNTER.
        
        CALCULATE-SALARY.
       *    Try typing variable names to see completions with PIC info
            COMPUTE WS-RESULT = WS-EMP-SALARY * 1.10.
            ADD 1 TO WS-COUNTER.
            IF STATUS-ACTIVE
                DISPLAY "Employee is active"
            ELSE
                DISPLAY "Employee is inactive"
            END-IF.
        
        DISPLAY-RESULTS.
            DISPLAY "Employee ID: " WS-EMP-ID.
            DISPLAY "Employee Name: " WS-EMP-NAME.
            DISPLAY "Calculated Result: " WS-RESULT.
            DISPLAY "Counter: " WS-COUNTER.
