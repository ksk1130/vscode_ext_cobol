        IDENTIFICATION DIVISION.
        PROGRAM-ID. TYPE-MISMATCH-TEST.
        AUTHOR. Test Program.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  NUMERIC-VARS.
            05  NUM-SHORT      PIC 9(3).
            05  NUM-LONG       PIC 9(8).
            05  NUM-DECIMAL    PIC 9(5)V99.
            05  NUM-DECIMAL2   PIC 9(5)V9.
        
        01  ALPHA-VARS.
            05  ALPHA-SHORT    PIC X(10).
            05  ALPHA-LONG     PIC X(50).
        
        01  ALPHANUMERIC-VAR   PIC X(20).
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
       *    OK: Small to large (same type)
            MOVE NUM-SHORT TO NUM-LONG.
            MOVE ALPHA-SHORT TO ALPHA-LONG.
            
       *    WARNING: Large to small (size mismatch - data truncation)
            MOVE NUM-LONG TO NUM-SHORT.
            MOVE ALPHA-LONG TO ALPHA-SHORT.
            
       *    WARNING: Type mismatch (numeric to alphanumeric)
            MOVE NUM-SHORT TO ALPHA-SHORT.
            
       *    WARNING: Type mismatch (alphanumeric to numeric)
            MOVE ALPHA-SHORT TO NUM-SHORT.
            
       *    WARNING: Decimal precision loss (2 decimals to 0)
            MOVE NUM-DECIMAL TO NUM-SHORT.
            
       *    WARNING: Decimal precision loss (2 decimals to 1)
            MOVE NUM-DECIMAL TO NUM-DECIMAL2.
            
            STOP RUN.
