      * ============================================================================
      * COMPREHENSIVE-TEST.cbl
      * 
      * This file consolidates multiple LSP extension features for regression
      * testing. It demonstrates all major capabilities:
      * - Japanese language support (日本語変数名・パラグラフ名)
      * - COPY statements (including multiple lines, disjoining, joining)
      * - CALL statements (including multiline with multiple USING parameters)
      * - 88-level condition names
      * - Type checking and variable references
      * - Qualified names and nested structures
      * ============================================================================
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPREHENSIVE-TEST.
       AUTHOR. COBOL LSP Extension Team.
       
      * ============================================================================
      * SECTION 1: DATA DIVISION WITH JAPANESE NAMES AND COPYBOOKS
      * ============================================================================
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD               PIC X(100).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD              PIC X(100).
       
       WORKING-STORAGE SECTION.
      * ---- Test 88-Level Condition Names ----
       01  WS-FILE-STATUS             PIC XX.
           88  FILE-OK                VALUE '00'.
           88  FILE-EOF               VALUE '10'.
           88  FILE-ERROR             VALUE '99'.
       
      * ---- Japanese Variable Names ----
       01  日本語データ構造.
           05  顧客番号                 PIC 9(6).
           05  顧客名                   PIC X(30).
           05  売上高                   PIC 9(8)V99.
           05  処理状態                 PIC X(1).
               88  状態-有効             VALUE 'A'.
               88  状態-無効             VALUE 'I'.
       
      * ---- English Variable Names (Type Testing) ----
       01  WS-NUMERIC-VARS.
           05  WS-SHORT-NUM           PIC 9(3).
           05  WS-MEDIUM-NUM          PIC 9(5)V99.
           05  WS-LONG-NUM            PIC 9(10)V99.
       
       01  WS-ALPHA-VARS.
           05  WS-SHORT-ALPHA         PIC X(10).
           05  WS-LONG-ALPHA          PIC X(50).
       
      * ---- Input/Output Parameters ----
       01  WS-INPUT-DATA              PIC X(100).
       01  WS-OUTPUT-DATA-1           PIC X(100).
       01  WS-OUTPUT-DATA-2           PIC X(100).
       01  WS-PROGRAM-NAME            PIC X(30).
       01  WS-COUNTER                 PIC 9(5) VALUE ZERO.
       
      * ---- COPYBOOK References ----
      * Test COPY with DISJOINING and JOINING
       COPY CUSTOMER-DATA.
       COPY PRODUCT-DATA
            DISJOINING OLD
            JOINING NEW AS PREFIX.
       
      * ============================================================================
      * SECTION 2: PROCEDURE DIVISION - TESTING ALL FEATURES
      * ============================================================================
       
       PROCEDURE DIVISION.
       
      * ============================================================================
      * 2.1: Initialize Variables (日本語パラグラフ名)
      * ============================================================================
       初期化処理.
           MOVE 'Test Input' TO WS-INPUT-DATA.
           MOVE 'Test Output 1' TO WS-OUTPUT-DATA-1.
           MOVE 'Test Output 2' TO WS-OUTPUT-DATA-2.
           MOVE 'SAMPLE-PROG' TO WS-PROGRAM-NAME.
           
           SET FILE-OK TO TRUE.
           SET 状態-有効 TO TRUE.
           
           MOVE 123 TO WS-SHORT-NUM.
           MOVE 12345 TO WS-MEDIUM-NUM.
           MOVE 1000000 TO WS-LONG-NUM.
           
           MOVE 'ShortStr' TO WS-SHORT-ALPHA.
           MOVE 'This is a longer string' TO WS-LONG-ALPHA.
           
           MOVE 100001 TO 顧客番号.
           MOVE 'Yamada Taro' TO 顧客名.
           MOVE 5000000 TO 売上高.
       
      * ============================================================================
      * 2.2: Test Variable References and Type Checking
      * ============================================================================
       変数参照テスト.
      *    OK: Small to large (same type)
           MOVE WS-SHORT-NUM TO WS-LONG-NUM.
           MOVE WS-SHORT-ALPHA TO WS-LONG-ALPHA.
           
      *    Type mismatch examples (for static analysis)
           MOVE WS-LONG-NUM TO WS-SHORT-NUM.
           MOVE WS-LONG-ALPHA TO WS-SHORT-ALPHA.
       
      * ============================================================================
      * 2.3: Test Qualified Names (Nested Structure References)
      * ============================================================================
       定義済み名テスト.
      *    These would reference copybook structures with qualified names
      *    CUSTOMER-DATA.顧客ID TO 顧客番号
      *    PRODUCT-DATA.製品名 TO 顧客名
           DISPLAY 'Test qualified names'.
       
      * ============================================================================
      * 2.4: Test CALL Statements (Single and Multiline)
      * ============================================================================
       プログラム呼出テスト.
      *    Test 1: Single line CALL
           CALL 'SAMPLE-PROGRAM' USING WS-INPUT-DATA
                                       WS-OUTPUT-DATA-1
           END-CALL.
           
      *    Test 2: Multiline CALL with 2 parameters
           CALL 'TWO-PARAM-PROG'
               USING WS-INPUT-DATA
                     WS-OUTPUT-DATA-1
           END-CALL.
           
      *    Test 3: Multiline CALL with 3 parameters (the issue we fixed)
           CALL 'THREE-PARAM-PROG'
               USING WS-INPUT-DATA
                     WS-OUTPUT-DATA-1
                     WS-OUTPUT-DATA-2
           END-CALL.
           
      *    Test 4: Dynamic CALL using variable
           CALL WS-PROGRAM-NAME
               USING WS-INPUT-DATA
           END-CALL.
       
      * ============================================================================
      * 2.5: Test PERFORM (Paragraph and Section Jumps)
      * ============================================================================
       段落呼出テスト.
      *    Test paragraph jumps
           PERFORM 初期化処理.
           PERFORM 変数参照テスト.
           PERFORM 定義済み名テスト.
           PERFORM プログラム呼出テスト.
       
      * ============================================================================
      * 2.6: Test 88-Level Conditions
      * ============================================================================
       条件テスト.
           SET FILE-OK TO TRUE.
           IF FILE-OK
               DISPLAY 'File status is OK'
           END-IF.
           
           SET FILE-EOF TO TRUE.
           IF FILE-EOF
               DISPLAY 'File reached EOF'
           END-IF.
           
           SET 状態-有効 TO TRUE.
           IF 状態-有効
               DISPLAY '顧客は有効です'
           END-IF.
       
      * ============================================================================
      * 2.7: Main Processing
      * ============================================================================
       MAIN-PROCEDURE.
           DISPLAY 'Starting Comprehensive Test'.
           
      *    Run all test sections
           PERFORM 初期化処理.
           PERFORM 変数参照テスト.
           PERFORM 定義済み名テスト.
           PERFORM プログラム呼出テスト.
           PERFORM 段落呼出テスト.
           PERFORM 条件テスト.
           
           DISPLAY 'Comprehensive Test Complete'.
           STOP RUN.
       
      * ============================================================================
      * END OF PROGRAM
      * ============================================================================
