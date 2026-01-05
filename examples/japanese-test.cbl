       IDENTIFICATION DIVISION.
       PROGRAM-ID. JAPANESE-TEST.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  顧客情報.
           05  顧客番号        PIC 9(8).
           05  顧客名          PIC X(20).
           05  郵便番号        PIC X(8).
           05  住所            PIC X(50).
       01  商品マスタ.
           05  商品コード      PIC X(10).
           05  商品名          PIC X(30).
           05  単価            PIC 9(6).
           05  在庫数          PIC 9(5).
       01  計算結果            PIC 9(8).
       01  メッセージ          PIC X(50).
       
       PROCEDURE DIVISION.
       メイン処理.
           PERFORM 初期化処理.
           PERFORM データ入力処理.
           PERFORM 計算処理.
           PERFORM 結果表示処理.
           STOP RUN.
       
       初期化処理.
           MOVE ZERO TO 計算結果.
           MOVE SPACES TO メッセージ.
       
       データ入力処理.
           MOVE 12345678 TO 顧客番号.
           MOVE "山田太郎" TO 顧客名.
           MOVE "〒100-0001" TO 郵便番号.
           MOVE "東京都千代田区千代田1-1" TO 住所.
           MOVE "ITEM001" TO 商品コード.
           MOVE "テスト商品" TO 商品名.
           MOVE 1000 TO 単価.
           MOVE 50 TO 在庫数.
       
       計算処理.
           COMPUTE 計算結果 = 単価 * 在庫数.
       
       結果表示処理.
           DISPLAY "顧客番号: " 顧客番号.
           DISPLAY "顧客名: " 顧客名.
           DISPLAY "商品名: " 商品名.
           DISPLAY "計算結果: " 計算結果.
           MOVE "処理が完了しました" TO メッセージ.
           DISPLAY メッセージ.
