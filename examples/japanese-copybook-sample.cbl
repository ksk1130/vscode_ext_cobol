       IDENTIFICATION DIVISION.
       PROGRAM-ID. JAPANESE-COPYBOOK-TEST.
      *================================================================
      * 日本語COPYBOOK使用サンプルプログラム
      * Sample program using Japanese COPYBOOKs with 「ー」
      *
      * このプログラムは以下をテストします:
      * - 日本語変数名を含むCOPYBOOKの読み込み
      * - 長音記号「ー」を含む変数名の解決
      * - 複数のCOPYBOOKで同じパターンの変数名の追跡
      *================================================================
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 社員マスターCOPYBOOKを参照
       COPY 社員マスター.
       
      * 顧客データーCOPYBOOKを参照
       COPY 顧客データー.
       
      * 製品マスターCOPYBOOKを参照
       COPY 製品マスター.
       
      * ワーク変数
       01  処理ーカウンター               PIC 9(8) VALUE ZERO.
       01  エラーーフラグ                 PIC X(1) VALUE 'N'.
           88  エラーあり                 VALUE 'Y'.
           88  エラーなし                 VALUE 'N'.
       
       PROCEDURE DIVISION.
       メインー処理.
      *    社員情報の設定
           MOVE 12345678 TO 社員ーコード
           MOVE "山田　太郎" TO 社員ー氏名
           MOVE "営業部" TO 部署ーコード
           MOVE 05 TO 役職ーレベル
           MOVE 2020 TO 入社ー年
           MOVE 04 TO 入社ー月
           MOVE 01 TO 入社ー日
           MOVE 5000000.00 TO 基本給ー金額
           MOVE 50000.00 TO 手当ー金額
           MOVE 10000.00 TO 控除ー金額
           SET 優秀ーランク TO TRUE
           SET 在籍中 TO TRUE
           
      *    顧客情報の設定
           MOVE 9876543210 TO 顧客ーID
           MOVE "タナカ" TO 姓ーカナ
           MOVE "ハナコ" TO 名ーカナ
           MOVE "田中" TO 姓ー漢字
           MOVE "花子" TO 名ー漢字
           MOVE 1234567 TO 郵便番号
           MOVE "東京都" TO 住所ー都道府県
           MOVE "渋谷区" TO 住所ー市区町村
           MOVE "道玄坂1-2-3" TO 住所ー番地
           MOVE "03-1234-5678" TO 電話番号ー自宅
           MOVE "090-1234-5678" TO 電話番号ーケータイ
           MOVE "tanaka@example.jp" TO メールアドレス
           SET プレミアム会員 TO TRUE
           MOVE 20210101 TO 登録ー日付
           MOVE 20231215 TO 最終利用ー日付
           MOVE 1234567.89 TO 購入ー累計金額
           
      *    製品情報の設定
           MOVE "PROD-12345-ABC" TO 製品ーコード
           MOVE "高性能ノートパソコン" TO 製品ー名称ー日本語
           MOVE "High Performance Laptop" TO 製品ー名称ー英語
           MOVE "COMP-001" TO カテゴリーコード
           MOVE "MAKER-999" TO メーカーコード
           MOVE 200000.00 TO 標準価格
           MOVE 180000.00 TO 販売価格
           MOVE 120000.00 TO 原価
           MOVE 0.10 TO 税率
           MOVE 150 TO 在庫数量
           MOVE 20 TO 発注点
           MOVE 500 TO 最大在庫数
           MOVE "WH001" TO 倉庫ーコード
           MOVE 35.50 TO 長さーセンチ
           MOVE 25.00 TO 幅ーセンチ
           MOVE 2.50 TO 高さーセンチ
           MOVE 1800 TO 重量ーグラム
           SET 販売中 TO TRUE
           
      *    処理結果の表示
           DISPLAY "==================================="
           DISPLAY "日本語COPYBOOK使用サンプル実行結果"
           DISPLAY "==================================="
           DISPLAY " "
           DISPLAY "【社員情報】"
           DISPLAY "社員コード: " 社員ーコード
           DISPLAY "氏名: " 社員ー氏名
           DISPLAY "部署: " 部署ーコード
           DISPLAY "基本給: " 基本給ー金額
           DISPLAY " "
           DISPLAY "【顧客情報】"
           DISPLAY "顧客ID: " 顧客ーID
           DISPLAY "氏名: " 姓ー漢字 名ー漢字
           DISPLAY "住所: " 住所ー都道府県 住所ー市区町村
           DISPLAY "累計購入金額: " 購入ー累計金額
           DISPLAY " "
           DISPLAY "【製品情報】"
           DISPLAY "製品コード: " 製品ーコード
           DISPLAY "製品名: " 製品ー名称ー日本語
           DISPLAY "販売価格: " 販売価格
           DISPLAY "在庫数量: " 在庫数量
           DISPLAY " "
           DISPLAY "==================================="
           DISPLAY "処理が正常に完了しました"
           DISPLAY "==================================="
           
           STOP RUN.
