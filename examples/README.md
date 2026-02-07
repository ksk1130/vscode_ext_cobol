# COBOL LSP 拡張機能のサンプルファイル

このディレクトリには、COBOL LSP 拡張機能の各機能を試すためのサンプルファイルが含まれています。

## ファイル一覧

### メインテストファイル

- **[comprehensive-test.cbl](#comprehensive-testcbl)** ⭐ 推奨
  - すべての主要機能を1つのファイルで検証可能
  - リグレッション対策用

### 機能別デモファイル

- **[intellisense-example.cbl](#intellisense-examplecbl)** - IntelliSense デモ
- **[outline-example.cbl](#outline-examplecbl)** - アウトラインビュー デモ
- **[special-constants-test.cbl](#special-constants-testcbl)** - 特殊定数テスト
- **[qualified-name-test.cbl](#qualified-name-testcbl)** - 定義済み名テスト
- **call-jump-main.cbl / call-jump-sub.cbl** - CALLジャンプの基本デモ

### COPY関連テスト（エッジケース）

- **copy-disjoining-comment-test.cbl** - COPY複数行 + インラインコメント
- **copy-inline-comment-test.cbl** - COPY + インラインコメント
- **copy-multiple-spaces-test.cbl** - COPY + 複数スペース処理
- **fd-copy-test.cbl** - FD セクションでの COPY
- **multiple-copybooks-test.cbl** - 複数COPYBOOK参照
- **japanese-copybook-sample.cbl** - 日本語COPYBOOK

---

## comprehensive-test.cbl

⭐ **推奨**: すべての主要機能を1つのファイルで検証できる統合テストファイルです。リグレッション対策用です。

### テスト対象機能

このファイルは以下の機能をカバーしています：

1. **日本語言語サポート**:
   - 日本語変数名：`顧客番号`, `顧客名`, `売上高` など
   - 日本語パラグラフ名：`初期化処理`, `変数参照テスト`, `プログラム呼出テスト` など
   - 88レベルの日本語条件名：`状態-有効`, `状態-無効`

2. **複数行CALL文（改行対応）** ⭐ **新機能**:
   - 2行にわたるCALL文
   - 3行以上の複数パラメータを持つCALL文
   - 動的CALL（変数を使用）
   - 複数行USING句での変数参照ジャンプが正常に動作

3. **COPY文の処理**:
   - 基本的なCOPY参照
   - DISJOINING/JOINING句を含むCOPY文

4. **88レベル条件名**:
   - 複数の88レベル条件の定義
   - 条件名の重複排除（アウトラインに1度だけ表示）

5. **変数参照と型チェック**:
   - 数値型変数の参照
   - 文字列型変数の参照
   - 型の不整合検出（警告対象）

6. **PERFORM文によるジャンプ**:
   - パラグラフへの参照
   - 日本語パラグラフ名への参照

### 検証項目

このファイルを使用して以下の項目を検証できます：

- ✓ 日本語識別子の変数定義・参照ジャンプ
- ✓ **複数行にわたるCALL文でのプログラム名抽出**
- ✓ **複数行USING句での変数参照ジャンプ**
- ✓ 88レベル条件名のアウトライン表示（重複排除）
- ✓ COPY文の参照解決
- ✓ PERFORM文によるパラグラフジャンプ
- ✓ ホバー時の型情報表示
- ✓ IntelliSense補完候補

### 使い方

1. VS Code でこのファイルを開きます
2. 以下の操作を試して、各機能が正常に動作することを確認してください：

```
- 日本語変数（顧客番号など）にカーソルを置いてホバー → 型情報が表示される
- CALL文内の変数にCtrl+Click → 定義位置にジャンプ
- PERFORM文の日本語パラグラフ名をCtrl+Click → 定義位置にジャンプ
- アウトラインビュー表示 (Ctrl+Shift+O) → 階層構造が正しく表示される
- 88レベル条件名（FILE-OK など）がアウトラインに1度だけ表示される
```

---

## intellisense-example.cbl

IntelliSense（自動補完）機能のデモンストレーション用ファイルです。

### IntelliSense機能の確認

このファイルでは、以下のIntelliSense機能を試すことができます：

1. **COBOLキーワードの補完**:
   - `MOV` と入力して Ctrl+Space を押すと、`MOVE` などのキーワードが候補として表示されます
   - `PER` と入力すると `PERFORM` が補完されます
   - `IF`, `COMPUTE`, `ADD`, `DISPLAY` など、すべてのCOBOLキーワードが補完対象です

2. **変数名の補完**:
   - `WS-` と入力すると、すべての WS- で始まる変数が候補として表示されます
   - 各変数には Level 番号や PIC 句も表示されるので、どの変数を使うべきか簡単に判断できます
   - COPYBOOK 内で定義された変数も補完候補に含まれます

3. **パラグラフ/セクション名の補完**:
   - `PERFORM ` の後で Ctrl+Space を押すと、パラグラフとセクションの一覧が表示されます
   - パラグラフには「関数」アイコン、セクションには「クラス」アイコンが表示されます

4. **プログラム名の補完**:
   - `CALL ` の後で Ctrl+Space を押すと、ワークスペース内のプログラム名が表示されます
   - プログラム名には自動的に引用符が付加されます

5. **COPYBOOK名の補完**:
   - `COPY ` の後で Ctrl+Space を押すと、copybooks フォルダ内のファイルが候補として表示されます

6. **シグネチャヘルプ**:
   - `CALL "PROGRAM-NAME"` と入力すると、USING パラメータのヒントが表示されます

### 使い方

1. VS Code でこのファイルを開きます
2. コードを編集しながら、次の操作を試してください：
   - **Ctrl+Space**: 現在のコンテキストに応じた補完候補を表示
   - **文字入力中**: 自動的に補完候補が表示されることもあります
   - **矢印キーまたはマウス**: 補完候補を選択
   - **Enter または Tab**: 選択した候補を挿入

### 補完のトリガー文字

以下の文字を入力すると、自動的に補完候補が表示されます：
- スペース (` `)
- ハイフン (`-`)
- ピリオド (`.`)

---

## outline-example.cbl

アウトライン表示機能のデモンストレーション用ファイルです。

### アウトラインビューでの表示イメージ

VS Code でこのファイルを開き、アウトラインビュー (Ctrl+Shift+O または View → Outline) を表示すると、以下のような階層構造が表示されます：

```
├─ IDENTIFICATION DIVISION
├─ DATA DIVISION
│  ├─ CUSTOMER-RECORD (Level 1)
│  │  ├─ CUSTOMER-ID (Level 5 PIC 9(8))
│  │  ├─ CUSTOMER-NAME (Level 5 PIC X(50))
│  │  ├─ CUSTOMER-ADDRESS (Level 5)
│  │  │  ├─ STREET (Level 10 PIC X(50))
│  │  │  ├─ CITY (Level 10 PIC X(30))
│  │  │  └─ ZIP-CODE (Level 10 PIC 9(5))
│  │  └─ ACCOUNT-BALANCE (Level 5 PIC 9(10)V99)
│  ├─ STATUS-CODE (Level 1 PIC 99)
│  │  ├─ STATUS-OK (Level 88)
│  │  └─ STATUS-ERROR (Level 88)
│  └─ COUNTERS (Level 1)
│     ├─ TOTAL-PROCESSED (Level 5 PIC 9(6))
│     └─ TOTAL-ERRORS (Level 5 PIC 9(6))
└─ PROCEDURE DIVISION
   ├─ MAIN-SECTION (Section)
   ├─ MAIN-PARAGRAPH (Paragraph)
   ├─ INITIALIZE-DATA (Paragraph)
   ├─ PROCESS-CUSTOMER (Paragraph)
   ├─ DISPLAY-STATISTICS (Paragraph)
   └─ CLEANUP (Paragraph)
```

### 機能の確認

1. **Division 表示**: COBOL の各 Division（見出し部、環境部、データ部、手続き部）がアウトラインの最上位に表示されます
2. **階層表示**: 変数のレベル番号に基づいて、親子関係が自動的に構築されます
3. **シンボルタイプ**: 
   - Division は「モジュール」アイコンで表示
   - 変数は「変数」アイコンで表示
   - 段落は「関数」アイコンで表示
   - セクションは「クラス」アイコンで表示
4. **詳細情報**: レベル番号と PIC 句がシンボル名の右側に表示されます
5. **ナビゲーション**: アウトライン内のシンボルをクリックすると、該当行にジャンプします

### その他の機能

このファイルでは、以下の機能も試すことができます：

- **定義ジャンプ**: PERFORM 文の段落名を Ctrl+Click
- **ホバー**: 変数にカーソルを置くと、レベル・PIC・定義位置が表示
- **診断**: 未定義変数や未使用変数の警告

---

## special-constants-test.cbl

COBOL の特殊定数（ZERO, SPACE, LOW-VALUE, HIGH-VALUE など）をテストするファイルです。

---

## qualified-name-test.cbl

ネストされたデータ構造での定義済み名（修飾名）テストファイルです。

---

## call-jump-main.cbl / call-jump-sub.cbl

CALL文から外部プログラムへジャンプする動作確認用の最小サンプルです。

### 使い方

1. `Cobol: Program Search Paths` に `./examples` を追加
2. call-jump-main.cbl を開く
3. `CALL 'CALL-JUMP-SUB'` を Ctrl+Click → call-jump-sub.cbl の PROGRAM-ID にジャンプ

---

## COPY関連テストファイル

各ファイルは COPY 文の特定のエッジケースをテストするために存在します：

- **copy-disjoining-comment-test.cbl** - COPY複数行 + インラインコメント処理
- **copy-inline-comment-test.cbl** - COPY行内のコメント処理
- **copy-multiple-spaces-test.cbl** - 複数スペースの処理
- **fd-copy-test.cbl** - FD セクションでの COPY 使用
- **multiple-copybooks-test.cbl** - 複数のCOPYBOOK参照
- **japanese-copybook-sample.cbl** - 日本語変数を含むCOPYBOOK

---

## 推奨される検証フロー

新機能追加や修正後は、以下の順序で検証することをお勧めします：

1. **comprehensive-test.cbl** を開く → 主要機能が動作することを確認
2. 該当する専門テストファイルを開く → 特定の機能を詳細に検証
3. 関連する COPY テストファイルを確認 → エッジケースが処理されていることを確認
