# COBOL LSP 拡張機能のサンプルファイル

このディレクトリには、COBOL LSP 拡張機能の各機能を試すためのサンプルファイルが含まれています。

## intellisense-example.cbl

**NEW!** IntelliSense（自動補完）機能のデモンストレーション用ファイルです。

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
