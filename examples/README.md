# COBOL LSP 拡張機能のサンプルファイル

このディレクトリには、COBOL LSP 拡張機能の各機能を試すためのサンプルファイルが含まれています。

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
