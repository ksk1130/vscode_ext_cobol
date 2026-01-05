# DISJOINING/JOINING AS PREFIX Support / DISJOINING/JOINING AS PREFIX サポート

## 概要 / Overview

### 日本語

NETCOBOL拡張構文である `DISJOINING/JOINING AS PREFIX` をサポートしました。この機能により、COPYBOOKの変数接頭辞を置き換えて使用する際の「定義へジャンプ」が正しく動作するようになりました。

また、複数行にわたるCOPY文もサポートしています。

### English

Added support for NETCOBOL extended syntax `DISJOINING/JOINING AS PREFIX`. This feature enables "Go to Definition" to work correctly when using COPYBOOKs with prefix replacement.

Multi-line COPY statements are also supported.

## 構文 / Syntax

### 単一行 / Single Line
```cobol
COPY <COPYBOOK名> DISJOINING <旧プリフィックス> JOINING <新プリフィックス> AS PREFIX.
```

### 複数行 / Multi-Line
```cobol
COPY <COPYBOOK名>
    DISJOINING <旧プリフィックス> 
    JOINING <新プリフィックス> 
    AS PREFIX.
```

### 例 / Example

**COPYBOOK (TEST-COPY.cpy):**
```cobol
       01  HOGE-変数       PIC X(02).
       01  HOGE-DATA      PIC 9(04).
       01  HOGE-RECORD.
           05  HOGE-FIELD1  PIC X(10).
           05  HOGE-FIELD2  PIC 9(05).
```

**参照元 COBOL:**
```cobol
       COPY TEST-COPY DISJOINING HOGE JOINING FUGA AS PREFIX.
       
       PROCEDURE DIVISION.
           MOVE 'AB' TO FUGA-変数.
           MOVE 1234 TO FUGA-DATA.
           DISPLAY FUGA-FIELD1.
```

この場合：
- COPYBOOK内の `HOGE-変数` は `FUGA-変数` として使用可能
- COPYBOOK内の `HOGE-DATA` は `FUGA-DATA` として使用可能
- COPYBOOK内の `HOGE-FIELD1` は `FUGA-FIELD1` として使用可能

In this case:
- `HOGE-変数` in the COPYBOOK can be used as `FUGA-変数`
- `HOGE-DATA` in the COPYBOOK can be used as `FUGA-DATA`
- `HOGE-FIELD1` in the COPYBOOK can be used as `FUGA-FIELD1`

## 機能 / Features

### 1. 接頭辞置換 / Prefix Replacement

COPYBOOKをロードする際、指定された接頭辞を自動的に置き換えます。

When loading the COPYBOOK, the specified prefix is automatically replaced.

**変換ルール / Transformation Rule:**
```
<旧プリフィックス>-<変数名> → <新プリフィックス>-<変数名>
HOGE-変数 → FUGA-変数
```

### 2. 定義へジャンプ / Go to Definition

参照元COBOLで `FUGA-変数` にカーソルを置いてF12キーを押すと、COPYBOOK内の元の定義 `HOGE-変数` へジャンプします。

When you place the cursor on `FUGA-変数` in the referencing COBOL and press F12, it jumps to the original definition `HOGE-変数` in the COPYBOOK.

### 3. ホバー情報 / Hover Information

`FUGA-変数` にマウスをホバーすると、COPYBOOK内の元の定義情報が表示されます。

When you hover over `FUGA-変数`, the original definition information from the COPYBOOK is displayed.

### 4. Unicode文字サポート / Unicode Character Support

日本語を含むUnicode文字の変数名にも対応しています。

Supports variable names containing Unicode characters including Japanese.

```cobol
HOGE-変数       → FUGA-変数
HOGE-データ項目  → FUGA-データ項目
HOGE-カウンター  → FUGA-カウンター
```

## 技術詳細 / Technical Details

### 実装 / Implementation

#### 0. 複数行COPY文の結合 / Multi-Line COPY Statement Combination

`collectCopyStatements()` 関数が複数行にわたるCOPY文を自動的に結合します：

```typescript
function collectCopyStatements(lines: string[]): Array<{statement: string, startLine: number}> {
    // COPY文の開始を検出
    // ピリオド(.)で終わるまで行を結合
    // 結合された完全なCOPY文を返す
}
```

これにより、以下のような複数行COPY文も正しく処理されます：

```cobol
COPY COPYBOOK
    DISJOINING HOGE 
    JOINING FUGA 
    AS PREFIX.
```

#### 1. ルール抽出 / Rule Extraction

`copybookResolver.extractDisjoiningJoiningRules()` が COPY文から DISJOINING/JOINING AS PREFIX 句を抽出します。

```typescript
const pattern = /DISJOINING\s+([\w\u0080-\uFFFF\-]+)\s+JOINING\s+([\w\u0080-\uFFFF\-]+)\s+AS\s+PREFIX/gi;
```

#### 2. 接頭辞置換 / Prefix Replacement

`applyReplacingRules()` で接頭辞を置換：

```typescript
if (rule.isPrefix) {
    const regex = new RegExp(`(^|\\s|\\.)${escapedFrom}(-[\w\u0080-\uFFFF\-]+)`, 'gi');
    result = result.replace(regex, `$1${rule.to}$2`);
}
```

#### 3. 逆変換（検索用）/ Reverse Transformation (For Search)

ユーザーが `FUGA-変数` を検索する際、COPYBOOK内の元の名前 `HOGE-変数` に変換して検索：

When the user searches for `FUGA-変数`, convert to the original name `HOGE-変数` in the COPYBOOK:

```typescript
if (rule.isPrefix) {
    const regex = new RegExp(`^${escapedTo}(-[\w\u0080-\uFFFF\-]+)$`, 'i');
    searchWord = searchWord.replace(regex, `${rule.from}$1`);
}
```

### 変更されたファイル / Modified Files

- `server/src/server.ts`:
  - `collectCopyStatements()` 関数を追加 - 複数行COPY文を結合
  - `searchInCopybooksWithPath()` で複数行COPY文に対応
  - `searchInCopybooks()` で複数行COPY文に対応
  - `loadCopybooksFromDocument()` で複数行COPY文に対応

- `server/src/resolver/copybookResolver.ts`:
  - `ReplacingRule` インターフェースに `isPrefix` プロパティを追加
  - `extractDisjoiningJoiningRules()` で `isPrefix: true` を設定
  - `applyReplacingRules()` で接頭辞置換ロジックを追加

## 使用例 / Usage Examples

### 例1: 基本的な使用 / Example 1: Basic Usage

**COPYBOOK (CUSTOMER.cpy):**
```cobol
       01  CUST-RECORD.
           05  CUST-ID      PIC 9(08).
           05  CUST-NAME    PIC X(50).
           05  CUST-ADDRESS PIC X(100).
```

**COBOL:**
```cobol
       COPY CUSTOMER DISJOINING CUST JOINING CLIENT AS PREFIX.
       
       PROCEDURE DIVISION.
           MOVE 12345678 TO CLIENT-ID.
           MOVE 'John Doe' TO CLIENT-NAME.
           *> F12で CLIENT-ID から COPYBOOK の CUST-ID へジャンプ可能
```

### 例2: 複数行COPY文 / Example 2: Multi-Line COPY Statement

**COPYBOOK (DATA.cpy):**
```cobol
       01  OLD-RECORD.
           05  OLD-ID       PIC 9(08).
           05  OLD-NAME     PIC X(50).
```

**COBOL:**
```cobol
       COPY DATA
           DISJOINING OLD
           JOINING NEW
           AS PREFIX.
       
       PROCEDURE DIVISION.
           MOVE 12345678 TO NEW-ID.
           DISPLAY NEW-NAME.
           *> F12で NEW-ID から COPYBOOK の OLD-ID へジャンプ可能
```

### 例3: 日本語変数名 / Example 3: Japanese Variable Names

**COPYBOOK (データ定義.cpy):**
```cobol
       01  旧プリフィックス-レコード.
           05  旧プリフィックス-ID       PIC 9(08).
           05  旧プリフィックス-名前     PIC X(50).
           05  旧プリフィックス-住所     PIC X(100).
```

**COBOL:**
```cobol
       COPY データ定義 
           DISJOINING 旧プリフィックス 
           JOINING 新プリフィックス 
           AS PREFIX.
       
       PROCEDURE DIVISION.
           MOVE 12345678 TO 新プリフィックス-ID.
           DISPLAY 新プリフィックス-名前.
```

### 例4: 全角区切り文字 / Example 4: Full-Width Separator

**COPYBOOK (MASTER.cpy):**
```cobol
       01  古ーレコード.
           05  古ーID         PIC 9(08).
           05  古ー名前       PIC X(50).
           05  古ーデータ     PIC X(100).
```

**COBOL:**
```cobol
       COPY MASTER
           DISJOINING 古
           JOINING 新
           AS PREFIX.
       
       PROCEDURE DIVISION.
           *> 全角「ー」区切り文字にも対応
           MOVE 12345678 TO 新ーID.
           DISPLAY 新ー名前.
           *> F12 キーで 新ーID から COPYBOOK の 古ーID へジャンプ
```

### 例5: REPLACINGとの組み合わせ / Example 5: Combined with REPLACING

```cobol
       COPY COPYBOOK
           DISJOINING OLD JOINING NEW AS PREFIX
           REPLACING ==CONST1== BY ==CONST2==.
```

両方のルールが適用されます：
- 接頭辞: `OLD-変数` → `NEW-変数`
- 定数: `CONST1` → `CONST2`

Both rules are applied:
- Prefix: `OLD-変数` → `NEW-変数`  
- Constant: `CONST1` → `CONST2`

## 制限事項 / Limitations

1. **区切り文字必須 / Separator Required**:
   - 接頭辞の後に必ず区切り文字が必要です
   - The prefix must be followed by a separator character
   - サポートされる区切り文字 / Supported separators:
     - `-` (U+002D): ASCII hyphen
     - `ー` (U+30FC): Full-width katakana prolonged sound mark (Shift-JIS 817C)
   - 例 / Examples:
     - `HOGE-変数` ✓
     - `HOGEー変数` ✓
     - `HOGE変数` ✗ (no separator)

2. **AS PREFIX のみ / AS PREFIX Only**:
   - 現在は `AS PREFIX` のみサポート
   - Currently only `AS PREFIX` is supported
   - `AS SUFFIX` や他のバリエーションは未対応
   - `AS SUFFIX` or other variations are not supported

## 参考資料 / References

- NETCOBOL Extended Syntax
- COBOL COPY statement with REPLACING
- DISJOINING and JOINING clauses

## 関連コミット / Related Commits

- Initial Unicode support: 72fd05d
- This feature: Support for DISJOINING/JOINING AS PREFIX
