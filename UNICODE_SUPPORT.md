# Shift-JIS and Unicode Character Support / Shift-JISとUnicode文字のサポート

## 概要 / Overview

### 日本語

Shift-JIS の 817C「ー」(全角の伸ばし棒、長音記号) を含む、すべての日本語文字が正規表現で正しく認識されるように修正しました。

以前のコードでは、一部の正規表現で `\b` (単語境界) を使用していたため、日本語文字を含む識別子が正しくマッチしませんでした。

### English

Fixed to properly recognize all Japanese characters including Shift-JIS 817C「ー」(full-width prolonged sound mark, chōonpu) in regular expressions.

Previous code used `\b` (word boundary) in some regex patterns, which prevented proper matching of identifiers containing Japanese characters.

## 技術詳細 / Technical Details

### 文字コード / Character Encoding

Shift-JIS 817C「ー」は Unicode では U+30FC (KATAKANA-HIRAGANA PROLONGED SOUND MARK) です。
この文字は `\u0080-\uFFFF` の範囲に含まれるため、パターン `[\w\u0080-\uFFFF\-]+` で正しく認識されます。

Shift-JIS 817C「ー」is U+30FC (KATAKANA-HIRAGANA PROLONGED SOUND MARK) in Unicode.
This character falls within the `\u0080-\uFFFF` range, so it is properly recognized by the pattern `[\w\u0080-\uFFFF\-]+`.

```
Character: ー
Unicode: U+30FC
Decimal: 12540
Shift-JIS: 817C
```

### 問題となっていた箇所 / Problem Areas

#### 1. 変数使用チェック (server.ts)

**修正前 / Before:**
```typescript
const words = contentLine.match(/\b[\w\u0080-\uFFFF\-]+\b/gi);
```

**修正後 / After:**
```typescript
const words = contentLine.match(/[\w\u0080-\uFFFF\-]+/gi);
```

**問題 / Issue:**
- `\b` (単語境界) は ASCII 文字のみを単語文字として認識
- 日本語文字は単語文字として認識されないため、マッチしない

**理由 / Reason:**
- `\b` (word boundary) only recognizes ASCII characters as word characters
- Japanese characters are not recognized as word characters, so they don't match

#### 2. REPLACING ルール適用 (copybookResolver.ts)

**修正前 / Before:**
```typescript
const regex = new RegExp(`\\b${rule.from}\\b`, 'gi');
```

**修正後 / After:**
```typescript
const escapedFrom = rule.from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
const regex = new RegExp(`(?<=^|\\s|-|\\.)${escapedFrom}(?=\\s|-|\\.|$)`, 'gi');
```

**変更点 / Changes:**
- `\b` を使用せず、前後のコンテキスト（スペース、ハイフン、ピリオド、文字列境界）で判定
- 正規表現特殊文字のエスケープを追加

**Improvements:**
- Use context (space, hyphen, period, string boundary) instead of `\b`
- Added escaping of regex special characters

## テスト結果 / Test Results

### 識別子のマッチング / Identifier Matching

```
✓ テストー
✓ データー
✓ カウンター
✓ 結果ー
✓ データー項目
✓ TEST-ー
✓ ーテスト
```

### COBOL 文のパース / COBOL Statement Parsing

```cobol
       MOVE テストー TO DEST           → 識別子: テストー
       ADD 1 TO カウンター              → 識別子: カウンター
       IF データー項目 = SPACES         → 識別子: データー項目
       COMPUTE 結果ー = 10 * 20         → 識別子: 結果ー
```

### REPLACING ルールの動作 / REPLACING Rule Behavior

```cobol
入力 / Input:    01 テストー-FIELD PIC X.
ルール / Rule:   テストー → データー
結果 / Output:   01 データー-FIELD PIC X.

入力 / Input:    MOVE データー項目 TO DEST.
ルール / Rule:   データー項目 → テストー項目
結果 / Output:   MOVE テストー項目 TO DEST.
```

## 対象ファイル / Affected Files

### server/src/server.ts
- `validateDocument()` 関数の変数抽出ロジック
- 日本語識別子を含む未使用変数・未定義変数の検出

### server/src/resolver/copybookResolver.ts
- `applyReplacingRules()` メソッド
- COPY REPLACING 句での日本語識別子の置換

## 互換性 / Compatibility

### サポートされる文字 / Supported Characters

- ASCII 文字 (a-z, A-Z, 0-9, _)
- ハイフン (-)
- 日本語文字 (ひらがな、カタカナ、漢字)
- 全角記号 (ー、ッ、etc.)
- その他の Unicode 文字 (\u0080-\uFFFF)

### COBOL 識別子の例 / COBOL Identifier Examples

```cobol
TEST-DATA          ✓
データ項目          ✓
テストー            ✓
カウンター値        ✓
結果ー計算          ✓
TEST-データー       ✓
```

## 注意事項 / Notes

1. **単語境界の制限 / Word Boundary Limitations:**
   - JavaScript/TypeScript の `\b` は Unicode 文字を正しく扱えない
   - Unicode 文字を含む識別子には `\b` を使用しない

2. **パフォーマンス / Performance:**
   - Lookahead/lookbehind を使用するため、大きなファイルでは若干遅くなる可能性
   - ただし、COBOL ファイルのサイズでは実用上問題なし

3. **エッジケース / Edge Cases:**
   - 識別子の前後にスペース、ハイフン、ピリオドがある場合のみマッチ
   - これは COBOL の構文に適合

## 関連コミット / Related Commits

- Initial: Support for Japanese characters with `[\w\u0080-\uFFFF\-]+` pattern
- This fix: Remove `\b` usage for proper Unicode support

## 参考資料 / References

- Unicode character U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK
- Shift-JIS encoding: 817C
- JavaScript RegExp limitations with Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Character_Classes
