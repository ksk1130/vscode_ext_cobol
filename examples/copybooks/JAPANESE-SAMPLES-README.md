# 日本語COPYBOOK サンプル / Japanese COPYBOOK Samples

## 概要 (Overview)

このディレクトリには、日本語変数名と長音記号「ー」を使用したCOPYBOOKのサンプルが含まれています。

This directory contains sample COPYBOOKs that demonstrate the use of Japanese variable names and the prolonged sound mark "ー" (katakana-hiragana prolonged sound mark, U+30FC).

## サンプルファイル (Sample Files)

### COPYBOOKファイル

1. **社員マスター.cpy** (Employee Master)
   - 社員情報を管理するCOPYBOOK
   - 「社員ーコード」「部署ーコード」など、「ー」を含む変数名を使用
   - 日本語の条件名（88レベル）を含む

2. **顧客データー.cpy** (Customer Data)
   - 顧客情報を管理するCOPYBOOK
   - 「顧客ーID」「姓ーカナ」「電話番号ーケータイ」など複数の「ー」使用例
   - 階層構造を持つ日本語変数名

3. **製品マスター.cpy** (Product Master)
   - 製品情報を管理するCOPYBOOK
   - 「製品ーコード」「長さーセンチ」「重量ーグラム」など多様な「ー」使用
   - カタカナと「ー」の組み合わせ（「サイズー情報」「メーカーコード」）

### テストプログラム

**japanese-copybook-sample.cbl**
- 上記3つのCOPYBOOKを使用するサンプルプログラム
- 各COPYBOOKの変数に値を設定して表示

## 特徴 (Features)

### 1. 長音記号「ー」の使用
すべてのCOPYBOOKで、変数名に長音記号「ー」（U+30FC）を使用しています：
- `社員ーコード` (Employee Code)
- `顧客ーID` (Customer ID)
- `製品ー情報` (Product Info)

### 2. 日本語変数名
漢字、ひらがな、カタカナを含む変数名：
- `社員情報` (Employee Information)
- `メールアドレス` (Email Address)
- `カテゴリーコード` (Category Code)

### 3. 条件名（88レベル）
日本語の条件名を定義：
- `優秀ーランク` (Excellent Rank)
- `在籍中` (Currently Employed)
- `プレミアム会員` (Premium Member)

### 4. 階層構造
複雑な階層構造を持つデータ定義：
```cobol
05  給与ー情報.
    10  基本給ー金額           PIC 9(9)V99.
    10  手当ー金額             PIC 9(7)V99.
    10  控除ー金額             PIC 9(7)V99.
```

## テスト方法 (How to Test)

### 1. ホバー表示のテスト
`japanese-copybook-sample.cbl` を開き、変数名にカーソルを置く：
- `社員ーコード` → "Defined in: 社員マスター.cpy (COPYBOOK)" が表示される
- `顧客ーID` → "Defined in: 顧客データー.cpy (COPYBOOK)" が表示される
- `製品ーコード` → "Defined in: 製品マスター.cpy (COPYBOOK)" が表示される

### 2. 定義へのジャンプ
変数名の上で F12 キーを押す：
- 対応するCOPYBOOKファイルが開き、定義行にジャンプする

### 3. 自動補完
PROCEDURE DIVISION内で Ctrl+Space を押す：
- すべてのCOPYBOOK由来の変数が補完候補として表示される
- 詳細情報にCOPYBOOK名が含まれる

### 4. ログ確認
VS Codeの出力パネル（"COBOL Language Server"）でログを確認：
```
[SymbolIndex] Indexed 社員マスター.cpy: 18 total symbols (16 variables, 0 paragraphs, 0 sections)
[SymbolIndex] Registered COPYBOOK "社員マスター" for japanese-copybook-sample.cbl (line 17): 18 symbols (16 variables)
[SymbolIndex] COPYBOOK Table for japanese-copybook-sample.cbl: 3 COPYBOOK(s) registered
  1. "社員マスター" (line 17): 18 symbols (16 variables)
  2. "顧客データー" (line 20): 20 symbols (18 variables)
  3. "製品マスター" (line 23): 22 symbols (20 variables)
```

## Unicode文字情報 (Unicode Information)

### 長音記号「ー」
- **文字**: ー
- **Unicode**: U+30FC
- **名称**: KATAKANA-HIRAGANA PROLONGED SOUND MARK
- **Shift-JIS**: 0x817C
- **用途**: カタカナの長音を表す（例: コード → コーード、データ → データー）

この文字は日本語のプログラミングで変数名に使用でき、本実装では正しく認識・処理されます。

## 設定例 (Configuration Example)

`.vscode/settings.json`:
```json
{
  "cobol.copybookPaths": [
    "./examples/copybooks"
  ],
  "cobol.copybookExtensions": [
    ".cpy",
    ".CPY"
  ]
}
```

## 注意事項 (Notes)

1. ファイルのエンコーディングはUTF-8を推奨
2. 一部のシステムではShift-JISエンコーディングが必要な場合があります
3. 「ー」は正規表現で `\u30FC` または `ー` として扱われます
4. ハイフン `-` (U+002D) と長音記号 `ー` (U+30FC) は異なる文字です

## 関連ドキュメント (Related Documentation)

- [UNICODE_SUPPORT.md](../../UNICODE_SUPPORT.md) - Unicode文字サポートの詳細
- [COPYBOOK_VARIABLE_TRACKING.md](../../COPYBOOK_VARIABLE_TRACKING.md) - COPYBOOK変数追跡機能の説明
- [README.md](../../README.md) - メインドキュメント
