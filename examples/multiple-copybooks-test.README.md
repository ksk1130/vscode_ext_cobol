# Multiple COPYBOOKs with Duplicate Variable Names - Test

## 概要 (Overview)

This test verifies the COPYBOOK variable tracking functionality when multiple COPYBOOKs contain variables with the same name.

このテストは、複数のCOPYBOOKに同じ変数名が含まれている場合のCOPYBOOK変数追跡機能を検証します。

## テストファイル (Test Files)

### COBOLプログラム
- `multiple-copybooks-test.cbl`: メインのテストプログラム

### COPYBOOKファイル
- `copybooks/CUSTOMER-DATA.cpy`: 顧客データ用COPYBOOK
- `copybooks/PRODUCT-DATA.cpy`: 製品データ用COPYBOOK

## 重複する変数名 (Duplicate Variable Names)

両方のCOPYBOOKには以下の同名の変数が定義されています：

Both COPYBOOKs define the following variables with identical names:
- `RECORD-ID` (PIC 9(8))
- `RECORD-NAME` (PIC X(50))

## テストケース (Test Cases)

### 1. ホバー表示のテスト (Hover Display Test)

**手順 (Steps):**
1. VS Codeで `multiple-copybooks-test.cbl` を開く
2. 行22の `RECORD-ID` にカーソルを置く
3. ホバー情報を確認

**期待される結果 (Expected Result):**
- COPYBOOK名 "CUSTOMER-DATA.cpy" が表示される
- レベル番号、PIC句、定義行が表示される

### 2. 定義へのジャンプのテスト (Go to Definition Test)

**手順 (Steps):**
1. 行22の `RECORD-ID OF CUSTOMER-RECORD` でF12を押す
2. ジャンプ先を確認
3. 行26の `RECORD-ID OF PRODUCT-RECORD` でF12を押す
4. ジャンプ先を確認

**期待される結果 (Expected Result):**
1. `CUSTOMER-DATA.cpy` の3行目にジャンプ
2. `PRODUCT-DATA.cpy` の3行目にジャンプ

### 3. 自動補完のテスト (Auto-completion Test)

**手順 (Steps):**
1. PROCEDURE DIVISION内で新しい行を追加
2. "MOVE 100 TO " と入力
3. Ctrl+Space で補完候補を表示
4. "RECORD" と入力してフィルタ

**期待される結果 (Expected Result):**
- `RECORD-ID` と `RECORD-NAME` が表示される
- それぞれのCOPYBOOK名が詳細情報に含まれる
  - 例: "Level 05 (CUSTOMER-DATA.cpy) PIC 9(8)"
  - 例: "Level 05 (PRODUCT-DATA.cpy) PIC X(50)"

### 4. ログ出力の確認 (Log Output Verification)

**手順 (Steps):**
1. VS Codeの出力パネルを開く
2. "COBOL Language Server" チャンネルを選択
3. ログを確認

**期待されるログエントリ (Expected Log Entries):**
```
[Hover] Found 2 symbols with name "RECORD-ID"
[Hover]   1. CUSTOMER-DATA.cpy (line 3)
[Hover]   2. PRODUCT-DATA.cpy (line 3)
```

## 実装の確認ポイント (Implementation Verification Points)

### SymbolIndex
- ✅ `copybookReferences` マップが正しく管理されている
- ✅ `registerCopybookReference()` が呼び出されている
- ✅ `findSymbolsWithCopybookContext()` が複数のシンボルを返す

### Server
- ✅ `loadCopybooksFromDocument()` がCOPYBOOK参照を登録
- ✅ ホバー表示がCOPYBOOK名を含む
- ✅ 定義ジャンプが正しいCOPYBOOKを開く
- ✅ 補完候補にCOPYBOOK名が含まれる

## トラブルシューティング (Troubleshooting)

### COPYBOOK名が表示されない場合
1. COPYBOOKディレクトリが `settings.json` に設定されているか確認
2. COPYBOOKファイルの拡張子が `.cpy` であるか確認
3. LSPサーバーを再起動してみる

### 定義ジャンプが機能しない場合
1. COPYBOOK検索パスが正しく設定されているか確認
2. 出力パネルでエラーログを確認
3. ドキュメントを再読み込みしてみる

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

## 参照 (References)

詳細な実装情報は以下を参照してください：
- `COPYBOOK_VARIABLE_TRACKING.md`: 実装の詳細
- `server/src/index/symbolIndex.ts`: SymbolIndexクラス
- `server/src/server.ts`: LSPサーバー実装
