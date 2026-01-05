# COPYBOOK Scanner Logging Feature / COPYBOOKスキャナーログ機能

## 概要 / Overview

### 日本語

COPYBOOKリゾルバーの動作状況を確認できるように、設定されたパス配下で見つかったCOPYBOOKファイルをログ出力する機能を追加しました。

### English

Added a logging feature to help understand the operation status of the COPYBOOK resolver by outputting the COPYBOOK files found under the configured paths.

## 機能 / Features

### 起動時のスキャン / Startup Scan

VS Code 拡張機能の起動時（ワークスペースを開いた時）に、以下の情報がログに出力されます：

When the VS Code extension starts (when opening a workspace), the following information is logged:

1. **検索パス / Search Paths**: 設定された COPYBOOK 検索パス
2. **拡張子 / Extensions**: 検索対象の拡張子リスト
3. **見つかったファイル / Found Files**: 各パス配下で見つかった COPYBOOK ファイルの一覧
4. **合計 / Total**: 見つかった COPYBOOK ファイルの総数

### ログ出力例 / Log Output Example

```
[CopybookResolver] Scanning COPYBOOK files...
[CopybookResolver] Search paths: /workspace/copybooks, /workspace/copy
[CopybookResolver] Extensions: .cpy, .CPY, .cbl, .CBL, 
[CopybookResolver]   Found 3 file(s) in /workspace/copybooks:
[CopybookResolver]     - CUSTOMER.cpy
[CopybookResolver]     - PRODUCT.CPY
[CopybookResolver]     - ORDER.cpy
[CopybookResolver]   Found 1 file(s) in /workspace/copy:
[CopybookResolver]     - COMMON.cbl
[CopybookResolver] Total COPYBOOK files found: 4
```

### パスが見つからない場合 / When Path Not Found

```
[CopybookResolver]   Path not found: /workspace/missing-dir
```

### エラーが発生した場合 / When Error Occurs

```
[CopybookResolver]   Error scanning /workspace/copybooks: Permission denied
```

## ログの確認方法 / How to View Logs

### VS Code での確認

1. **出力パネルを開く / Open Output Panel**: `表示` > `出力` (または `View` > `Output`)
2. **チャンネルを選択 / Select Channel**: ドロップダウンから `COBOL Language Server` を選択
3. **ログを確認 / View Logs**: 拡張機能起動時のログが表示されます

### トレースレベルの設定 / Trace Level Configuration

ログの詳細度は設定で変更できます：

The verbosity of logs can be configured:

```json
{
  "cobol.trace.server": "verbose"  // "off", "messages", or "verbose"
}
```

## 技術詳細 / Technical Details

### 実装内容 / Implementation

1. **CopybookResolver クラス**:
   - `logCallback` パラメータを追加（オプショナル）
   - `scanAndLogCopybookFiles()` メソッドを追加

2. **server.ts**:
   - CopybookResolver 初期化時にログコールバックを渡す
   - 初期化完了後に `scanAndLogCopybookFiles()` を呼び出す

### ファイル検出ロジック / File Detection Logic

- ディレクトリは除外 / Directories are excluded
- 設定された拡張子のファイルのみを検出 / Only files with configured extensions are detected
- エラーハンドリング付き / With error handling

### 変更ファイル / Changed Files

- `server/src/resolver/copybookResolver.ts`: スキャン機能の追加
- `server/src/server.ts`: ログコールバックの設定とスキャン実行

## 使用例 / Use Cases

### デバッグ / Debugging

- COPYBOOK が見つからない場合の原因調査
- Investigation when COPYBOOK files are not found

### 設定確認 / Configuration Verification

- 検索パスが正しく設定されているか確認
- Verify that search paths are correctly configured

### プロジェクト構成の把握 / Understanding Project Structure

- ワークスペース内の COPYBOOK ファイルの全体像を把握
- Get an overview of COPYBOOK files in the workspace

## 関連設定 / Related Configuration

```json
{
  "cobol.copybookPaths": ["./copybooks", "./copy", "./COPY"],
  "cobol.copybookExtensions": [".cpy", ".CPY", ".cbl", ".CBL", ""],
  "cobol.trace.server": "verbose"
}
```
