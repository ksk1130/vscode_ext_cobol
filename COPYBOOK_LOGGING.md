# COPYBOOK Scanner Logging Feature / COPYBOOKスキャナーログ機能

## 概要 / Overview

### 日本語

COPYBOOKリゾルバーの動作状況を確認できるように、COPYBOOK解決や読み込みの際にログを出力します。
起動時の自動スキャンは現行の構成では実行していません。

### English

Logs are emitted during COPYBOOK resolution/loading to help verify resolver behavior.
Automatic startup scans are not executed in the current build.

## 機能 / Features

### いつログが出るか / When Logs Are Emitted

COPYBOOK解決やロードが実行されるタイミングで、以下の情報が出力されます：

Logs are emitted when COPYBOOK resolution/loading runs:

1. **検索パス / Search Paths**: 使用される COPYBOOK 検索パス
2. **拡張子 / Extensions**: 検索対象の拡張子リスト
3. **対象のCOPYBOOK / Target COPYBOOK**: 解決された COPYBOOK 名と URI

### ログ出力例 / Log Output Example

```
[loadCopybooksFromDocument] About to index COPYBOOK: CUSTOMER-DATA
[loadCopybooksFromDocument] COPYBOOK URI: file:///workspace/copybooks/CUSTOMER-DATA.cpy
[loadCopybooksFromDocument] Content length: 128 chars
[loadCopybooksFromDocument] Indexed 3 symbols from CUSTOMER-DATA
```

### エラーが発生した場合 / When Error Occurs

現行実装では COPYBOOK ロード時のエラーはログに出さず、静かにスキップします。

In the current implementation, COPYBOOK load errors are silently skipped without logging.

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
  - `logCallback` を通じて解決処理のログを出力

2. **server.ts**:
  - COPYBOOK解決/ロード処理でログを出力

### 変更ファイル / Changed Files

- `server/src/resolver/copybookResolver.ts`: 解析ロジックとログ出力
- `server/src/server.ts`: COPYBOOKロード時のログ

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
  "cobol.copybookPaths": ["./copybooks"],
  "cobol.copybookExtensions": [".cpy", ".CPY", ".cbl", ".CBL", ""],
  "cobol.trace.server": "verbose"
}
```
