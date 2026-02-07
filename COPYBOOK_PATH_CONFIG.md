# COPYBOOK Path Configuration / COPYBOOK パス設定

## 概要 / Overview

### 日本語

VS Code の設定ファイル（`settings.json`）で指定した `cobol.copybookPaths` と `cobol.copybookExtensions` を正しく尊重するように改善しました。

以前は、ワークスペースルートからの相対パス（`<workspace>/copybooks`, `<workspace>/copy`）が固定で使用されていましたが、現在は設定ファイルで指定されたパスを優先して使用します。

### English

Improved to properly respect `cobol.copybookPaths` and `cobol.copybookExtensions` specified in VS Code settings (`settings.json`).

Previously, relative paths from workspace root (`<workspace>/copybooks`, `<workspace>/copy`) were hardcoded, but now the paths specified in the configuration file are prioritized.

## 設定方法 / Configuration

### 絶対パスの指定 / Absolute Paths

絶対パスを指定すると、そのパスがそのまま使用されます：

Absolute paths are used as-is:

```json
{
  "cobol.copybookPaths": [
    "/absolute/path/to/copybooks",
    "C:\\absolute\\path\\on\\windows"
  ]
}
```

### 相対パスの指定 / Relative Paths

相対パスを指定すると、ワークスペースルートを基準に解決されます：

Relative paths are resolved relative to the workspace root:

```json
{
  "cobol.copybookPaths": [
    "./copybooks",      // <workspace>/copybooks
    "./copy",           // <workspace>/copy
    "../shared/copy"    // <workspace>/../shared/copy
  ]
}
```

### 拡張子の設定 / Extension Configuration

COPYBOOK ファイルの拡張子も設定できます：

COPYBOOK file extensions can also be configured:

```json
{
  "cobol.copybookExtensions": [
    ".cpy",
    ".CPY",
    ".cbl",
    ".CBL",
    ""        // 拡張子なしも許可 / Allow files without extension
  ]
}
```

## デフォルト設定 / Default Settings

設定が未指定の場合、以下のデフォルト値が使用されます：

If not configured, the following defaults are used:

```json
{
  "cobol.copybookPaths": ["./copybooks", "./copy", "./COPY"],
  "cobol.copybookExtensions": [".cpy", ".CPY", ".cbl", ".CBL", ""]
}
```

## 解決の優先順位 / Resolution Priority

### 日本語

同名COPYBOOKが複数パスに存在する場合、以下の順で**最初に見つかったもの**が採用されます：

1. ソースファイルと同じディレクトリ
2. `cobol.copybookPaths` に指定した順（環境変数 `COBOL_COPYPATH` は最後に追加）
3. `cobol.copybookExtensions` に指定した拡張子順

また、一度解決されたCOPYBOOK名はキャッシュされ、同名の別パスが後から追加されても再解決は行われません。

### English

When multiple COPYBOOKs share the same name, the **first match** is selected in the following order:

1. The same directory as the source file
2. The order of `cobol.copybookPaths` (the `COBOL_COPYPATH` environment variable is appended last)
3. The order of extensions in `cobol.copybookExtensions`

Once a COPYBOOK name is resolved, it is cached and will not be re-resolved even if another path is added later.

## 動的な設定変更 / Dynamic Configuration Changes

設定を変更すると、自動的に COPYBOOK リゾルバーが再初期化され、新しいパスが使用されます：

When settings are changed, the COPYBOOK resolver is automatically reinitialized with the new paths:

1. VS Code の設定を変更
2. 自動的に設定が再読み込みされる
3. COPYBOOK ファイルが再スキャンされる
4. ログに新しい設定が出力される

## ログ出力 / Log Output

設定が読み込まれると、以下のようなログが出力されます：

When configuration is loaded, the following logs are output:

```
[Config] copybookPaths from settings: ["./copybooks","./copy"]
[Config] copybookExtensions from settings: [".cpy",".CPY",".cbl",".CBL",""]
[Config] Resolved copybook paths: ["/workspace/copybooks","/workspace/copy"]
[CopybookResolver] Scanning COPYBOOK files...
[CopybookResolver] Search paths: /workspace/copybooks, /workspace/copy
[CopybookResolver] Extensions: .cpy, .CPY, .cbl, .CBL, 
[CopybookResolver]   Found 3 file(s) in /workspace/copybooks:
[CopybookResolver]     - CUSTOMER.cpy
[CopybookResolver]     - PRODUCT.CPY
[CopybookResolver]     - ORDER.cpy
[CopybookResolver] Total COPYBOOK files found: 3
```

設定変更時：

When configuration changes:

```
[Config Changed] copybookPaths: ["/absolute/path/to/copybooks"]
[Config Changed] copybookExtensions: [".cpy"]
[Config Changed] Resolved paths: ["/absolute/path/to/copybooks"]
[CopybookResolver] Scanning COPYBOOK files...
...
```

## 環境変数 / Environment Variables

`COBOL_COPYPATH` 環境変数も引き続きサポートされます：

The `COBOL_COPYPATH` environment variable is still supported:

```bash
export COBOL_COPYPATH=/system/cobol/copybooks
```

この環境変数で指定されたパスは、設定ファイルのパスに追加されます。

Paths specified by this environment variable are added to the configured paths.

## 技術詳細 / Technical Details

### 実装内容 / Implementation

1. **server.ts**:
   - `connection.onInitialize` で `connection.workspace.getConfiguration()` を使用して設定を取得
   - 相対パスを絶対パスに変換
   - `connection.onDidChangeConfiguration` で設定変更を監視

2. **copybookResolver.ts**:
   - `getConfig()` メソッドを追加して設定を公開

3. **getCopybookCompletions()**:
   - ハードコードされたパスを削除
   - `copybookResolver.getConfig()` から設定を取得

### 変更されたファイル / Changed Files

- `server/src/server.ts`: 設定の読み込みと動的変更への対応
- `server/src/resolver/copybookResolver.ts`: 設定公開メソッドの追加

## 使用例 / Use Cases

### ケース1: 複数のプロジェクトで共有するCOPYBOOK / Shared COPYBOOKs

```json
{
  "cobol.copybookPaths": [
    "./copybooks",                    // プロジェクト固有
    "../shared-copybooks",            // チーム共有
    "/company/standard/copybooks"     // 会社標準
  ]
}
```

### ケース2: 環境ごとの設定 / Per-environment Configuration

開発環境：
Development:

```json
{
  "cobol.copybookPaths": [
    "./dev/copybooks"
  ]
}
```

本番環境：
Production:

```json
{
  "cobol.copybookPaths": [
    "/prod/cobol/copybooks"
  ]
}
```

## トラブルシューティング / Troubleshooting

### COPYBOOK が見つからない場合

1. ログを確認して設定が正しく読み込まれているか確認
2. パスが正しいか確認（絶対パス/相対パス）
3. 拡張子が設定に含まれているか確認
4. ファイルのパーミッションを確認

ログの確認方法：
View logs:
- VS Code: `表示` > `出力` > `COBOL Language Server`
