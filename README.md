# COBOL Language Server Extension

COBOL 向けの VS Code 拡張機能です。COBOL ソース (.cbl/.cob/.cobol/.cpy) に対して定義ジャンプ、ホバー、診断などの機能を提供します。

## 機能
- 変数・COPYBOOK・段落/節 (PERFORM)・プログラム呼び出し (CALL) への定義ジャンプ
- ホバー表示（レベル、PIC、行/桁、定義ファイル名を表示。COPYBOOK 由来も表示）
- COPYBOOK の REPLACING,DISJOINING/JOINING による接頭辞置換をサポート
- COPYBOOK 検索パス（ワークスペース内の `copybooks/`、`copy/`、環境変数 `COBOL_COPYPATH`）
- TextMate ベースのシンタックスハイライト
- 診断: 未定義への代入警告、未使用シンボルのヒント（グループ項目に配慮）
- アウトライン表示: VS Code のアウトラインビューに変数・段落・セクションを階層表示

## プロジェクト構成
- client/ — VS Code 拡張本体（language client を起動）
- server/ — Language Server (LSP) 実装とシンボルインデックス
- client/syntaxes/ — COBOL 用 TextMate grammar
- server/src/resolver/ — COPYBOOK / PROGRAM のリゾルバ
- server/src/index/ — シンボル抽出とインデックス

## セットアップ
1. 依存関係インストール
```
npm install
```
2. クライアントとサーバーのビルド
```
npm run compile
```
3. VS Code で F5 を押して Extension Development Host を起動

## 配布用パッケージの作成とインストール

### 簡単な方法（推奨）
ビルド＋VSIX作成を一度に行うバッチファイルを用意しています。

**Windows:**
```
build-vsix.bat
```

**Linux/Mac:**
```
./build-vsix.sh
```

このスクリプトは以下を自動実行します:
1. 依存関係のインストール (`npm install`)
2. クライアントとサーバーのコンパイル (`npm run compile`)
3. VSIX パッケージの作成 (`vsce package`)

実行後、ルートディレクトリに `cobol-lsp-0.1.0.vsix` が生成されます。

### 手動での作成方法
1. vsce のインストール（未導入なら）
```
npm install -g @vscode/vsce
```
2. 依存関係のインストールとビルド
```
npm install
npm run compile
```
3. ルートでパッケージを作成（.vsix が生成されます）
```
vsce package --allow-missing-repository
```

### インストール方法
- VS Code のコマンドパレット → "Extensions: Install from VSIX..." で .vsix を選択
- または CLI から:
```
code --install-extension cobol-lsp-0.1.0.vsix
```

### アップデート時
再度ビルドスクリプトを実行するか `vsce package` で新しい .vsix を生成し、同様にインストール

## 開発メモ
- LSP サーバーのエントリ: server/src/server.ts
- パース前にシーケンス領域（1-7 桁）を除去
- .cpy はデフォルトで DATA DIVISION として扱う
- COPYBOOK は REPLACING / DISJOINING / JOINING を適用した上でインデックス化

## 動作確認のチェックリスト
- COBOL ファイルを開き、Ctrl+Click で以下を確認:
  - COPY 文 → 対応する COPYBOOK が開く
  - PERFORM → 段落/節へジャンプ
  - CALL → ワークスペース内の PROGRAM-ID へジャンプ
  - COPYBOOK 定義の変数（REPLACING / DISJOINING / JOINING を含む）が解決される
- 変数にホバーしてレベル/PIC/行/ファイル名が表示される
- Problems パネルに未定義代入や未使用シンボルが出ることを確認
- アウトラインビュー (Ctrl+Shift+O または View → Outline) で変数・段落・セクションの階層構造が表示される

## スクリプト
- `npm run compile` — クライアントとサーバーをビルド
- `build-vsix.bat` (Windows) / `./build-vsix.sh` (Linux/Mac) — ビルド＋VSIX作成を一括実行

## 依存関係
- クライアント: vscode-languageclient (^9.0.1)
- サーバー: vscode-languageserver (^9.0.1), vscode-languageserver-textdocument (^1.0.11), vscode-uri (^3.0.0)
- 共通開発: typescript (^5.0.0), @types/node (^18.0.0), @types/vscode (^1.75.0, client)
- 配布用: @vscode/vsce (^2.19.0, package 用)

## 設定
- COPYBOOK 検索パス: `copybooks/`、`copy/`、環境変数 `COBOL_COPYPATH`
- 対応拡張子: .cpy, .cbl, .cob, .cobol（大文字小文字を区別しない）
