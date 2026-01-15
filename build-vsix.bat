@echo off
REM ================================================================
REM COBOL LSP拡張機能 - ビルド＋VSIXパッケージ作成スクリプト
REM ================================================================
REM このスクリプトは以下の処理を実行します:
REM 1. npm依存関係のインストール
REM 2. クライアントとサーバーのTypeScriptコードをコンパイル
REM 3. 配布用VSIXパッケージの作成
REM ================================================================
REM 使用方法: build-vsix.bat [version]
REM 例: build-vsix.bat 0.2.0
REM ================================================================

setlocal enabledelayedexpansion

REM バージョン指定の確認
if not "%~1"=="" (
    set VERSION=%~1
    echo バージョン指定: !VERSION!
) else (
    echo 使用方法: build-vsix.bat [version]
    echo 例: build-vsix.bat 0.2.0
    echo.
    echo バージョン引数がない場合、package.jsonの既存バージョンを使用します
    set VERSION=
)

echo ========================================
echo COBOL LSP拡張機能 - ビルドとパッケージ化
echo ========================================
echo.

REM バージョン指定がある場合、package.jsonを更新
if not "!VERSION!"=="" (
    echo [0/4] バージョンを更新中: !VERSION!
    node -e "const fs = require('fs'); const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8')); pkg.version = '!VERSION!'; fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');"
    if errorlevel 1 (
        echo エラー: package.jsonの更新に失敗しました
        exit /b 1
    )
    echo package.jsonを更新しました
    echo.
)

REM ステップ1: 依存関係のインストール
echo [1/4] 依存関係をインストール中...
call npm install
if errorlevel 1 (
    echo エラー: 依存関係のインストールに失敗しました
    exit /b 1
)
echo 依存関係のインストールが完了しました。
echo.

REM ステップ2: クライアントとサーバーのコンパイル
echo [2/4] クライアントとサーバーをコンパイル中...
call npm run compile
if errorlevel 1 (
    echo エラー: コンパイルに失敗しました
    exit /b 1
)
echo コンパイルが完了しました。
echo.

REM ステップ3: VSIXパッケージの作成
echo [3/4] VSIXパッケージを作成中...
call npx vsce package --allow-missing-repository
if errorlevel 1 (
    echo エラー: VSIXパッケージの作成に失敗しました
    exit /b 1
)
echo.

echo ========================================
echo ビルドとパッケージ化が完了しました！
echo ========================================
echo.
echo VSIXファイルが現在のディレクトリに作成されました。
echo 次の方法でインストールできます:
echo   - VS Codeのコマンドパレット: "Extensions: Install from VSIX..."
echo   - またはCLIから: code --install-extension <package-name>-<version>.vsix
echo.

exit /b 0
