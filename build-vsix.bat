@echo off
REM ================================================================
REM COBOL LSP拡張機能 - ビルド＋VSIXパッケージ作成スクリプト
REM ================================================================
REM このスクリプトは以下の処理を実行します:
REM 1. npm依存関係のインストール
REM 2. クライアントとサーバーのTypeScriptコードをコンパイル
REM 3. 配布用VSIXパッケージの作成
REM ================================================================

echo ========================================
echo COBOL LSP拡張機能 - ビルドとパッケージ化
echo ========================================
echo.

REM ステップ1: 依存関係のインストール
echo [1/3] 依存関係をインストール中...
call npm install
if errorlevel 1 (
    echo エラー: 依存関係のインストールに失敗しました
    exit /b 1
)
echo 依存関係のインストールが完了しました。
echo.

REM ステップ2: クライアントとサーバーのコンパイル
echo [2/3] クライアントとサーバーをコンパイル中...
call npm run compile
if errorlevel 1 (
    echo エラー: コンパイルに失敗しました
    exit /b 1
)
echo コンパイルが完了しました。
echo.

REM ステップ3: VSIXパッケージの作成
echo [3/3] VSIXパッケージを作成中...
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
