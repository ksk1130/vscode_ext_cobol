@echo off
REM ================================================================
REM COBOL LSP Extension - Build and VSIX Package Creation Script
REM ================================================================
REM This script performs the following steps:
REM 1. Install npm dependencies
REM 2. Compile client and server TypeScript code
REM 3. Create VSIX package for distribution
REM ================================================================

echo ========================================
echo COBOL LSP Extension - Build and Package
echo ========================================
echo.

REM Step 1: Install dependencies
echo [1/3] Installing dependencies...
call npm install
if errorlevel 1 (
    echo ERROR: Failed to install dependencies
    exit /b 1
)
echo Dependencies installed successfully.
echo.

REM Step 2: Compile client and server
echo [2/3] Compiling client and server...
call npm run compile
if errorlevel 1 (
    echo ERROR: Failed to compile
    exit /b 1
)
echo Compilation completed successfully.
echo.

REM Step 3: Create VSIX package
echo [3/3] Creating VSIX package...
call npx vsce package --allow-missing-repository
if errorlevel 1 (
    echo ERROR: Failed to create VSIX package
    exit /b 1
)
echo.

echo ========================================
echo Build and packaging completed successfully!
echo ========================================
echo.
echo The VSIX file has been created in the current directory.
echo You can install it using:
echo   - VS Code Command Palette: "Extensions: Install from VSIX..."
echo   - Or CLI: code --install-extension cobol-lsp-0.1.0.vsix
echo.

exit /b 0
