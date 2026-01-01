#!/bin/bash
# ================================================================
# COBOL LSP Extension - Build and VSIX Package Creation Script
# ================================================================
# This script performs the following steps:
# 1. Install npm dependencies
# 2. Compile client and server TypeScript code
# 3. Create VSIX package for distribution
# ================================================================

set -e  # Exit on error

echo "========================================"
echo "COBOL LSP Extension - Build and Package"
echo "========================================"
echo ""

# Step 1: Install dependencies
echo "[1/3] Installing dependencies..."
npm install
echo "Dependencies installed successfully."
echo ""

# Step 2: Compile client and server
echo "[2/3] Compiling client and server..."
npm run compile
echo "Compilation completed successfully."
echo ""

# Step 3: Create VSIX package
echo "[3/3] Creating VSIX package..."
npx vsce package --allow-missing-repository
echo ""

echo "========================================"
echo "Build and packaging completed successfully!"
echo "========================================"
echo ""
echo "The VSIX file has been created in the current directory."
echo "You can install it using:"
echo "  - VS Code Command Palette: \"Extensions: Install from VSIX...\""
echo "  - Or CLI: code --install-extension <package-name>-<version>.vsix"
echo ""

exit 0
