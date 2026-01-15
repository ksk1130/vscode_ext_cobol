#!/bin/bash
# ================================================================
# COBOL LSP Extension - Build and VSIX Package Creation Script
# ================================================================
# This script performs the following steps:
# 1. Install npm dependencies
# 2. Compile client and server TypeScript code
# 3. Create VSIX package for distribution
# ================================================================
# Usage: ./build-vsix.sh [version]
# Example: ./build-vsix.sh 0.2.0
# ================================================================

set -e  # Exit on error

# Check for version argument
if [ -n "$1" ]; then
    VERSION="$1"
    echo "Version specified: $VERSION"
else
    echo "Usage: ./build-vsix.sh [version]"
    echo "Example: ./build-vsix.sh 0.2.0"
    echo ""
    echo "If no version argument is provided, the existing version in package.json will be used."
    VERSION=""
fi

echo "========================================"
echo "COBOL LSP Extension - Build and Package"
echo "========================================"
echo ""

# Update package.json if version is specified
if [ -n "$VERSION" ]; then
    echo "[0/4] Updating version to: $VERSION"
    
    # Use Node.js to update package.json
    node -e "
    const fs = require('fs');
    const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8'));
    pkg.version = '$VERSION';
    fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');
    "
    
    if [ $? -ne 0 ]; then
        echo "Error: Failed to update package.json"
        exit 1
    fi
    echo "Updated package.json successfully"
    echo ""
fi

# Step 1: Install dependencies
echo "[1/4] Installing dependencies..."
npm install
echo "Dependencies installed successfully."
echo ""

# Step 2: Compile client and server
echo "[2/4] Compiling client and server..."
npm run compile
echo "Compilation completed successfully."
echo ""

# Step 3: Create VSIX package
echo "[3/4] Creating VSIX package..."
echo "  - VS Code Command Palette: \"Extensions: Install from VSIX...\""
echo "  - Or CLI: code --install-extension <package-name>-<version>.vsix"
echo ""

exit 0
