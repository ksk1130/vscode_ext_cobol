# Test File for 88-Level Symbol Fix

## Purpose
This test file verifies that 88-level condition names appear only **once** in the VS Code outline view, not duplicated.

## Background
Prior to this fix, 88-level condition names (like `FILE-OK`, `FILE-EOF`, etc.) were appearing **twice** in the outline view due to being matched by two different regex patterns in the symbol extraction logic.

## Test Cases
This file includes multiple 88-level condition names across different parent variables:

### 1. File Status Flags (WS-FILE-STATUS)
- FILE-OK (value '00')
- FILE-EOF (value '10')
- FILE-ERROR (value '99')

### 2. Customer Status Flags (WS-CUSTOMER-STATUS)
- STATUS-ACTIVE (value 'A')
- STATUS-INACTIVE (value 'I')
- STATUS-SUSPENDED (value 'S')

### 3. Processing Flags (WS-PROCESSING-FLAG)
- PROCESSING-COMPLETE (value 'Y')
- PROCESSING-PENDING (value 'N')

## Expected Behavior
When viewing this file in VS Code with the COBOL LSP extension:

1. Open the Outline view (Ctrl+Shift+O or View → Outline)
2. Expand the DATA DIVISION → WORKING-STORAGE SECTION
3. Each 88-level condition name should appear **exactly once** under its parent variable
4. No duplicate entries should be visible

## Technical Details
The fix was implemented in `/server/src/index/symbolIndex.ts` by:
- Adding a check to exclude level 88 from the generic variable pattern
- Ensuring 88-level items are only processed by the specific 88-level pattern
- This prevents double-extraction of condition names

## Verification
Run the test script to verify:
```bash
npm run compile
node /tmp/test-symbols.js
```

Expected output:
```
✅ No duplicates found - all symbols appear exactly once!
```
