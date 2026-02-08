import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { CopybookResolver } from '../src/resolver/copybookResolver';

describe('CopybookResolver', () => {
    it('extracts copybook names across common patterns', () => {
        const resolver = new CopybookResolver({ searchPaths: [], extensions: ['.cpy', '.cbl', ''] });

        assert.equal(resolver.extractCopybookName('COPY CUSTMAST.'), 'CUSTMAST');
        assert.equal(resolver.extractCopybookName('COPY "CUSTMAST.CPY".'), 'CUSTMAST.CPY');
        assert.equal(resolver.extractCopybookName("COPY 'CUSTMAST'."), 'CUSTMAST');
        assert.equal(resolver.extractCopybookName('COPY CUSTMAST IN LIBNAME.'), 'CUSTMAST');
        assert.equal(resolver.extractCopybookName('COPY 顧客データー.'), '顧客データー');
    });

    it('extracts DISJOINING/JOINING AS PREFIX rules', () => {
        const resolver = new CopybookResolver({ searchPaths: [], extensions: ['.cpy', '.cbl', ''] });

        const info = resolver.extractCopybookInfo('COPY CP-SMPL2 DISJOINING XXX JOINING TODAY AS PREFIX.');
        assert.equal(info.replacing.length, 1);
        assert.deepEqual(info.replacing[0], { from: 'XXX', to: 'TODAY', isPrefix: true });
    });

    it('applies REPLACING and prefix rules', () => {
        const resolver = new CopybookResolver({ searchPaths: [], extensions: ['.cpy', '.cbl', ''] });

        const replacing = resolver.applyReplacingRules('MOVE CUST TO CUST-NAME.', [
            { from: 'CUST', to: 'CUSTOMER' }
        ]);
        assert.equal(replacing, 'MOVE CUSTOMER TO CUSTOMER-NAME.');

        const prefixed = resolver.applyReplacingRules('XXX-FOO XXX-FOO.', [
            { from: 'XXX', to: 'TODAY', isPrefix: true }
        ]);
        assert.equal(prefixed, 'TODAY-FOO TODAY-FOO.');
    });

    it('resolves copybook files via search paths', () => {
        const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'cobol-copybook-'));
        const copyPath = path.join(tempDir, 'TESTCOPY.cpy');
        fs.writeFileSync(copyPath, '       01 CP-VAR PIC X.');

        try {
            const resolver = new CopybookResolver({ searchPaths: [tempDir], extensions: ['.cpy', '.CPY', ''] });
            const resolved = resolver.resolveCopybook('TESTCOPY', 'C:\\nonexistent');
            assert.equal(resolved, copyPath);
        } finally {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
    });
});
