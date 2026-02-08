import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { ProgramResolver } from '../src/resolver/programResolver';

describe('ProgramResolver', () => {
    it('extracts called program names from single-line CALL statements', () => {
        const resolver = new ProgramResolver();

        assert.equal(resolver.extractCalledProgram("CALL 'SUBPROG1'"), 'SUBPROG1');
        assert.equal(resolver.extractCalledProgram('CALL "SUBPROG2"'), 'SUBPROG2');
        assert.equal(resolver.extractCalledProgram('CALL WS-PROGRAM-NAME'), 'WS-PROGRAM-NAME');
    });

    it('extracts called program names from multi-line CALL statements', () => {
        const resolver = new ProgramResolver();
        const lines = [
            '       CALL',
            "           'SUBPROG'",
            '           USING WS-ARG',
            '       END-CALL.'
        ];

        assert.equal(resolver.extractCalledProgramMultiLine(lines, 0), 'SUBPROG');
    });

    it('indexes workspace programs and resolves PROGRAM-ID', async () => {
        const resolver = new ProgramResolver();
        const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'cobol-prog-'));
        const programPath = path.join(tempDir, 'MAIN.cbl');
        const content = [
            '       PROGRAM-ID. MAINPROG.',
            '       PROCEDURE DIVISION.',
            '       STOP RUN.'
        ].join('\n');
        fs.writeFileSync(programPath, content, 'utf-8');

        try {
            await resolver.indexWorkspace(tempDir);
            const resolved = resolver.resolveProgram('mainprog');
            assert.ok(resolved);
            assert.equal(resolved?.programId, 'MAINPROG');
            assert.equal(resolved?.filePath, programPath);
            assert.equal(resolved?.line, 0);
        } finally {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
    });
});
