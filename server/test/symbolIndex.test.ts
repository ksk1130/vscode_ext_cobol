import assert from 'node:assert/strict';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { SymbolIndex } from '../src/index/symbolIndex';

describe('SymbolIndex', () => {
    it('indexes variables, sections, and paragraphs', () => {
        const index = new SymbolIndex();
        const content = [
            '       IDENTIFICATION DIVISION.',
            '       PROGRAM-ID. TESTPROG.',
            '       DATA DIVISION.',
            '       WORKING-STORAGE SECTION.',
            '       01 WS-NAME PIC X.',
            '       88 WS-FLAG.',
            '       PROCEDURE DIVISION.',
            '       MAIN-SECTION SECTION.',
            '       MAIN-PARA.',
            '           MOVE WS-NAME TO WS-NAME.',
            '       END-IF.'
        ].join('\n');

        const doc = TextDocument.create('file:///test.cbl', 'cobol', 1, content);
        index.indexDocument(doc);

        assert.ok(index.findSymbol(doc.uri, 'WS-NAME'));
        assert.ok(index.findSymbol(doc.uri, 'WS-FLAG'));
        assert.ok(index.findSymbol(doc.uri, 'MAIN-SECTION'));
        assert.ok(index.findSymbol(doc.uri, 'MAIN-PARA'));
    });

    it('treats copybooks as data division even without a DATA DIVISION header', () => {
        const index = new SymbolIndex();
        const content = [
            '       01 CP-VAR PIC X.',
            '       05 CP-CHILD PIC 9.'
        ].join('\n');

        const doc = TextDocument.create('file:///copybooks/TEST.CPY', 'cobol', 1, content);
        index.indexDocument(doc);

        assert.ok(index.findSymbol(doc.uri, 'CP-VAR'));
        assert.ok(index.findSymbol(doc.uri, 'CP-CHILD'));
    });
});
