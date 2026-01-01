// src/index/symbolIndex.ts
import { TextDocument } from 'vscode-languageserver-textdocument';

/**
 * PROCEDURE DIVISION内で段落として扱わないCOBOL予約語のセット
 */
const EXCLUDED_PARAGRAPH_KEYWORDS = new Set([
    'END-IF', 'END-PERFORM', 'END-EVALUATE', 'END-READ', 
    'END-WRITE', 'END-SEARCH', 'END-CALL', 'END-COMPUTE',
    'END-ADD', 'END-SUBTRACT', 'END-MULTIPLY', 'END-DIVIDE',
    'END-RETURN', 'END-REWRITE', 'END-START', 'END-STRING',
    'END-UNSTRING', 'END-ACCEPT', 'END-DISPLAY', 'END-DELETE',
    'ELSE', 'WHEN', 'EXIT'
]);

/**
 * シンボル情報インターフェース
 */
export interface SymbolInfo {
    name: string;
    type: 'variable' | 'paragraph' | 'section' | 'division';
    level?: number;  // COBOL level number (01, 05, etc.)
    line: number;
    column: number;
    picture?: string;  // PIC句
    endLine?: number;  // Division の終了行（division タイプのみ）
}

/**
 * シンボルインデックスクラス
 * - 変数、パラグラフ、セクション、Divisionの定義位置を管理
 * - ドキュメントごとにインデックスを保持
 */
export class SymbolIndex {
    private symbols: Map<string, SymbolInfo[]> = new Map();
    
    /**
     * ドキュメントから変数・パラグラフ・セクション・Divisionを抽出
     */
    indexDocument(document: TextDocument): void {
        const uri = document.uri;
        const symbols: SymbolInfo[] = [];
        const text = document.getText();
        const lines = text.split('\n');
        
        // COPYBOOK(.cpy) は DATA DIVISION 見出しが無いことが多いので、
        // .cpy であればデータ定義として扱う
        const fsPath = (document as any).fsPath || uri; // fallback
        const isCopybook = /\.cpy$/i.test(fsPath) || /\.cpy$/i.test(uri);
        
        let inDataDivision = isCopybook ? true : false;
        let inProcedureDivision = false;
        
        // Division の開始行を記録（後で終了行を設定するため）
        let identificationDivisionStart: number | null = null;
        let environmentDivisionStart: number | null = null;
        let dataDivisionStart: number | null = null;
        let procedureDivisionStart: number | null = null;
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();
            const contentArea = line.length > 7 ? line.substring(7) : line;
            
            // Division検出
            if (trimmed.match(/IDENTIFICATION\s+DIVISION/i)) {
                identificationDivisionStart = i;
                const match = contentArea.match(/IDENTIFICATION\s+DIVISION/i);
                const divName = 'IDENTIFICATION DIVISION';
                const columnInContent = match ? contentArea.indexOf(match[0]) : 0;
                symbols.push({
                    name: divName,
                    type: 'division',
                    line: i,
                    column: columnInContent >= 0 ? 8 + columnInContent : 8,
                    endLine: lines.length - 1  // 仮の終了行（後で更新）
                });
                continue;
            }
            if (trimmed.match(/ENVIRONMENT\s+DIVISION/i)) {
                // 前のDivisionの終了行を更新
                if (identificationDivisionStart !== null) {
                    const idDiv = symbols.find(s => s.line === identificationDivisionStart && s.type === 'division');
                    if (idDiv) idDiv.endLine = i - 1;
                }
                environmentDivisionStart = i;
                const match = contentArea.match(/ENVIRONMENT\s+DIVISION/i);
                const divName = 'ENVIRONMENT DIVISION';
                const columnInContent = match ? contentArea.indexOf(match[0]) : 0;
                symbols.push({
                    name: divName,
                    type: 'division',
                    line: i,
                    column: columnInContent >= 0 ? 8 + columnInContent : 8,
                    endLine: lines.length - 1
                });
                continue;
            }
            if (trimmed.match(/DATA\s+DIVISION/i)) {
                // 前のDivisionの終了行を更新
                if (environmentDivisionStart !== null) {
                    const envDiv = symbols.find(s => s.line === environmentDivisionStart && s.type === 'division');
                    if (envDiv) envDiv.endLine = i - 1;
                } else if (identificationDivisionStart !== null) {
                    const idDiv = symbols.find(s => s.line === identificationDivisionStart && s.type === 'division');
                    if (idDiv) idDiv.endLine = i - 1;
                }
                dataDivisionStart = i;
                const match = contentArea.match(/DATA\s+DIVISION/i);
                const divName = 'DATA DIVISION';
                const columnInContent = match ? contentArea.indexOf(match[0]) : 0;
                symbols.push({
                    name: divName,
                    type: 'division',
                    line: i,
                    column: columnInContent >= 0 ? 8 + columnInContent : 8,
                    endLine: lines.length - 1
                });
                inDataDivision = true;
                inProcedureDivision = false;
                continue;
            }
            if (trimmed.match(/PROCEDURE\s+DIVISION/i)) {
                // 前のDivisionの終了行を更新
                if (dataDivisionStart !== null) {
                    const dataDiv = symbols.find(s => s.line === dataDivisionStart && s.type === 'division');
                    if (dataDiv) dataDiv.endLine = i - 1;
                } else if (environmentDivisionStart !== null) {
                    const envDiv = symbols.find(s => s.line === environmentDivisionStart && s.type === 'division');
                    if (envDiv) envDiv.endLine = i - 1;
                } else if (identificationDivisionStart !== null) {
                    const idDiv = symbols.find(s => s.line === identificationDivisionStart && s.type === 'division');
                    if (idDiv) idDiv.endLine = i - 1;
                }
                procedureDivisionStart = i;
                const match = contentArea.match(/PROCEDURE\s+DIVISION/i);
                const divName = 'PROCEDURE DIVISION';
                const columnInContent = match ? contentArea.indexOf(match[0]) : 0;
                symbols.push({
                    name: divName,
                    type: 'division',
                    line: i,
                    column: columnInContent >= 0 ? 8 + columnInContent : 8,
                    endLine: lines.length - 1
                });
                inDataDivision = false;
                inProcedureDivision = true;
                continue;
            }
            
            // DATA DIVISION内の変数定義
            if (inDataDivision) {
                // COBOL形式: column 1-7は無視、column 8以降のコード領域をパース
                const varMatch = contentArea.match(/^\s*(\d{2})\s+([A-Z0-9\-]+)(\s+PIC\s+([^\s. ]+))?/i);
                if (varMatch) {
                    // column位置を計算（全体の行の中での位置）
                    const columnInContent = contentArea.indexOf(varMatch[2]);
                    symbols.push({
                        name: varMatch[2],
                        type: 'variable',
                        level: parseInt(varMatch[1]),
                        line: i,
                        column: 8 + columnInContent,  // column 8以降からのオフセット
                        picture: varMatch[4]
                    });
                }
                
                // 88 レベルの条件名も抽出（VALUE 句の有無は無視）
                const condMatch = contentArea.match(/^\s*88\s+([A-Z0-9\-]+)/i);
                if (condMatch) {
                    const columnInContent = contentArea.indexOf(condMatch[1]);
                    symbols.push({
                        name: condMatch[1],
                        type: 'variable',
                        level: 88,
                        line: i,
                        column: 8 + columnInContent
                    });
                }
            }
            
            // PROCEDURE DIVISION内のパラグラフ・セクション
            if (inProcedureDivision) {
                // コメント行（*から始まる行）をスキップ
                if (trimmed.startsWith('*')) {
                    continue;
                }
                
                // パラグラフ:  カラム8から始まり、ピリオドで終わる
                const paraMatch = contentArea.match(/^\s*([A-Z0-9\-]+)\./i);
                if (paraMatch && !contentArea.toUpperCase().includes('SECTION')) {
                    const paraName = paraMatch[1].toUpperCase();
                    
                    // COBOL予約語や制御構文の終端キーワードを除外
                    if (!EXCLUDED_PARAGRAPH_KEYWORDS.has(paraName)) {
                        symbols.push({
                            name: paraMatch[1],
                            type: 'paragraph',
                            line: i,
                            column: 8 + contentArea.indexOf(paraMatch[1])
                        });
                    }
                }
                
                // セクション
                const sectionMatch = contentArea.match(/^\s*([A-Z0-9\-]+)\s+SECTION/i);
                if (sectionMatch) {
                    symbols.push({
                        name: sectionMatch[1],
                        type: 'section',
                        line: i,
                        column: 8 + contentArea.indexOf(sectionMatch[1])
                    });
                }
            }
        }
        
        this.symbols.set(uri, symbols);
    }
    
    /**
     * シンボル名から定義位置を検索
     * 大文字・小文字を区別しない
     * @param documentUri ドキュメントURI
     * @param symbolName シンボル名
     * @returns シンボル情報またはnull
     */
    findSymbol(documentUri: string, symbolName: string): SymbolInfo | null {
        const symbols = this.symbols.get(documentUri);
        if (!symbols) return null;
        
        return symbols.find(s => 
            s.name.toUpperCase() === symbolName.toUpperCase()
        ) || null;
    }
    
    /**
     * すべてのシンボルを取得（補完用）
     * @param documentUri ドキュメントURI
     * @returns シンボル情報配列
     */
    getAllSymbols(documentUri: string): SymbolInfo[] {
        return this.symbols.get(documentUri) || [];
    }
}
