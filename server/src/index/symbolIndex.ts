// src/index/symbolIndex.ts
import { TextDocument } from 'vscode-languageserver-textdocument';

/**
 * シンボル情報インターフェース
 */
export interface SymbolInfo {
    name: string;
    type: 'variable' | 'paragraph' | 'section';
    level?:  number;  // COBOL level number (01, 05, etc.)
    line: number;
    column: number;
    picture?: string;  // PIC句
}

/**
 * シンボルインデックスクラス
 * - 変数、パラグラフ、セクションの定義位置を管理
 * - ドキュメントごとにインデックスを保持
 */
export class SymbolIndex {
    private symbols: Map<string, SymbolInfo[]> = new Map();
    
    /**
     * ドキュメントから変数・パラグラフ・セクションを抽出
     */
    indexDocument(document: TextDocument): void {
        const uri = document.uri;
        const symbols:  SymbolInfo[] = [];
        const text = document.getText();
        const lines = text.split('\n');
        
        // COPYBOOK(.cpy) は DATA DIVISION 見出しが無いことが多いので、
        // .cpy であればデータ定義として扱う
        const fsPath = (document as any).fsPath || uri; // fallback
        const isCopybook = /\.cpy$/i.test(fsPath) || /\.cpy$/i.test(uri);
        
        let inDataDivision = isCopybook ? true : false;
        let inProcedureDivision = false;
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();
            
            // Division検出
            if (trimmed.match(/DATA\s+DIVISION/i)) {
                inDataDivision = true;
                inProcedureDivision = false;
                continue;
            }
            if (trimmed.match(/PROCEDURE\s+DIVISION/i)) {
                inDataDivision = false;
                inProcedureDivision = true;
                continue;
            }
            
            // DATA DIVISION内の変数定義
            if (inDataDivision) {
                // COBOL形式: column 1-7は無視、column 8以降のコード領域をパース
                const contentArea = line.length > 7 ? line.substring(7) : line;
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
                // パラグラフ:  カラム8から始まり、ピリオドで終わる
                const contentArea = line.length > 7 ? line.substring(7) : line;
                const paraMatch = contentArea.match(/^\s*([A-Z0-9\-]+)\./i);
                if (paraMatch && ! contentArea.toUpperCase().includes('SECTION')) {
                    symbols.push({
                        name: paraMatch[1],
                        type:  'paragraph',
                        line: i,
                        column: 8 + contentArea.indexOf(paraMatch[1])
                    });
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
