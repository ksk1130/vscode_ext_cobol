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
    copybookUri?: string;  // このシンボルが定義されているCOPYBOOKのURI（COPYBOOKから読み込まれた場合）
}

/**
 * COPYBOOK参照情報インターフェース
 * ドキュメント内で参照されているCOPYBOOKの情報を保持
 */
export interface CopybookReference {
    name: string;           // COPYBOOK名
    uri: string;            // COPYBOOKファイルのURI
    referenceLine: number;  // 参照元ドキュメント内のCOPY文の行番号
}

/**
 * シンボルインデックスクラス
 * - 変数、パラグラフ、セクション、Divisionの定義位置を管理
 * - ドキュメントごとにインデックスを保持
 * - COPYBOOK参照情報を管理し、どの変数がどのCOPYBOOKから来たかを追跡
 */
export class SymbolIndex {
    private symbols: Map<string, SymbolInfo[]> = new Map();
    // ドキュメントURIをキーとして、そのドキュメントで参照されているCOPYBOOKのリストを保持
    private copybookReferences: Map<string, CopybookReference[]> = new Map();
    private logCallback?: (message: string) => void;
    private copybookExtensions: string[] = ['.cpy', '.CPY'];  // Default copybook extensions
    
    constructor(logCallback?: (message: string) => void, copybookExtensions?: string[]) {
        this.logCallback = logCallback;
        if (copybookExtensions && copybookExtensions.length > 0) {
            this.copybookExtensions = copybookExtensions;
        }
    }
    
    /**
     * COPYBOOK拡張子設定を更新する
     * @param extensions COPYBOOK拡張子の配列
     */
    setCopybookExtensions(extensions: string[]): void {
        if (extensions && extensions.length > 0) {
            this.copybookExtensions = extensions;
        }
    }
    
    private log(message: string): void {
        if (this.logCallback) {
            this.logCallback(message);
        }
    }
    
    /**
     * ファイルパスがCOPYBOOKかどうかを判定する
     * @param filePath ファイルパスまたはURI
     * @returns COPYBOOKの場合true
     */
    private isCopybookFile(filePath: string): boolean {
        return this.copybookExtensions.some(ext => {
            if (ext === '') {
                // 拡張子なしのファイルは特別な判定が必要なのでスキップ
                return false;
            }
            const regex = new RegExp(`${ext.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, 'i');
            return regex.test(filePath);
        });
    }
    
    /**
     * ドキュメントから変数・パラグラフ・セクション・Divisionを抽出
     */
    indexDocument(document: TextDocument): void {
        const uri = document.uri;
        const symbols: SymbolInfo[] = [];
        const text = document.getText();
        const lines = text.split('\n');
        
        // COPYBOOK は DATA DIVISION 見出しが無いことが多いので、
        // copybookExtensions設定に基づいて判定してデータ定義として扱う
        const fsPath = (document as any).fsPath || uri; // fallback
        const isCopybook = this.isCopybookFile(fsPath) || this.isCopybookFile(uri);
        
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
                // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
                // \- = ASCII hyphen (U+002D), ー = katakana prolonged sound mark (U+30FC), － = full-width minus (U+FF0D)
                const varMatch = contentArea.match(/^\s*(\d{2})\s+([\w\u0080-\uFFFF\-ー－]+)(\s+PIC\s+([^\s. ]+))?/i);
                if (varMatch) {
                    const levelNum = parseInt(varMatch[1]);
                    // 88レベルは条件名として別途処理されるため、ここではスキップ
                    if (levelNum !== 88) {
                        // column位置を計算（全体の行の中での位置）
                        const columnInContent = contentArea.indexOf(varMatch[2]);
                        symbols.push({
                            name: varMatch[2],
                            type: 'variable',
                            level: levelNum,
                            line: i,
                            column: 8 + columnInContent,  // column 8以降からのオフセット
                            picture: varMatch[4]
                        });
                    }
                }
                
                // 88 レベルの条件名も抽出（VALUE 句の有無は無視）
                // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
                // \- = ASCII hyphen (U+002D), ー = katakana prolonged sound mark (U+30FC), － = full-width minus (U+FF0D)
                const condMatch = contentArea.match(/^\s*88\s+([\w\u0080-\uFFFF\-ー－]+)/i);
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
                // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
                const paraMatch = contentArea.match(/^\s*([\w\u0080-\uFFFF\-ー－]+)\./i);
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
                // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
                const sectionMatch = contentArea.match(/^\s*([\w\u0080-\uFFFF\-ー－]+)\s+SECTION/i);
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
        
        // Log indexing summary
        const fileName = uri.substring(uri.lastIndexOf('/') + 1);
        const variableCount = symbols.filter(s => s.type === 'variable').length;
        const paragraphCount = symbols.filter(s => s.type === 'paragraph').length;
        const sectionCount = symbols.filter(s => s.type === 'section').length;
        this.log(`[SymbolIndex] Indexed ${fileName}: ${symbols.length} total symbols (${variableCount} variables, ${paragraphCount} paragraphs, ${sectionCount} sections)`);
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
    
    /**
     * ドキュメントにCOPYBOOK参照を登録する
     * @param documentUri ドキュメントURI
     * @param copybookName COPYBOOK名
     * @param copybookUri COPYBOOKファイルのURI
     * @param referenceLine COPY文の行番号
     */
    registerCopybookReference(documentUri: string, copybookName: string, copybookUri: string, referenceLine: number): void {
        if (!this.copybookReferences.has(documentUri)) {
            this.copybookReferences.set(documentUri, []);
        }
        
        const references = this.copybookReferences.get(documentUri)!;
        const docFileName = documentUri.substring(documentUri.lastIndexOf('/') + 1);
        
        // 既に同じCOPYBOOKが登録されていない場合のみ追加
        if (!references.some(ref => ref.uri === copybookUri)) {
            references.push({
                name: copybookName,
                uri: copybookUri,
                referenceLine: referenceLine
            });
            
            // Log COPYBOOK registration
            const copybookSymbols = this.symbols.get(copybookUri) || [];
            const variableCount = copybookSymbols.filter(s => s.type === 'variable').length;
            this.log(`[SymbolIndex] Registered COPYBOOK "${copybookName}" for ${docFileName} (line ${referenceLine + 1}): ${copybookSymbols.length} symbols (${variableCount} variables)`);
        } else {
            this.log(`[SymbolIndex] COPYBOOK "${copybookName}" already registered for ${docFileName}`);
        }
    }
    
    /**
     * ドキュメントに登録されているCOPYBOOK参照をすべて取得
     * @param documentUri ドキュメントURI
     * @returns COPYBOOK参照配列
     */
    getCopybookReferences(documentUri: string): CopybookReference[] {
        return this.copybookReferences.get(documentUri) || [];
    }
    
    /**
     * ドキュメントと参照されているすべてのCOPYBOOKからシンボルを検索
     * 同じ名前のシンボルが複数ある場合は、すべて返す（COPYBOOK情報付き）
     * @param documentUri ドキュメントURI
     * @param symbolName シンボル名
     * @returns 見つかったシンボル情報の配列
     */
    findSymbolsWithCopybookContext(documentUri: string, symbolName: string): SymbolInfo[] {
        const results: SymbolInfo[] = [];
        
        // 1. ドキュメント自身のシンボルを検索
        this.log(`[SymbolIndex] Searching for symbol "${symbolName}" in ${documentUri}`);
        const documentSymbols = this.symbols.get(documentUri) || [];
        const matchedInDocument = documentSymbols.filter(s => 
            s.name.toUpperCase() === symbolName.toUpperCase()
        );
        results.push(...matchedInDocument);
        
        // 2. 参照されているCOPYBOOK内のシンボルを検索
        const copybookRefs = this.copybookReferences.get(documentUri) || [];
        for (const ref of copybookRefs) {
            this.log(`[SymbolIndex] Searching for symbol "${symbolName}" in COPYBOOK "${ref.name}" (${ref.uri})`);
            const copybookSymbols = this.symbols.get(ref.uri) || [];
            const matchedInCopybook = copybookSymbols.filter(s => 
                s.name.toUpperCase() === symbolName.toUpperCase()
            );
            // COPYBOOKから来たことを示すため、copybookUriを設定
            for (const symbol of matchedInCopybook) {
                results.push({
                    ...symbol,
                    copybookUri: ref.uri
                });
            }
        }
        
        // Log search results if logging is enabled
        if (results.length > 0) {
            const docFileName = documentUri.substring(documentUri.lastIndexOf('/') + 1);
            if (results.length === 1) {
                const source = results[0].copybookUri 
                    ? this.getCopybookName(results[0].copybookUri) || 'COPYBOOK'
                    : docFileName;
                this.log(`[SymbolIndex] Found symbol "${symbolName}" in ${source}`);
            } else {
                const sources = results.map(r => 
                    r.copybookUri ? this.getCopybookName(r.copybookUri) || 'COPYBOOK' : docFileName
                );
                this.log(`[SymbolIndex] Found ${results.length} definitions of "${symbolName}" in: ${sources.join(', ')}`);
            }
        }
        
        return results;
    }
    
    /**
     * COPYBOOK URI からCOPYBOOK名を取得する
     * @param copybookUri COPYBOOK URI
     * @returns COPYBOOK名（ファイル名から拡張子を除いたもの）
     */
    getCopybookName(copybookUri: string): string | null {
        try {
            // Use Node.js path module for cross-platform compatibility
            const path = require('path');
            // Parse URI to file path first
            const fsPath = copybookUri.replace(/^file:\/\//, '');
            const basename = path.basename(fsPath);
            const extname = path.extname(basename);
            
            // Check if the extension matches any of the configured COPYBOOK extensions
            if (extname && this.copybookExtensions.some(ext => ext.toLowerCase() === extname.toLowerCase())) {
                return basename.substring(0, basename.length - extname.length);
            }
            return basename;
        } catch {
            return null;
        }
    }
    
    /**
     * COPYBOOK参照テーブルの状態をログ出力する
     * @param documentUri ドキュメントURI（指定された場合はそのドキュメントのみ、未指定の場合は全て）
     */
    logCopybookTableStatus(documentUri?: string): void {
        if (documentUri) {
            // 特定のドキュメントの状態をログ出力
            const docFileName = documentUri.substring(documentUri.lastIndexOf('/') + 1);
            const references = this.copybookReferences.get(documentUri) || [];
            
            if (references.length === 0) {
                this.log(`[SymbolIndex] COPYBOOK Table for ${docFileName}: No COPYBOOKs registered`);
            } else {
                this.log(`[SymbolIndex] COPYBOOK Table for ${docFileName}: ${references.length} COPYBOOK(s) registered`);
                references.forEach((ref, idx) => {
                    const symbols = this.symbols.get(ref.uri) || [];
                    const variableCount = symbols.filter(s => s.type === 'variable').length;
                    this.log(`[SymbolIndex]   ${idx + 1}. "${ref.name}" (line ${ref.referenceLine + 1}): ${symbols.length} symbols (${variableCount} variables)`);
                });
            }
        } else {
            // 全ドキュメントの状態をログ出力
            const totalDocs = this.copybookReferences.size;
            let totalCopybooks = 0;
            
            this.log(`[SymbolIndex] COPYBOOK Table Status: ${totalDocs} document(s) tracked`);
            
            this.copybookReferences.forEach((references, uri) => {
                totalCopybooks += references.length;
                const docFileName = uri.substring(uri.lastIndexOf('/') + 1);
                this.log(`[SymbolIndex]   ${docFileName}: ${references.length} COPYBOOK(s)`);
            });
            
            this.log(`[SymbolIndex] Total: ${totalCopybooks} COPYBOOK reference(s) across ${totalDocs} document(s)`);
        }
    }
}
