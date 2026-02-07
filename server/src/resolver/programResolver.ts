// src/resolver/programResolver.ts
import * as fs from 'fs';
import * as path from 'path';
import { TextDecoder } from 'util';

/**
 * ファイルのエンコーディングを自動検出
 * @param buffer ファイルのバッファ
 * @returns 検出されたエンコーディング ('utf-8' | 'shift_jis')
 */
function detectEncoding(buffer: Buffer): 'utf-8' | 'shift_jis' {
    // BOMチェック
    if (buffer.length >= 3 && buffer[0] === 0xEF && buffer[1] === 0xBB && buffer[2] === 0xBF) {
        return 'utf-8';
    }
    
    const sampleSize = Math.min(buffer.length, 1024);
    let utf8Score = 0;
    let shiftJisScore = 0;
    
    for (let i = 0; i < sampleSize; i++) {
        const byte = buffer[i];
        
        if (i < sampleSize - 1) {
            const nextByte = buffer[i + 1];
            if (((byte >= 0x81 && byte <= 0x9F) || (byte >= 0xE0 && byte <= 0xEF)) &&
                ((nextByte >= 0x40 && nextByte <= 0x7E) || (nextByte >= 0x80 && nextByte <= 0xFC))) {
                shiftJisScore += 2;
                i++;
                continue;
            }
        }
        
        if ((byte & 0xE0) === 0xC0 && i < sampleSize - 1) {
            const nextByte = buffer[i + 1];
            if ((nextByte & 0xC0) === 0x80) {
                utf8Score += 2;
                i++;
                continue;
            }
        } else if ((byte & 0xF0) === 0xE0 && i < sampleSize - 2) {
            const byte2 = buffer[i + 1];
            const byte3 = buffer[i + 2];
            if ((byte2 & 0xC0) === 0x80 && (byte3 & 0xC0) === 0x80) {
                utf8Score += 3;
                i += 2;
                continue;
            }
        }
        
        if (byte <= 0x7F) {
            utf8Score += 0.1;
            shiftJisScore += 0.1;
        }
    }
    
    return shiftJisScore > utf8Score ? 'shift_jis' : 'utf-8';
}

/**
 * エンコーディングを自動検出してファイルを読み込む
 * @param filePath ファイルパス
 * @param logCallback ログ出力用コールバック関数（オプション）
 * @returns ファイル内容（文字列）
 */
function readFileWithEncoding(filePath: string, logCallback?: (message: string) => void): string {
    const buffer = fs.readFileSync(filePath);
    const encoding = detectEncoding(buffer);
    
    if (logCallback) {
        logCallback(`[Encoding] Detected ${encoding} for file: ${path.basename(filePath)}`);
    }
    
    try {
        const decoder = new TextDecoder(encoding);
        return decoder.decode(buffer);
    } catch (err) {
        if (logCallback) {
            logCallback(`[Encoding] Failed to decode ${filePath} as ${encoding}, falling back to utf-8`);
        }
        const decoder = new TextDecoder('utf-8');
        return decoder.decode(buffer);
    }
}

/**
 * プログラム情報インターフェース
 */
export interface ProgramInfo {
    programId: string;
    filePath: string;
    line: number;
    isExternal: boolean;
}

/**
 * プログラムリゾルバークラス
 */
export class ProgramResolver {
    private programIndex: Map<string, ProgramInfo> = new Map();
    private logCallback?: (message: string) => void;
    
    constructor(logCallback?: (message: string) => void) {
        this.logCallback = logCallback;
    }
    
    /**
     * ワークスペース内の全COBOLプログラムをインデックス化
     * @param workspacePath ワークスペースのルートパス
     * @return Promise<void>
     */
    async indexWorkspace(workspacePath: string): Promise<void> {
        const cobolFiles = this.findCobolFiles(workspacePath);
        
        for (const filePath of cobolFiles) {
            const content = readFileWithEncoding(filePath, this.logCallback);
            const programInfo = this.extractProgramId(content, filePath);
            
            if (programInfo) {
                this.programIndex.set(
                    programInfo.programId. toUpperCase(), 
                    programInfo
                );
            }
        }
    }
    
    /**
     * CALL文からプログラム名を抽出
     * @param line CALL文の行
     * @returns プログラム名またはnull
     */
    extractCalledProgram(line: string): string | null {
        // CALL 'SUBPROG1'
        // CALL "SUBPROG1"
        // CALL WS-PROGRAM-NAME (動的CALL)
        // CALL 'SUBPROG1' USING ... 
        
        const patterns = [
            /CALL\s+'([^']+)'/i,
            /CALL\s+"([^"]+)"/i,
        ];
        
        for (const pattern of patterns) {
            const match = line.match(pattern);
            if (match) {
                return match[1];
            }
        }
        
        // 動的CALLの場合は変数名を返す
        // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
        const dynamicMatch = line.match(/CALL\s+([\w\u0080-\uFFFF\-ー－]+)/i);
        if (dynamicMatch) {
            return dynamicMatch[1];  // 変数名（後で値を追跡）
        }
        
        return null;
    }

    /**
     * 複数行にわたるCALL文からプログラム名を抽出
     * @param lines ドキュメントの全行
     * @param startLineIndex CALL文が開始する行のインデックス
     * @returns プログラム名またはnull
     */
    extractCalledProgramMultiLine(lines: string[], startLineIndex: number): string | null {
        if (startLineIndex < 0 || startLineIndex >= lines.length) {
            return null;
        }

        // CALL文が始まる行から、END-CALLまたはステートメント終了まで行を連結
        let concatenated = '';
        let foundCALL = false;
        let parenthesisBalance = 0; // 括弧のバランス追跡

        for (let i = startLineIndex; i < lines.length && i < startLineIndex + 50; i++) {
            const line = lines[i];
            
            // シーケンス領域（7文字目まで）を削除して処理
            const contentLine = line.length > 7 ? line.substring(7) : line;
            concatenated += ' ' + contentLine;

            if (contentLine.toUpperCase().includes('CALL')) {
                foundCALL = true;
            }

            if (foundCALL) {
                // 括弧のバランスを追跡
                for (const char of contentLine) {
                    if (char === '(') parenthesisBalance++;
                    if (char === ')') parenthesisBalance--;
                }

                // ステートメント終了のシグナル: END-CALL
                if (contentLine.toUpperCase().includes('END-CALL')) {
                    break;
                }

                // ピリオド（文末）が見つかり、括弧が閉じている場合
                // COBOL では文末のピリオドが重要なため、USING句内にピリオドがないと仮定
                if (contentLine.trim().endsWith('.') && parenthesisBalance === 0) {
                    break;
                }
            }
        }

        if (!foundCALL) {
            return null;
        }

        // 連結された文字列から、単一行で処理できるパターンで抽出
        return this.extractCalledProgram(concatenated);
    }
    
    /**
     * PROGRAM-IDを抽出
     * @param content ファイル内容
     * @param filePath ファイルパス
     * @returns ProgramInfoまたはnull
     */
    private extractProgramId(content: string, filePath: string): ProgramInfo | null {
        const lines = content.split('\n');
        
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
            const match = line.match(/PROGRAM-ID\.\s+([\w\u0080-\uFFFF\-ー－]+)/i);
            
            if (match) {
                // IS COMMON/IS INITIAL などのチェック
                const isExternal = ! line.toUpperCase().includes('IS COMMON');
                
                return {
                    programId: match[1],
                    filePath,
                    line:  i,
                    isExternal
                };
            }
        }
        
        return null;
    }
    
    /**
     * プログラムIDからファイルパスを解決
     * @param programId プログラムID
     * @returns ProgramInfoまたはnull
     */
    resolveProgram(programId: string): ProgramInfo | null {
        return this.programIndex.get(programId.toUpperCase()) || null;
    }
    
    /**
     * すべてのプログラムを取得
     * @returns ProgramInfo配列
     */
    getAllPrograms(): ProgramInfo[] {
        return Array.from(this.programIndex.values());
    }

    /**
     * インデックスをクリアする
     */
    clearIndex(): void {
        this.programIndex.clear();
    }
    
    /**
     * ワークスペース内のCOBOLファイルを再帰的に検索
     * @param dir 検索ディレクトリ
     * @returns COBOLファイルパス配列
     */
    private findCobolFiles(dir: string): string[] {
        const results: string[] = [];
        const extensions = ['.cbl', '.cob', '.cobol'];
        
        const walk = (currentPath: string) => {
            const files = fs.readdirSync(currentPath);
            
            for (const file of files) {
                const fullPath = path.join(currentPath, file);
                const stat = fs.statSync(fullPath);
                
                if (stat.isDirectory()) {
                    walk(fullPath);
                } else if (extensions.some(ext => file.endsWith(ext))) {
                    results. push(fullPath);
                }
            }
        };
        
        walk(dir);
        return results;
    }
}
