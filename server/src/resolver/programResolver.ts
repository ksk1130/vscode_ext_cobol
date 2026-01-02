// src/resolver/programResolver.ts
import * as fs from 'fs';
import * as path from 'path';

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
    
    /**
     * ワークスペース内の全COBOLプログラムをインデックス化
     * @param workspacePath ワークスペースのルートパス
     * @return Promise<void>
     */
    async indexWorkspace(workspacePath: string): Promise<void> {
        const cobolFiles = this.findCobolFiles(workspacePath);
        
        for (const filePath of cobolFiles) {
            const content = fs.readFileSync(filePath, 'utf-8');
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
        const dynamicMatch = line.match(/CALL\s+([A-Z0-9\-]+)/i);
        if (dynamicMatch) {
            return dynamicMatch[1];  // 変数名（後で値を追跡）
        }
        
        return null;
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
            const match = line.match(/PROGRAM-ID\.\s+([A-Z0-9\-]+)/i);
            
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
