// src/resolver/copybookResolver.ts
import * as path from 'path';
import * as fs from 'fs';

/**
 * COPYBOOK解決設定インターフェース
 */
export interface CopybookConfig {
    searchPaths: string[];  // COPYBOOK検索パス
    extensions: string[];   // . cpy, .cbl など
}

/**
 * REPLACINGルールインターフェース
 */
export interface ReplacingRule {
    from: string;
    to: string;
}

/**
 * COPYBOOKリゾルバークラス
 */
export class CopybookResolver {
    private copybookCache: Map<string, string> = new Map();
    
    constructor(private config: CopybookConfig) {}
    
    /**
     * COPY文からCOPYBOOK名とREPLACING/DISJOINING/JOINING句を抽出
     * @param line COPY文の行
     * @returns COPYBOOK名とREPLACINGルール配列
     */
    extractCopybookInfo(line: string): { name: string | null; replacing: ReplacingRule[] } {
        const name = this.extractCopybookName(line);
        let replacing = this.extractReplacingRules(line);
        
        // DISJOINING/JOININGルールも抽出してマージ
        const disjoiningRules = this.extractDisjoiningJoiningRules(line);
        replacing = replacing.concat(disjoiningRules);
        
        return { name, replacing };
    }
    
    /**
     * COPY文からCOPYBOOK名を抽出
     * 例: COPY CUSTMAST. , COPY "CUSTMAST.CPY". , COPY CUSTMAST IN LIBNAME.
     * @param line COPY文の行
     * @returns COPYBOOK名またはnull
     */
    extractCopybookName(line: string): string | null {
        // COPY CUSTMAST. 
        // COPY "CUSTMAST.CPY". 
        // COPY CUSTMAST IN LIBNAME.
        // COPY CUSTMAST REPLACING ==CUST== BY ==CUSTOMER==.
        // COPY CP-SMPL2 DISJOINING XXX JOINING TODAY AS PREFIX.
        const patterns = [
            /COPY\s+([A-Z0-9\-]+)/i,
            /COPY\s+"([^"]+)"/i,
            /COPY\s+'([^']+)'/i,
            /COPY\s+([A-Z0-9\-]+)\s+IN\s+([A-Z0-9\-]+)/i
        ];
        
        for (const pattern of patterns) {
            const match = line.match(pattern);
            if (match) {
                return match[1];
            }
        }
        return null;
    }
    
    /**
     * COPY REPLACING 句を抽出
     * 例: COPY CUSTMAST REPLACING ==CUST== BY ==CUSTOMER==.
     * @return REPLACINGルール配列
     * @param line COPY文の行
     */
    extractReplacingRules(line: string): ReplacingRule[] {
        const rules: ReplacingRule[] = [];
        const replacingMatch = line.match(/REPLACING\s+(.*)/i);
        
        if (!replacingMatch) return rules;
        
        const replacingPart = replacingMatch[1];
        // ==old== BY ==new== のパターンを抽出
        const rulePattern = /==([^=]+)==\s+BY\s+==([^=]+)==/gi;
        let match;
        
        while ((match = rulePattern.exec(replacingPart)) !== null) {
            rules.push({
                from: match[1].trim(),
                to: match[2].trim()
            });
        }
        
        return rules;
    }
    
    /**
     * COPY DISJOINING/JOINING 句を抽出（NETCOBOL拡張構文）
     * 例: COPY CP-SMPL2 DISJOINING XXX JOINING TODAY AS PREFIX.
     * XXX-xxx-xxx → TODAY-xxx-xxx のように接頭辞を置き換える
     * @param line COPY文の行
     * @return REPLACINGルール配列
     */
    extractDisjoiningJoiningRules(line: string): ReplacingRule[] {
        const rules: ReplacingRule[] = [];
        
        // DISJOINING <old> JOINING <new> AS PREFIX のパターン
        const pattern = /DISJOINING\s+([A-Z0-9\-]+)\s+JOINING\s+([A-Z0-9\-]+)\s+AS\s+PREFIX/gi;
        let match;
        
        while ((match = pattern.exec(line)) !== null) {
            rules.push({
                from: match[1].trim(),
                to: match[2].trim()
            });
        }
        
        return rules;
    }
    
    /**
     * REPLACING ルールを適用して文字列を変換
     * @param text 元の文字列
     * @param rules REPLACINGルール配列
     * @returns 変換後の文字列
     */
    applyReplacingRules(text: string, rules: ReplacingRule[]): string {
        let result = text;
        for (const rule of rules) {
            // 大文字小文字を区別せず、単語境界を考慮して置換
            const regex = new RegExp(`\\b${rule.from}\\b`, 'gi');
            result = result.replace(regex, rule.to);
        }
        return result;
    }
    
    /**
     * COPYBOOKファイルのパスを解決
     * @param copybookName COPYBOOK名
     * @param sourceFileDir ソースファイルのディレクトリ
     * @returns COPYBOOKファイルの絶対パスまたはnull
     */
    resolveCopybook(copybookName: string, sourceFileDir: string): string | null {
        // キャッシュチェック
        if (this. copybookCache.has(copybookName)) {
            return this.copybookCache.get(copybookName)!;
        }
        
        // 検索パスの優先順位
        const searchPaths = [
            sourceFileDir,  // 1. ソースファイルと同じディレクトリ
            ... this.config.searchPaths  // 2. 設定された検索パス
        ];
        
        for (const searchPath of searchPaths) {
            for (const ext of this.config. extensions) {
                const candidates = [
                    path.join(searchPath, `${copybookName}${ext}`),
                    path.join(searchPath, copybookName),
                    path.join(searchPath, copybookName. toLowerCase() + ext),
                ];
                
                for (const candidate of candidates) {
                    if (fs.existsSync(candidate)) {
                        this.copybookCache.set(copybookName, candidate);
                        return candidate;
                    }
                }
            }
        }
        
        return null;
    }
    
    /**
     * cobol特有のCOPYBOOKライブラリ指定の処理
     * 例: COPY CUSTMAST IN LIBNAME.
     * @param copybookName COPYBOOK名
     * @param libraryName ライブラリ名（null可能）
     * @param sourceFileDir ソースファイルのディレクトリ
     * @returns COPYBOOKファイルの絶対パスまたはnull
     */
    resolveCopybookWithLibrary(
        copybookName: string, 
        libraryName: string | null,
        sourceFileDir:  string
    ): string | null {
        if (libraryName) {
            // COPY CUSTMAST IN LIBNAME の場合
            // cobolのライブラリ設定から解決
            const libPath = this.resolveLibraryPath(libraryName);
            if (libPath) {
                return this.resolveCopybook(copybookName, libPath);
            }
        }
        return this.resolveCopybook(copybookName, sourceFileDir);
    }
    
    /**
     * COBOLライブラリ名からパスを解決
     * @param libraryName 
     * @returns ライブラリパスまたはnull
     */
    private resolveLibraryPath(libraryName: string): string | null {
        // cobolのライブラリ設定ファイルから読み込み
        // または環境変数から取得
        const envVar = process.env[`COBOL_LIB_${libraryName}`];
        return envVar || null;
    }
}
