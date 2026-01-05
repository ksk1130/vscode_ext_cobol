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
    isPrefix?: boolean;  // DISJOINING/JOINING AS PREFIX の場合は true
}


/**
 * COPYBOOKリゾルバークラス
 */
export class CopybookResolver {
    private copybookCache: Map<string, string> = new Map();
    private logCallback?: (message: string) => void;
    
    constructor(private config: CopybookConfig, logCallback?: (message: string) => void) {
        this.logCallback = logCallback;
    }
    
    /**
     * 設定を取得
     */
    getConfig(): CopybookConfig {
        return this.config;
    }
    
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
        // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
        const patterns = [
            /COPY\s+([\w\u0080-\uFFFF\-]+)/i,
            /COPY\s+"([^"]+)"/i,
            /COPY\s+'([^']+)'/i,
            /COPY\s+([\w\u0080-\uFFFF\-]+)\s+IN\s+([\w\u0080-\uFFFF\-]+)/i
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
        
        this.log(`[DEBUG-PREFIX] extractDisjoiningJoiningRules() called with: "${line}"`);
        
        // DISJOINING <old> JOINING <new> AS PREFIX のパターン
        // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
        const pattern = /DISJOINING\s+([\w\u0080-\uFFFF\-]+)\s+JOINING\s+([\w\u0080-\uFFFF\-]+)\s+AS\s+PREFIX/gi;
        let match;
        
        this.log(`[DEBUG-PREFIX] Pattern for extraction: ${pattern.source}`);
        
        while ((match = pattern.exec(line)) !== null) {
            this.log(`[DEBUG-PREFIX] Match found! from="${match[1]}" to="${match[2]}"`);
            rules.push({
                from: match[1].trim(),
                to: match[2].trim(),
                isPrefix: true  // 接頭辞置換であることを示す
            });
        }
        
        this.log(`[DEBUG-PREFIX] Extracted ${rules.length} DISJOINING/JOINING rules: ${JSON.stringify(rules)}`);
        
        return rules;
    }
    
    /**
     * ログ出力ヘルパー
     */
    private log(message: string): void {
        if (this.logCallback) {
            this.logCallback(message);
        }
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
            if (rule.isPrefix) {
                // 接頭辞置換（DISJOINING/JOINING AS PREFIX）
                // 例: HOGE-変数 → FUGA-変数, HOGEー変数 → FUGAー変数
                const escapedFrom = rule.from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
                // 行頭またはスペース・ピリオドの後に接頭辞があり、その後にハイフン(- or ー)が続くパターン
                // - (U+002D): ASCII hyphen
                // ー (U+30FC): Full-width katakana prolonged sound mark (Shift-JIS 817C)
                const regex = new RegExp(`(^|\\s|\\.)${escapedFrom}([-ー][\\w\\u0080-\\uFFFF\\-ー]+)`, 'gi');
                result = result.replace(regex, `$1${rule.to}$2`);
            } else {
                // 通常の単語置換（REPLACING）
                // 日本語を含むUnicode文字をサポートするため、\b（単語境界）の代わりに
                // 前後がスペース、ハイフン、ピリオド、または文字列の始終であることを確認
                // rule.fromに含まれる正規表現の特殊文字をエスケープ
                const escapedFrom = rule.from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
                const regex = new RegExp(`(?<=^|\\s|-|\\.)${escapedFrom}(?=\\s|-|\\.|$)`, 'gi');
                result = result.replace(regex, rule.to);
            }
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
    
    /**
     * 設定された検索パス配下のCOPYBOOKファイルをスキャンしてログ出力
     * copybookResolverの動作状況を確認するための診断機能
     */
    scanAndLogCopybookFiles(): void {
        if (!this.logCallback) {
            return;
        }
        
        this.logCallback('[CopybookResolver] Scanning COPYBOOK files...');
        this.logCallback(`[CopybookResolver] Search paths: ${this.config.searchPaths.join(', ')}`);
        this.logCallback(`[CopybookResolver] Extensions: ${this.config.extensions.join(', ')}`);
        
        let totalFiles = 0;
        
        for (const searchPath of this.config.searchPaths) {
            if (!searchPath || !fs.existsSync(searchPath)) {
                this.logCallback(`[CopybookResolver]   Path not found: ${searchPath}`);
                continue;
            }
            
            try {
                // withFileTypes オプションを使用して効率的にファイル情報を取得
                const dirents = fs.readdirSync(searchPath, { withFileTypes: true });
                const copybookFiles: string[] = [];
                
                for (const dirent of dirents) {
                    // ディレクトリはスキップ
                    if (!dirent.isFile()) {
                        continue;
                    }
                    
                    // 拡張子チェック
                    const ext = path.extname(dirent.name);
                    if (this.config.extensions.includes(ext)) {
                        copybookFiles.push(dirent.name);
                    }
                }
                
                totalFiles += copybookFiles.length;
                
                if (copybookFiles.length > 0) {
                    this.logCallback(`[CopybookResolver]   Found ${copybookFiles.length} file(s) in ${searchPath}:`);
                    copybookFiles.forEach(file => {
                        this.logCallback!(`[CopybookResolver]     - ${file}`);
                    });
                } else {
                    this.logCallback(`[CopybookResolver]   No COPYBOOK files found in ${searchPath}`);
                }
            } catch (err) {
                const errorMessage = err instanceof Error ? err.message : String(err);
                this.logCallback(`[CopybookResolver]   Error scanning ${searchPath}: ${errorMessage}`);
            }
        }
        
        this.logCallback(`[CopybookResolver] Total COPYBOOK files found: ${totalFiles}`);
    }
}
