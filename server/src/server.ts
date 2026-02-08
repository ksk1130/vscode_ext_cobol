// src/server.ts
import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DefinitionParams,
    Definition,
    Location,
    Range,
    Position,
    TextDocumentSyncKind,
    HoverParams,
    Hover,
    MarkupContent,
    Diagnostic,
    DiagnosticSeverity,
    DocumentSymbol,
    DocumentSymbolParams,
    SymbolKind,
    CompletionItem,
    CompletionItemKind,
    CompletionParams,
    SignatureHelp,
    SignatureHelpParams,
    SignatureInformation,
    ParameterInformation,
    DidChangeConfigurationNotification
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import * as path from 'path';
import * as fs from 'fs';
import { TextDecoder } from 'util';

import { CopybookResolver } from './resolver/copybookResolver';
import { ProgramResolver } from './resolver/programResolver';
import { SymbolIndex, SymbolInfo } from './index/symbolIndex';
import { configureLogger, getServerLogger } from './logger';

/**
 * ファイルのエンコーディングを自動検出
 * - UTF-8の妥当性を厳格に検証し、失敗した場合は Shift_JIS と判断する
 * @param buffer ファイルのバッファ
 * @returns 検出されたエンコーディング ('utf-8' | 'shift_jis')
 */
function detectEncoding(buffer: Buffer): 'utf-8' | 'shift_jis' {
    // UTF-8 BOM を優先
    if (buffer.length >= 3 && buffer[0] === 0xEF && buffer[1] === 0xBB && buffer[2] === 0xBF) {
        return 'utf-8';
    }

    // UTF-8 として厳格にデコードを試行し、失敗したら Shift_JIS 扱いにする
    try {
        const utf8Decoder = new TextDecoder('utf-8', { fatal: true });
        utf8Decoder.decode(buffer);
        return 'utf-8';
    } catch {
        return 'shift_jis';
    }
}

/**
 * エンコーディングを自動検出してファイルを読み込む
 * @param filePath ファイルパス
 * @returns ファイル内容（文字列）
 */
function readFileWithEncoding(filePath: string): string {
    const buffer = fs.readFileSync(filePath);
    const encoding = detectEncoding(buffer);

    logger.debug(`[Encoding] Detected ${encoding} for file: ${filePath} (size=${buffer.length} bytes)`);

    try {
        const decoder = new TextDecoder(encoding);
        const content = decoder.decode(buffer);
        logger.debug(`[Encoding] Successfully decoded ${filePath} as ${encoding}`);
        return content;
    } catch (err) {
        // フォールバック: UTF-8で試す
        logger.warn(`[Encoding] Failed to decode ${filePath} as ${encoding}, falling back to utf-8`);
        const decoder = new TextDecoder('utf-8');
        const content = decoder.decode(buffer);
        logger.debug(`[Encoding] Fallback decode succeeded for ${filePath} as utf-8`);
        return content;
    }
}

/**
 * COBOL LSP設定インターフェース
 */
interface CobolSettings {
    copybookPaths: string[];
    programSearchPaths: string[];
    fileExtensions: string[];
    copybookExtensions: string[];
    copybookAutoLoad: boolean;
    enableWarnings: boolean;
}

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

// Configure logger - if it fails, continue without structured logging
configureLogger().catch(err => {
    // Use console.error as fallback if logger configuration fails
    console.error(`Failed to configure LogTape logger: ${err}`);
});

const logger = getServerLogger();

// デフォルト設定
const defaultSettings: CobolSettings = {
    copybookPaths: [],
    programSearchPaths: [],
    fileExtensions: ['.cob', '.COB', '.cbl', '.CBL', '.cobol', '.COBOL'],
    // Include .cpy variants so COPY 社員マスター resolves even when configuration is unavailable
    copybookExtensions: ['.cpy', '.CPY', '.cbl', '.CBL', ''],
    copybookAutoLoad: false,
    enableWarnings: true
};

let globalSettings: CobolSettings = defaultSettings;

let copybookResolver: CopybookResolver;
let programResolver = new ProgramResolver((message: string) => logger.debug(message));
let symbolIndex = new SymbolIndex(
    (message: string) => logger.debug(message),
    defaultSettings.copybookExtensions
);
let workspaceRoot: string | null = null;
let hasConfigurationCapability = false;
const copybooksLoadedForDocument = new Set<string>();
const programRootsIndexed = new Set<string>();

/**
 * CopybookResolverを取得する。
 * @returns CopybookResolverインスタンス
 */
function ensureCopybookResolver(): CopybookResolver {
    if (copybookResolver) {
        return copybookResolver;
    }

    const searchPaths = resolveConfiguredPaths(globalSettings.copybookPaths)
        .concat(process.env.COBOL_COPYPATH ? [process.env.COBOL_COPYPATH] : [])
        .filter(p => p);

    if (searchPaths.length === 0 && workspaceRoot) {
        const defaultCopybooksPath = path.join(workspaceRoot, 'copybooks');
        if (fs.existsSync(defaultCopybooksPath)) {
            searchPaths.push(defaultCopybooksPath);
            logger.debug(`[updateConfiguration] Added default copybooks path: ${defaultCopybooksPath}`);
        }
    }

    copybookResolver = new CopybookResolver({
        searchPaths,
        extensions: globalSettings.copybookExtensions
    }, (message: string) => logger.debug(message));

    logger.debug(`[ensureCopybookResolver] Created resolver with paths: ${searchPaths.join(', ')}`);
    return copybookResolver;
}

/**
 * LSP初期化時にワークスペース情報を受け取り、各種リゾルバーを構築する。
 * @param params クライアントから渡される初期化パラメータ
 */
connection.onInitialize(async (params:  InitializeParams) => {
    workspaceRoot = params.rootUri ?  URI.parse(params.rootUri).fsPath : null;
    
    // クライアントの機能を確認
    const capabilities = params.capabilities;
    hasConfigurationCapability = !!(
        capabilities.workspace && !!capabilities.workspace.configuration
    );
    
    return {
        capabilities:  {
            textDocumentSync:  TextDocumentSyncKind.Full,
            definitionProvider: true,
            referencesProvider: true,
            hoverProvider: true,
            documentSymbolProvider: true,
            completionProvider: {
                resolveProvider: false,
                triggerCharacters: [' ', '-', '.']
            },
            signatureHelpProvider: {
                triggerCharacters: ['(', ',', ' ']
            }
        }
    };
});

/**
 * 初期化完了時の処理
 * 設定を取得してリゾルバーを初期化する
 */
connection.onInitialized(async () => {
    if (hasConfigurationCapability) {
        // 設定変更通知を登録
        connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
    
    // 初期設定を取得してリゾルバーを初期化
    await updateConfiguration();
});

/**
 * 設定変更時の処理
 */
connection.onDidChangeConfiguration(async change => {
    if (!hasConfigurationCapability) {
        globalSettings = <CobolSettings>(
            (change.settings.cobol || defaultSettings)
        );
    }
    
    // 設定が変更されたらリゾルバーを再初期化
    await updateConfiguration();
    
    // すべてのドキュメントを再検証
    documents.all().forEach(validateDocument);
});

/**
 * 相対パスを絶対パスに解決するヘルパー関数
 */
function resolveConfiguredPaths(paths: string[]): string[] {
    return paths.map(p => {
        if (workspaceRoot && p.startsWith('./')) {
            return path.join(workspaceRoot, p.substring(2));
        } else if (workspaceRoot && !path.isAbsolute(p)) {
            return path.join(workspaceRoot, p);
        }
        return p;
    });
}

/**
 * プログラム検索対象のルートパスを取得する
 * @param sourceFileDir 呼び出し元ファイルのディレクトリ（任意）
 */
function getProgramSearchRoots(sourceFileDir?: string): string[] {
    const roots: string[] = [];

    if (sourceFileDir) {
        roots.push(sourceFileDir);
    }

    const configuredRoots = resolveConfiguredPaths(globalSettings.programSearchPaths || [])
        .filter(p => p);
    roots.push(...configuredRoots);

    if (roots.length === 0 && workspaceRoot) {
        roots.push(workspaceRoot);
    }

    const uniqueRoots: string[] = [];
    const seen = new Set<string>();
    for (const root of roots) {
        if (!seen.has(root)) {
            seen.add(root);
            uniqueRoots.push(root);
        }
    }

    return uniqueRoots;
}

/**
 * プログラム検索パスをインデックス化する
 * @param roots 検索対象のルートパス
 */
function indexProgramRoots(roots: string[]): void {
    for (const root of roots) {
        if (!root || programRootsIndexed.has(root)) {
            continue;
        }
        if (fs.existsSync(root)) {
            programResolver.indexWorkspace(root);
            programRootsIndexed.add(root);
        }
    }
}

/**
 * 設定を取得してリゾルバーを更新する
 */
async function updateConfiguration() {
    if (hasConfigurationCapability) {
        try {
            const config = await connection.workspace.getConfiguration('cobol');
            globalSettings = {
                copybookPaths: config.copybookPaths || defaultSettings.copybookPaths,
                programSearchPaths: config.programSearchPaths || defaultSettings.programSearchPaths,
                fileExtensions: config.fileExtensions || defaultSettings.fileExtensions,
                copybookExtensions: config.copybookExtensions || defaultSettings.copybookExtensions,
                copybookAutoLoad: config.copybookAutoLoad !== undefined ? config.copybookAutoLoad : defaultSettings.copybookAutoLoad,
                enableWarnings: config.enableWarnings !== undefined ? config.enableWarnings : defaultSettings.enableWarnings
            };
        } catch (err) {
            const errorMessage = err instanceof Error ? err.message : String(err);
            logger.warn(`[updateConfiguration] Failed to get configuration: ${errorMessage}`);
            globalSettings = defaultSettings;
        }
    }

    programRootsIndexed.clear();
    
    // 設定値に基づいてCopybookResolverを再初期化
    const searchPaths = resolveConfiguredPaths(globalSettings.copybookPaths)
        .concat(process.env.COBOL_COPYPATH ? [process.env.COBOL_COPYPATH] : [])
        .filter(p => p);
    
    copybookResolver = new CopybookResolver({
        searchPaths: searchPaths,
        extensions: globalSettings.copybookExtensions
    }, (message: string) => logger.debug(message));
    
    // ProgramResolverの設定更新
    symbolIndex.setCopybookExtensions(globalSettings.copybookExtensions);
    
    // Note: programResolverはモジュールレベルで初期化されており、再設定は不要 
    // プログラム検索パスをインデックス化
    indexProgramRoots(getProgramSearchRoots());
    
    logger.debug(`[updateConfiguration] Copybook search paths: ${searchPaths.join(', ')}`);
    logger.debug(`[updateConfiguration] Copybook extensions: ${globalSettings.copybookExtensions.join(', ')}`);
}

/**
 * COPYBOOKの自動ロードが有効かどうか
 */
function shouldAutoLoadCopybooks(): boolean {
    return globalSettings.copybookAutoLoad === true;
}

/**
 * COPYBOOKを一度だけロードする（同一ドキュメントでの再ロードを抑制）
 * @param document 対象ドキュメント
 * @param force 強制的に再ロードする場合はtrue
 * @returns ロードを実行した場合はtrue
 */
function loadCopybooksOnce(document: TextDocument, force = false): boolean {
    if (!force && copybooksLoadedForDocument.has(document.uri)) {
        return false;
    }

    loadCopybooksFromDocument(document);
    copybooksLoadedForDocument.add(document.uri);
    return true;
}

/**
 * ドキュメント変更時の処理
 * - ドキュメントをインデックス化
 * - 参照されているコピーブックをロードしてインデックス化
 * - 診断を実行
 * @param change 変更イベント
 */
documents.onDidChangeContent(change => {
    symbolIndex.indexDocument(change.document);
    const allSymbols = symbolIndex.getAllSymbols(change.document.uri);
    logger.debug(`[onDidChangeContent] Document: ${change.document.uri.substring(change.document.uri.lastIndexOf('/'))}, Symbols: ${allSymbols.length}`);
    if (shouldAutoLoadCopybooks()) {
        loadCopybooksOnce(change.document);
    }
    
    // Log COPYBOOK table status after loading
    symbolIndex.logCopybookTableStatus(change.document.uri);
    
    // 診断を実行
    validateDocument(change.document);
});

/**
 * ドキュメントオープン時の処理
 * - ドキュメントをインデックス化
 * - 参照されているコピーブックをロードしてインデックス化
 * - 診断を実行
 * @param event オープンイベント
 */
documents.onDidOpen(event => {
    symbolIndex.indexDocument(event.document);
    const allSymbols = symbolIndex.getAllSymbols(event.document.uri);
    logger.debug(`[onDidOpen] Document: ${event.document.uri.substring(event.document.uri.lastIndexOf('/'))}, Symbols: ${allSymbols.length}`);
    const sourceFileDir = path.dirname(URI.parse(event.document.uri).fsPath);
    indexProgramRoots(getProgramSearchRoots(sourceFileDir));
    if (shouldAutoLoadCopybooks()) {
        loadCopybooksOnce(event.document);
    }
    
    // Log COPYBOOK table status after loading
    symbolIndex.logCopybookTableStatus(event.document.uri);
    
    // 診断を実行
    validateDocument(event.document);
});

/**
 * 定義へのジャンプ (Go to Definition) を処理する。
 * @param params 定義要求パラメータ
 * @returns 対応する定義の位置。見つからない場合は null。
 */
connection.onDefinition((params: DefinitionParams): Definition | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;
    
    const line = document.getText({
        start: { line: params.position.line, character: 0 },
        end: { line: params.position.line, character: 1000 }
    });

    const contentLine = stripSequenceArea(line);
    const normalizedLine = contentLine.trim().toUpperCase();
    
    // 1. COPYBOOKの参照ジャンプ
    // Use regex to match COPY followed by whitespace to avoid matching COPYBOOK, COPY-FILE, etc.
    // Support COPY anywhere in the line (e.g., after FD declaration)
    if (/\bCOPY\s+/i.test(normalizedLine)) {
        return handleCopybookJump(document, contentLine);
    }
    
    // 2. PERFORMのパラグラフ/セクションジャンプ
    if (normalizedLine.startsWith('PERFORM')) {
        return handlePerformJump(document, contentLine);
    }
    
    // 3. CALLの参照ジャンプ
    if (normalizedLine.includes('CALL')) {
        return handleProgramCallJump(document, contentLine, params.position.line);
    }
    
    // 4. 変数の定義ジャンプ
    const word = getWordAtPosition(document, params.position);
    if (word) {
        return handleVariableJump(document, word);
    }
    
    return null;
});

/**
 * ホバー情報の提供を行う。
 * @param params ホバー要求パラメータ
 * @returns ホバー表示用の情報。見つからない場合は null。
 */
connection.onHover((params: HoverParams): Hover | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        logger.debug(`[Hover] Document not found: ${params.textDocument.uri}`);
        return null;
    }
    
    // 現在のドキュメントと参照されているコピーブックをインデックス
    symbolIndex.indexDocument(document);
    if (shouldAutoLoadCopybooks()) {
        loadCopybooksOnce(document);
    }
    
    const word = getWordAtPosition(document, params.position);
    logger.debug(`[Hover] Word at position: "${word}"`);
    
    if (!word) {
        logger.debug(`[Hover] No word found at position`);
        return null;
    }
    
    // COPYBOOK参照を含めてシンボルを検索
    const symbols = symbolIndex.findSymbolsWithCopybookContext(document.uri, word);
    logger.debug(`[Hover] Found ${symbols.length} symbols with name "${word}"`);
    
    if (symbols.length > 0) {
        // 最初に見つかったシンボルを返す（優先順位: ドキュメント内 > COPYBOOK）
        const primarySymbol = symbols[0];
        const sourceUri = primarySymbol.copybookUri || document.uri;
        
        // 複数のCOPYBOOKに同じ変数名がある場合の情報を追加
        if (symbols.length > 1) {
            logger.debug(`[Hover] Multiple definitions found for "${word}":`);
            symbols.forEach((s, idx) => {
                const srcUri = s.copybookUri || document.uri;
                const srcName = path.basename(URI.parse(srcUri).fsPath);
                logger.debug(`[Hover]   ${idx + 1}. ${srcName} (line ${s.line + 1})`);
            });
        }
        
        return createHoverForSymbol(primarySymbol, sourceUri);
    }
    
    logger.debug(`[Hover] Symbol not found: ${word}`);
    return null;
});

/**
 * ドキュメントシンボル（アウトライン）の提供を行う。
 * @param params ドキュメントシンボル要求パラメータ
 * @returns ドキュメントシンボルの配列。見つからない場合は空配列。
 */
connection.onDocumentSymbol((params: DocumentSymbolParams): DocumentSymbol[] => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        logger.debug(`[DocumentSymbol] Document not found: ${params.textDocument.uri}`);
        return [];
    }
    
    // ドキュメントをインデックス化
    symbolIndex.indexDocument(document);
    if (shouldAutoLoadCopybooks()) {
        loadCopybooksOnce(document);
    }
    
    // すべてのシンボルを取得
    const allSymbols = symbolIndex.getAllSymbols(document.uri);
    logger.debug(`[DocumentSymbol] Found ${allSymbols.length} symbols in document`);
    
    // DocumentSymbol形式に変換
    return convertToDocumentSymbols(allSymbols, document);
});

/**
 * 自動補完候補の提供を行う。
 * @param params 補完要求パラメータ
 * @returns 補完候補の配列
 */
connection.onCompletion((params: CompletionParams): CompletionItem[] => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        logger.debug('[Completion] Document not found');
        return [];
    }
    
    const completions: CompletionItem[] = [];
    
    // 現在行のテキストを取得
    const line = document.getText({
        start: { line: params.position.line, character: 0 },
        end: { line: params.position.line, character: params.position.character }
    });
    
    const contentLine = stripSequenceArea(line);
    const trimmedLine = contentLine.trim().toUpperCase();
    
    logger.debug(`[Completion] Line: "${line}", Content: "${contentLine}", Trimmed: "${trimmedLine}", Position: ${params.position.line}:${params.position.character}`);
    
    // 1. COPY文の場合、COPYBOOKの補完候補を提供（最優先）
    // Use regex to match COPY followed by whitespace to avoid matching COPYBOOK, COPY-FILE, etc.
    if (/^COPY\s+/i.test(trimmedLine)) {
        const copybookCompletions = getCopybookCompletions(document);
        completions.push(...copybookCompletions);
        return completions;
    }
    
    // 2. PERFORM文の場合、パラグラフ/セクションの補完候補を提供
    if (trimmedLine.startsWith('PERFORM')) {
        const paragraphCompletions = getParagraphCompletions(document);
        completions.push(...paragraphCompletions);
        // キーワードも追加（PERFORM UNTIL など）
        const keywordCompletions = getCobolKeywords();
        completions.push(...keywordCompletions);
        return completions;
    }
    
    // 3. CALL文の場合、プログラム名の補完候補を提供
    if (trimmedLine.includes('CALL')) {
        const programCompletions = getProgramCompletions();
        completions.push(...programCompletions);
        // CALL 文でも変数やキーワードが必要な場合がある（USING句など）
        const variableCompletions = getVariableCompletions(document);
        completions.push(...variableCompletions);
        const keywordCompletions = getCobolKeywords();
        completions.push(...keywordCompletions);
        return completions;
    }
    
    // 4. 通常のコンテキストでは、変数とキーワードの補完候補を提供
    const variableCompletions = getVariableCompletions(document);
    completions.push(...variableCompletions);
    
    const keywordCompletions = getCobolKeywords();
    completions.push(...keywordCompletions);
    
    logger.debug(`[Completion] Returning ${completions.length} completions (${variableCompletions.length} variables, ${keywordCompletions.length} keywords)`);
    return completions;
});

/**
 * シグネチャヘルプの提供を行う（CALL文のパラメータヒント）。
 * @param params シグネチャヘルプ要求パラメータ
 * @returns シグネチャヘルプ情報
 */
connection.onSignatureHelp((params: SignatureHelpParams): SignatureHelp | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }
    
    const line = document.getText({
        start: { line: params.position.line, character: 0 },
        end: { line: params.position.line, character: params.position.character }
    });
    
    const contentLine = stripSequenceArea(line);
    const upperLine = contentLine.toUpperCase();
    
    // CALL文のシグネチャヘルプを提供
    if (upperLine.includes('CALL')) {
        const callMatch = contentLine.match(/CALL\s+['"]([^'"]+)['"]/i);
        if (callMatch) {
            const programName = callMatch[1];
            
            const signatures: SignatureInformation[] = [
                {
                    label: `CALL "${programName}" USING parameter1 parameter2 ...`,
                    documentation: `Call the program ${programName} with parameters`,
                    parameters: [
                        ParameterInformation.create('parameter1', 'First parameter'),
                        ParameterInformation.create('parameter2', 'Second parameter')
                    ]
                }
            ];
            
            return {
                signatures,
                activeSignature: 0,
                activeParameter: 0
            };
        }
    }
    
    return null;
});

/**
 * SymbolInfo配列をDocumentSymbol配列に変換する。
 * 変数の階層構造を考慮して、親子関係を構築する。
 * Divisionはコンテナとして機能し、その中の要素を含む。
 * @param symbols SymbolInfo配列
 * @param document ドキュメント（行の長さ取得に使用）
 * @returns DocumentSymbol配列（階層構造付き）
 */
function convertToDocumentSymbols(symbols: SymbolInfo[], document: TextDocument): DocumentSymbol[] {
    const documentSymbols: DocumentSymbol[] = [];
    const text = document.getText();
    const lines = text.split('\n');
    
    // Division を検出してマップを作成
    const divisions: Map<string, { symbol: DocumentSymbol; startLine: number; endLine: number }> = new Map();
    
    // まず Division を処理
    for (const symbol of symbols) {
        if (symbol.type === 'division') {
            const endLine = symbol.endLine !== undefined ? symbol.endLine : lines.length - 1;
            const endLineText = lines[endLine] || '';
            const endLineLength = endLineText.length;
            
            const range = Range.create(
                Position.create(symbol.line, 0),
                Position.create(endLine, endLineLength)
            );
            
            const selectionRange = Range.create(
                Position.create(symbol.line, symbol.column),
                Position.create(symbol.line, symbol.column + symbol.name.length)
            );
            
            const docSymbol: DocumentSymbol = {
                name: symbol.name,
                detail: undefined,
                kind: SymbolKind.Module,
                range: range,
                selectionRange: selectionRange,
                children: []
            };
            
            documentSymbols.push(docSymbol);
            divisions.set(symbol.name, {
                symbol: docSymbol,
                startLine: symbol.line,
                endLine: endLine
            });
        }
    }
    
    // Division 内の変数をグループ化するためのスタック
    const variableStack: { symbol: DocumentSymbol; level: number }[] = [];
    let currentDivision: DocumentSymbol | null = null;
    
    // 次に Division 以外のシンボルを処理
    for (const symbol of symbols) {
        if (symbol.type === 'division') continue;
        
        const symbolKind = getSymbolKind(symbol.type);
        const symbolName = symbol.name;
        
        // シンボルの範囲を設定（実際の行の長さを使用）
        const lineText = lines[symbol.line] || '';
        const lineLength = lineText.length;
        const range = Range.create(
            Position.create(symbol.line, 0),
            Position.create(symbol.line, lineLength)
        );
        
        // 選択範囲（シンボル名自体）
        const selectionRange = Range.create(
            Position.create(symbol.line, symbol.column),
            Position.create(symbol.line, symbol.column + symbolName.length)
        );
        
        // 詳細情報を構築
        let detail = '';
        if (symbol.level !== undefined) {
            detail = `Level ${symbol.level}`;
        }
        if (symbol.picture) {
            detail += detail ? ` PIC ${symbol.picture}` : `PIC ${symbol.picture}`;
        }
        
        const docSymbol: DocumentSymbol = {
            name: symbolName,
            detail: detail || undefined,
            kind: symbolKind,
            range: range,
            selectionRange: selectionRange,
            children: []
        };
        
        // このシンボルが属する Division を特定
        let belongsToDivision: DocumentSymbol | null = null;
        for (const [, divInfo] of divisions.entries()) {
            if (symbol.line >= divInfo.startLine && symbol.line <= divInfo.endLine) {
                belongsToDivision = divInfo.symbol;
                break;
            }
        }
        
        // 変数の場合、レベルに基づいて階層構造を構築
        if (symbol.type === 'variable' && symbol.level !== undefined) {
            const currentLevel = symbol.level;
            
            // Division が変わったらスタックをクリア
            if (belongsToDivision !== currentDivision) {
                variableStack.length = 0;
                currentDivision = belongsToDivision;
            }
            
            // スタックから現在のレベル以下のシンボルを削除
            while (variableStack.length > 0 && variableStack[variableStack.length - 1].level >= currentLevel) {
                variableStack.pop();
            }
            
            if (variableStack.length > 0) {
                // 親変数の子として追加
                const parent = variableStack[variableStack.length - 1].symbol;
                if (parent.children) {
                    parent.children.push(docSymbol);
                }
            } else if (belongsToDivision && belongsToDivision.children) {
                // Division の直接の子として追加
                belongsToDivision.children.push(docSymbol);
            } else {
                // Division がない場合はルートレベルに追加
                documentSymbols.push(docSymbol);
            }
            
            // スタックに追加（88レベルは親にならない）
            if (currentLevel !== 88) {
                variableStack.push({ symbol: docSymbol, level: currentLevel });
            }
        } else {
            // パラグラフやセクションは Division の子として追加
            if (belongsToDivision && belongsToDivision.children) {
                belongsToDivision.children.push(docSymbol);
            } else {
                // Division がない場合はルートレベルに追加
                documentSymbols.push(docSymbol);
            }
        }
    }
    
    return documentSymbols;
}

/**
 * シンボルタイプからVS CodeのSymbolKindに変換する。
 * @param symbolType シンボルタイプ ('variable', 'paragraph', 'section', 'division')
 * @returns SymbolKind
 */
function getSymbolKind(symbolType: string): SymbolKind {
    switch (symbolType) {
        case 'variable':
            return SymbolKind.Variable;
        case 'paragraph':
            return SymbolKind.Function;
        case 'section':
            return SymbolKind.Class;
        case 'division':
            return SymbolKind.Module;
        default:
            return SymbolKind.Variable;
    }
}

/**
 * 記号情報からホバー表示用のコンテンツを生成する。
 * @param symbol 対象となる記号
 * @param documentUri 記号が定義されているドキュメントURI
 */
function createHoverForSymbol(symbol: any, documentUri: string): Hover {
    const lines: string[] = [];
    lines.push(`**${symbol.name}** (${symbol.type})`);
    
    if (symbol.level !== undefined) {
        lines.push(`Level: ${symbol.level}`);
    }
    if (symbol.picture) {
        lines.push(`PIC: ${symbol.picture}`);
    }
    // 行・桁番号を表示（1始まり）
    if (typeof symbol.line === 'number' && typeof symbol.column === 'number') {
        lines.push(`Line: ${symbol.line + 1}, Column: ${symbol.column + 1}`);
    }
    // 参照元ファイル名（COPYBOOK名など）を表示
    try {
        const fsPath = URI.parse(documentUri).fsPath;
        const fileName = path.basename(fsPath);
        const isCopybook = /\.cpy$/i.test(fileName);
        
        // COPYBOOKから来たシンボルの場合、COPYBOOK名を明示的に表示
        if (symbol.copybookUri) {
            const copybookPath = URI.parse(symbol.copybookUri).fsPath;
            const copybookName = path.basename(copybookPath);
            lines.push(`Defined in: ${copybookName} (COPYBOOK)`);
        } else {
            lines.push(`Defined in: ${fileName}${isCopybook ? ' (COPYBOOK)' : ''}`);
        }
    } catch {}
    
    const contents: MarkupContent = {
        kind: 'markdown',
        value: lines.join('  \n')
    };
    
    return {
        contents
    };
}

/**
 * COBOL文から複数行にわたるCOPY文を結合する。
 * COPY文はピリオドで終わるまで継続する可能性がある。
 * @param lines ドキュメントの全行
 * @returns 結合されたCOPY文の配列（各要素は {statement: string, startLine: number}）
 */
function collectCopyStatements(lines: string[]): Array<{statement: string, startLine: number}> {
    const copyStatements: Array<{statement: string, startLine: number}> = [];
    let currentStatement = '';
    let inCopyStatement = false;
    let startLine = -1;
    
    for (let i = 0; i < lines.length; i++) {
        const contentLine = stripSequenceArea(lines[i]);
        const trimmedLine = contentLine.trim();
        const lineWithoutTrailingComment = trimmedLine.split(/\s*\*>/)[0].trim();
        const normalizedNoComment = lineWithoutTrailingComment.toUpperCase();

        if (/^COPY\s+/i.test(normalizedNoComment)) {
            // COPY文の開始
            inCopyStatement = true;
            startLine = i;
            currentStatement = lineWithoutTrailingComment;
            
            // 同じ行でピリオドで終わっている場合
            if (lineWithoutTrailingComment.endsWith('.')) {
                copyStatements.push({statement: currentStatement, startLine});
                currentStatement = '';
                inCopyStatement = false;
                startLine = -1;
            }
        } else if (inCopyStatement) {
            // COPY文の継続
            currentStatement += ' ' + lineWithoutTrailingComment;
            
            // ピリオドで終わっている場合
            if (lineWithoutTrailingComment.endsWith('.')) {
                copyStatements.push({statement: currentStatement, startLine});
                currentStatement = '';
                inCopyStatement = false;
                startLine = -1;
            }
        }
    }
    
    // ピリオドなしで終わった場合も追加（不完全な文）
    if (inCopyStatement && currentStatement) {
        copyStatements.push({statement: currentStatement, startLine});
    }
    
    return copyStatements;
}

/**
 * COPY文の参照先コピーブックへジャンプする位置を解決する。
 * @param document 現在のドキュメント
 * @param line 現在行のテキスト
 * @returns ジャンプ先の位置。解決できない場合は null。
 */
function handleCopybookJump(document: TextDocument, line: string): Definition | null {
    const resolver = ensureCopybookResolver();
    const copybookName = resolver.extractCopybookName(line);
    if (!copybookName) return null;
    
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    const copybookPath = resolver.resolveCopybook(copybookName, sourceFileDir);
    
    if (copybookPath) {
        return Location.create(
            URI.file(copybookPath).toString(),
            Range.create(0, 0, 0, 0)
        );
    }
    
    return null;
}

/**
 * PERFORMで指定されたパラグラフ/セクションの位置を解決する。
 * @param document 現在のドキュメント
 * @param line PERFORM行のテキスト
 * @returns ジャンプ先の位置。該当が無い場合は null。
 */
function handlePerformJump(document: TextDocument, line: string): Definition | null {
    // PERFORM の後ろの単語を抽出
    // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
    // \- = ASCII hyphen (U+002D), ー = katakana prolonged sound mark (U+30FC), － = full-width minus (U+FF0D)
    const paragraphMatch = line.match(/PERFORM\s+([\w\u0080-\uFFFF\-ー－]+)/i);
    if (!paragraphMatch) return null;
    
    const paragraphName = paragraphMatch[1];
    
    // 現在のドキュメント内でパラグラフ/セクションを検索
    const symbol = symbolIndex.findSymbol(document.uri, paragraphName);
    if (symbol && (symbol.type === 'paragraph' || symbol.type === 'section')) {
        return Location.create(
            document.uri,
            Range.create(symbol.line, symbol.column, symbol.line, symbol.column + paragraphName.length)
        );
    }
    
    return null;
}

/**
 * CALL文の呼び出し先プログラムをワークスペースから解決する。
 * @param document 現在のドキュメント
 * @param line CALL行のテキスト
 * @param lineNumber オプション: ドキュメント内の行番号（複数行対応用）
 * @returns ジャンプ先の位置。見つからない場合は null。
 */
function handleProgramCallJump(document: TextDocument, line: string, lineNumber?: number): Definition | null {
    let programName: string | null = null;

    // lineNumber が指定された場合は複数行対応の処理を試みる
    if (lineNumber !== undefined) {
        const lines = document.getText().split('\n');
        programName = programResolver.extractCalledProgramMultiLine(lines, lineNumber);
    }

    // 複数行処理で見つからなかった場合は単一行処理にフォールバック
    if (!programName) {
        programName = programResolver.extractCalledProgram(line);
    }

    if (!programName) return null;
    
    // プログラム検索パスをインデックス化
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    indexProgramRoots(getProgramSearchRoots(sourceFileDir));
    
    const programInfo = programResolver.resolveProgram(programName);
    if (programInfo) {
        return Location.create(
            URI.file(programInfo.filePath).toString(),
            Range.create(programInfo.line, 0, programInfo.line, 0)
        );
    }
    
    return null;
}

/**
 * 変数参照の定義元を検索する。
 * @param document 現在のドキュメント
 * @param word 検索対象のシンボル名
 * @returns 定義位置。見つからない場合は null。
 */
function handleVariableJump(document:  TextDocument, word: string): Definition | null {
    // COPYBOOK参照を含めてシンボルを検索
    const symbols = symbolIndex.findSymbolsWithCopybookContext(document.uri, word);
    
    if (symbols.length > 0) {
        // 最初に見つかったシンボルへジャンプ（優先順位: ドキュメント内 > COPYBOOK）
        const primarySymbol = symbols[0];
        const sourceUri = primarySymbol.copybookUri || document.uri;
        
        // 複数定義がある場合はログに記録
        if (symbols.length > 1) {
            logger.debug(`[Jump] Multiple definitions found for "${word}":`);
            symbols.forEach((s, idx) => {
                const srcUri = s.copybookUri || document.uri;
                const srcName = path.basename(URI.parse(srcUri).fsPath);
                logger.debug(`[Jump]   ${idx + 1}. ${srcName} (line ${s.line + 1})`);
            });
        }
        
        return Location.create(
            sourceUri,
            Range.create(primarySymbol.line, primarySymbol.column, primarySymbol.line, primarySymbol.column + word.length)
        );
    }
    
    return null;
}

/**
 * COPYで参照されているコピーブック内の記号を検索し、パス付きで返す。
 * @param document 現在のドキュメント
 * @param word 検索対象のシンボル名
 */
function searchInCopybooksWithPath(document: TextDocument, word: string): { symbol: any; copybookPath: string } | null {
    const text = document.getText();
    const lines = text.split('\n');
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    
    logger.debug(`[Parse-COPYBOOK] searchInCopybooksWithPath() called with word: "${word}"`);
    
    // 複数行にわたるCOPY文を結合
    const copyStatements = collectCopyStatements(lines);
    logger.debug(`[Parse-COPYBOOK] Found ${copyStatements.length} COPY statements`);
    
    const resolver = ensureCopybookResolver();

    for (const {statement: contentLine} of copyStatements) {
        logger.debug(`[Parse-COPYBOOK] Processing COPY statement: "${contentLine.substring(0, 100)}..."`);
        
        // COPY 文から COPYBOOK 名と REPLACING ルールを抽出
        const copybookInfo = resolver.extractCopybookInfo(contentLine);
        if (!copybookInfo.name) {
            logger.debug(`[Parse-COPYBOOK] No copybook name found, skipping`);
            continue;
        }
        
        logger.debug(`[Parse-COPYBOOK] Copybook name: "${copybookInfo.name}"`);
        logger.debug(`[Parse-COPYBOOK] Replacing rules: ${JSON.stringify(copybookInfo.replacing)}`);
        
        const copybookPath = resolver.resolveCopybook(copybookInfo.name, sourceFileDir);
        if (!copybookPath) {
            logger.debug(`[Parse-COPYBOOK] Copybook path not resolved, skipping`);
            continue;
        }
        
        logger.debug(`[Parse-COPYBOOK] Copybook path: "${copybookPath}"`);
        
        // コピーブックのドキュメントを取得して記号を検索
        const copybookUri = URI.file(copybookPath).toString();
        
        // REPLACING が適用された記号名で検索
        let searchWord = word;
        if (copybookInfo.replacing.length > 0) {
            logger.debug(`[Parse-COPYBOOK] Applying reverse transformation...`);
            logger.debug(`[Parse-COPYBOOK] Original word: "${searchWord}"`);
            
            // 逆変換：現在のコード内の名前 → COPYBOOK 内の元の名前
            for (const rule of copybookInfo.replacing) {
                const beforeTransform = searchWord;
                
                if (rule.isPrefix) {
                    // 接頭辞置換の逆変換
                    // 例: FUGA-変数 → HOGE-変数, FUGAー変数 → HOGEー変数
                    const escapedTo = rule.to.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
                    // - (U+002D): ASCII hyphen
                    // ー (U+30FC): Full-width katakana prolonged sound mark (Shift-JIS 817C)
                    const regex = new RegExp(`^${escapedTo}([-ー－][\w\u0080-\uFFFF\-ー－]+)$`, 'i');
                    logger.debug(`[Parse-COPYBOOK] PREFIX rule: from="${rule.from}" to="${rule.to}"`);
                    logger.debug(`[Parse-COPYBOOK] PREFIX regex pattern: ${regex.source}`);
                    logger.debug(`[Parse-COPYBOOK] Testing regex against: "${searchWord}"`);
                    
                    const match = searchWord.match(regex);
                    if (match) {
                        logger.debug(`[Parse-COPYBOOK] Regex matched! Groups: ${JSON.stringify(match)}`);
                    } else {
                        logger.debug(`[Parse-COPYBOOK] Regex did NOT match`);
                    }
                    
                    searchWord = searchWord.replace(regex, `${rule.from}$1`);
                } else {
                    // 通常の単語置換の逆変換
                    const escapedTo = rule.to.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
                    const regex = new RegExp(`^${escapedTo}$`, 'i');
                    logger.debug(`[Parse-COPYBOOK] WORD rule: from="${rule.from}" to="${rule.to}"`);
                    logger.debug(`[Parse-COPYBOOK] WORD regex pattern: ${regex.source}`);
                    searchWord = searchWord.replace(regex, rule.from);
                }
                
                if (beforeTransform !== searchWord) {
                    logger.debug(`[Parse-COPYBOOK] Transformed: "${beforeTransform}" → "${searchWord}"`);
                } else {
                    logger.debug(`[Parse-COPYBOOK] No transformation applied (word unchanged)`);
                }
            }
            
            logger.debug(`[Parse-COPYBOOK] Final search word after all transformations: "${searchWord}"`);
        }
        
        logger.debug(`[Parse-COPYBOOK] Searching for symbol "${searchWord}" in copybook "${copybookUri}"`);
        const copybookSymbol = symbolIndex.findSymbol(copybookUri, searchWord);
        
        if (copybookSymbol) {
            logger.debug(`[Parse-COPYBOOK] ✓ Symbol found! line=${copybookSymbol.line}, col=${copybookSymbol.column}`);
            return { symbol: copybookSymbol, copybookPath };
        } else {
            logger.debug(`[Parse-COPYBOOK] ✗ Symbol NOT found in this copybook`);
        }
    }
    
    logger.debug(`[Parse-COPYBOOK] Symbol "${word}" not found in any copybook`);
    return null;
}

/**
 * 指定座標に存在する単語を抽出する。
 * @param document 対象ドキュメント
 * @param position 行・桁位置
 * @returns 見つかった単語。存在しない場合は null。
 */
function getWordAtPosition(document: TextDocument, position: Position): string | null {
    const line = document.getText({
        start: { line: position.line, character: 0 },
        end: { line: position.line, character: 1000 }
    });
    
    // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
    const words = line.match(/[\w\u0080-\uFFFF\-ー－]+/gi);
    if (!words) return null;
    
    let currentPos = 0;
    for (const word of words) {
        const wordStart = line.indexOf(word, currentPos);
        const wordEnd = wordStart + word.length;
        
        if (position.character >= wordStart && position.character <= wordEnd) {
            return word;
        }
        
        currentPos = wordEnd;
    }
    
    return null;
}

/**
 * COBOL キーワードの補完候補を生成する。
 * @returns COBOL キーワードの CompletionItem 配列
 */
function getCobolKeywords(): CompletionItem[] {
    const keywords = [
        // Division keywords
        'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID', 'ENVIRONMENT', 'CONFIGURATION', 'SECTION',
        'INPUT-OUTPUT', 'FILE-CONTROL', 'DATA', 'WORKING-STORAGE', 'LOCAL-STORAGE', 
        'LINKAGE', 'FILE', 'PROCEDURE',
        
        // Data definition keywords
        'PIC', 'PICTURE', 'VALUE', 'OCCURS', 'REDEFINES', 'RENAMES', 'USAGE', 'COMP', 'COMP-3',
        'BINARY', 'DISPLAY', 'PACKED-DECIMAL', 'JUSTIFIED', 'BLANK', 'SYNCHRONIZED', 'SIGN',
        
        // Procedure keywords
        'MOVE', 'TO', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE', 'GIVING', 'REMAINDER',
        'PERFORM', 'UNTIL', 'VARYING', 'FROM', 'BY', 'AFTER', 'TIMES', 'THRU', 'THROUGH',
        'IF', 'THEN', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN', 'OTHER', 'END-EVALUATE',
        'CALL', 'USING', 'RETURNING', 'END-CALL',
        'GO', 'GOTO', 'DEPENDING', 'ON',
        'CONTINUE', 'EXIT', 'STOP', 'RUN', 'GOBACK',
        'ACCEPT', 'DISPLAY', 'UPON',
        
        // File operations
        'OPEN', 'INPUT', 'OUTPUT', 'EXTEND', 'I-O', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 
        'DELETE', 'START', 'INTO', 'AT', 'END', 'NOT', 'INVALID', 'KEY',
        
        // String operations
        'STRING', 'DELIMITED', 'SIZE', 'POINTER', 'END-STRING',
        'UNSTRING', 'DELIMITER', 'ALL', 'LEADING', 'TALLYING', 'END-UNSTRING',
        'INSPECT', 'REPLACING', 'CONVERTING', 'CHARACTERS',
        
        // Control structures
        'SEARCH', 'VARYING', 'END-SEARCH', 'SET', 'UP', 'DOWN', 'INDEX',
        'SORT', 'ASCENDING', 'DESCENDING', 'MERGE',
        
        // Logical operators
        'AND', 'OR', 'NOT', 'EQUAL', 'GREATER', 'LESS', 'THAN',
        
        // Special registers and constants
        'SPACE', 'SPACES', 'ZERO', 'ZEROS', 'ZEROES', 'HIGH-VALUE', 'HIGH-VALUES',
        'LOW-VALUE', 'LOW-VALUES', 'QUOTE', 'QUOTES', 'NULL', 'NULLS',
        
        // Conditions
        'TRUE', 'FALSE', 'POSITIVE', 'NEGATIVE', 'NUMERIC', 'ALPHABETIC',
        
        // Copy
        'COPY', 'REPLACING', 'SUPPRESS'
    ];
    
    return keywords.map(keyword => ({
        label: keyword,
        kind: CompletionItemKind.Keyword,
        detail: 'COBOL Keyword',
        insertText: keyword
    }));
}

/**
 * 変数の補完候補を生成する。
 * Generate variable completion items from current document and referenced COPYBOOKs.
 * @param document 現在のドキュメント
 * @returns 変数の CompletionItem 配列
 */
function getVariableCompletions(document: TextDocument): CompletionItem[] {
    const completions: CompletionItem[] = [];
    const addedVariables = new Set<string>(); // Track duplicates
    
    // Get symbols from current document and optionally load referenced COPYBOOKs once
    symbolIndex.indexDocument(document);
    if (shouldAutoLoadCopybooks()) {
        loadCopybooksOnce(document);
    }
    
    const symbols = symbolIndex.getAllSymbols(document.uri);
    
    // 変数の補完候補を追加
    for (const symbol of symbols) {
        if (symbol.type === 'variable') {
            let detail = '';
            if (symbol.level !== undefined) {
                detail = `Level ${symbol.level}`;
            }
            if (symbol.picture) {
                detail += detail ? ` PIC ${symbol.picture}` : `PIC ${symbol.picture}`;
            }
            
            completions.push({
                label: symbol.name,
                kind: CompletionItemKind.Variable,
                detail: detail || 'Variable',
                insertText: symbol.name
            });
            addedVariables.add(symbol.name.toUpperCase());
        }
    }
    
    // COPY で参照されているコピーブック内の変数も追加
    const copybookRefs = symbolIndex.getCopybookReferences(document.uri);
    
    for (const ref of copybookRefs) {
        const copybookSymbols = symbolIndex.getAllSymbols(ref.uri);
        const copybookName = path.basename(URI.parse(ref.uri).fsPath);
        
        for (const symbol of copybookSymbols) {
            if (symbol.type === 'variable') {
                const upperName = symbol.name.toUpperCase();
                
                // 既に追加済みの変数の場合、COPYBOOK名を追記
                if (addedVariables.has(upperName)) {
                    // 既存の補完候補を探して更新
                    const existing = completions.find(c => c.label.toUpperCase() === upperName);
                    if (existing && existing.detail) {
                        existing.detail = `${existing.detail} | Also in ${copybookName}`;
                    }
                } else {
                    let detail = `From ${copybookName}`;
                    if (symbol.level !== undefined) {
                        detail = `Level ${symbol.level} (${copybookName})`;
                    }
                    if (symbol.picture) {
                        detail += ` PIC ${symbol.picture}`;
                    }
                    
                    completions.push({
                        label: symbol.name,
                        kind: CompletionItemKind.Variable,
                        detail: detail,
                        insertText: symbol.name
                    });
                    addedVariables.add(upperName);
                }
            }
        }
    }
    
    return completions;
}

/**
 * パラグラフ/セクションの補完候補を生成する。
 * @param document 現在のドキュメント
 * @returns パラグラフ/セクションの CompletionItem 配列
 */
function getParagraphCompletions(document: TextDocument): CompletionItem[] {
    const completions: CompletionItem[] = [];
    
    symbolIndex.indexDocument(document);
    const symbols = symbolIndex.getAllSymbols(document.uri);
    
    for (const symbol of symbols) {
        if (symbol.type === 'paragraph') {
            completions.push({
                label: symbol.name,
                kind: CompletionItemKind.Function,
                detail: 'Paragraph',
                insertText: symbol.name
            });
        } else if (symbol.type === 'section') {
            completions.push({
                label: symbol.name,
                kind: CompletionItemKind.Class,
                detail: 'Section',
                insertText: symbol.name
            });
        }
    }
    
    return completions;
}

/**
 * COPYBOOK 名の補完候補を生成する。
 * @param document 現在のドキュメント
 * @returns COPYBOOK 名の CompletionItem 配列
 */
function getCopybookCompletions(document: TextDocument): CompletionItem[] {
    const completions: CompletionItem[] = [];
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    
    try {
        // 設定から COPYBOOK 検索パスを取得
        const configuredPaths = resolveConfiguredPaths(globalSettings.copybookPaths);
        
        const searchPaths = [
            sourceFileDir,
            ...configuredPaths
        ].filter(p => p && fs.existsSync(p));
        
        const extensions = globalSettings.copybookExtensions;
        
        for (const searchPath of searchPaths) {
            const files = fs.readdirSync(searchPath);
            
            for (const file of files) {
                const ext = path.extname(file);
                if (extensions.includes(ext) || (ext === '' && extensions.includes(''))) {
                    const basename = path.basename(file, ext);
                    
                    completions.push({
                        label: basename,
                        kind: CompletionItemKind.File,
                        detail: `COPYBOOK from ${path.basename(searchPath)}`,
                        insertText: basename
                    });
                }
            }
        }
    } catch (err) {
        logger.warn(`[getCopybookCompletions] Error reading copybook directories: ${err}`);
    }
    
    return completions;
}

/**
 * プログラム名の補完候補を生成する。
 * @returns プログラム名の CompletionItem 配列
 */
function getProgramCompletions(): CompletionItem[] {
    const completions: CompletionItem[] = [];
    
    // ワークスペースは初期化時にすでにインデックス化されている
    // 登録されているプログラム名を取得
    const programs = programResolver.getAllPrograms();
    
    for (const program of programs) {
        completions.push({
            label: program.programId,
            kind: CompletionItemKind.Module,
            detail: `Program from ${path.basename(program.filePath)}`,
            insertText: `"${program.programId}"`
        });
    }
    
    return completions;
}

interface LoadCopybooksParams {
    documentUri: string;
}

interface LoadCopybooksResult {
    loadedCopybooks: number;
}

interface ReloadProgramIndexParams {
    documentUri?: string;
}

interface ReloadProgramIndexResult {
    indexedPrograms: number;
    indexedRoots: number;
}

connection.onRequest('cobol/loadCopybooks', (params: LoadCopybooksParams): LoadCopybooksResult => {
    if (!params?.documentUri) {
        return { loadedCopybooks: 0 };
    }

    const document = documents.get(params.documentUri);
    if (!document) {
        return { loadedCopybooks: 0 };
    }

    symbolIndex.indexDocument(document);
    loadCopybooksOnce(document, true);

    const refs = symbolIndex.getCopybookReferences(document.uri);
    return { loadedCopybooks: refs.length };
});

connection.onRequest('cobol/reloadProgramIndex', (params: ReloadProgramIndexParams): ReloadProgramIndexResult => {
    let sourceFileDir: string | undefined;
    if (params?.documentUri) {
        const document = documents.get(params.documentUri);
        if (document) {
            sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
        }
    }

    programRootsIndexed.clear();
    programResolver.clearIndex();
    const roots = getProgramSearchRoots(sourceFileDir);
    indexProgramRoots(roots);

    return {
        indexedPrograms: programResolver.getAllPrograms().length,
        indexedRoots: roots.length
    };
});

documents.onDidClose(event => {
    copybooksLoadedForDocument.delete(event.document.uri);
});

documents.listen(connection);
connection.listen();

/**
 * COBOLのシーケンス/インジケータ領域(1-7桁)を除去する。
 * @param line 行テキスト
 */
function stripSequenceArea(line: string): string {
    // Remove COBOL sequence/indicator columns (1-7) before parsing statements.
    return line.length > 7 ? line.substring(7) : line;
}

/**
 * PIC句の情報を表すインターフェース
 */
interface PictureInfo {
    type: 'numeric' | 'alphabetic' | 'alphanumeric' | 'unknown';
    size: number;
    hasDecimal: boolean;
    decimalPlaces: number;
}

/**
 * PIC句を解析して型とサイズ情報を抽出する。
 * @param picture PIC句文字列 (例: "9(8)", "X(50)", "9(5)V99")
 * @returns PictureInfo オブジェクト
 */
function parsePicture(picture: string | undefined): PictureInfo | null {
    if (!picture) return null;
    
    const pic = picture.toUpperCase().trim();
    
    // 数値型: 9, S9, V (decimal point)
    // 例: 9(8), S9(5), 9(5)V99, S9(3)V9(2), S999, 999
    if (pic.includes('9')) {
        let size = 0;
        let decimalPlaces = 0;
        let hasDecimal = false;
        
        // V (decimal point) の前後を分けて処理
        const parts = pic.split('V');
        
        // 整数部分 (S符号も考慮、連続する9も処理)
        const integerPart = parts[0];
        // S符号を除去して9の数をカウント
        const integerDigits = integerPart.replace(/S/g, '');
        const intMatch = integerDigits.match(/9(\((\d+)\))?/g);
        if (intMatch) {
            for (const match of intMatch) {
                const parenMatch = match.match(/\((\d+)\)/);
                if (parenMatch) {
                    size += parseInt(parenMatch[1]);
                } else {
                    size += 1;
                }
            }
        }
        
        // 小数部分 (連続する9も処理)
        if (parts.length > 1) {
            hasDecimal = true;
            const decimalPart = parts[1];
            const decMatch = decimalPart.match(/9(\((\d+)\))?/g);
            if (decMatch) {
                for (const match of decMatch) {
                    const parenMatch = match.match(/\((\d+)\)/);
                    const places = parenMatch ? parseInt(parenMatch[1]) : 1;
                    decimalPlaces += places;
                    size += places;
                }
            }
        }
        
        return {
            type: 'numeric',
            size: size,
            hasDecimal: hasDecimal,
            decimalPlaces: decimalPlaces
        };
    }
    
    // 英字型: A
    // 例: A(20)
    if (pic.includes('A')) {
        let size = 0;
        const matches = pic.match(/A(\((\d+)\))?/g);
        if (matches) {
            for (const match of matches) {
                const count = match.match(/\((\d+)\)/);
                size += count ? parseInt(count[1]) : 1;
            }
        }
        return {
            type: 'alphabetic',
            size: size,
            hasDecimal: false,
            decimalPlaces: 0
        };
    }
    
    // 英数字型: X
    // 例: X(50), X(10)
    if (pic.includes('X')) {
        let size = 0;
        const matches = pic.match(/X(\((\d+)\))?/g);
        if (matches) {
            for (const match of matches) {
                const count = match.match(/\((\d+)\)/);
                size += count ? parseInt(count[1]) : 1;
            }
        }
        return {
            type: 'alphanumeric',
            size: size,
            hasDecimal: false,
            decimalPlaces: 0
        };
    }
    
    return {
        type: 'unknown',
        size: 0,
        hasDecimal: false,
        decimalPlaces: 0
    };
}

/**
 * 2つの変数の型とサイズの互換性をチェックする。
 * @param sourceInfo 代入元のPictureInfo
 * @param targetInfo 代入先のPictureInfo
 * @returns 警告メッセージ。互換性がある場合はnull。
 */
function checkTypeCompatibility(sourceInfo: PictureInfo, targetInfo: PictureInfo, sourceName: string, targetName: string): string | null {
    // 型が異なる場合
    if (sourceInfo.type !== targetInfo.type) {
        // 数値型と英数字型の混在
        if ((sourceInfo.type === 'numeric' && targetInfo.type === 'alphanumeric') ||
            (sourceInfo.type === 'alphanumeric' && targetInfo.type === 'numeric')) {
            return `型の不一致: '${sourceName}' (${sourceInfo.type}) から '${targetName}' (${targetInfo.type}) への代入`;
        }
        // 英字型と他の型の混在
        if (sourceInfo.type === 'alphabetic' || targetInfo.type === 'alphabetic') {
            return `型の不一致: '${sourceName}' (${sourceInfo.type}) から '${targetName}' (${targetInfo.type}) への代入`;
        }
    }
    
    // 数値型の場合、小数点の精度を優先してチェック
    if (sourceInfo.type === 'numeric' && targetInfo.type === 'numeric') {
        if (sourceInfo.decimalPlaces > targetInfo.decimalPlaces) {
            return `小数点以下の精度不一致: '${sourceName}' (小数点以下 ${sourceInfo.decimalPlaces} 桁) から '${targetName}' (小数点以下 ${targetInfo.decimalPlaces} 桁) への代入により、精度が失われる可能性があります`;
        }
    }
    
    // サイズが異なる場合（精度が落ちる可能性がある）
    if (sourceInfo.size > targetInfo.size) {
        return `サイズ不一致: '${sourceName}' (サイズ ${sourceInfo.size}) から '${targetName}' (サイズ ${targetInfo.size}) への代入により、データが切り捨てられる可能性があります`;
    }
    
    return null;
}

/**
 * ドキュメントを診断し、未定義変数や未使用変数の警告を生成する。
 * @param document 診断対象ドキュメント
 */
function validateDocument(document: TextDocument): void {
    // 警告機能が無効な場合はスキップ
    if (!globalSettings.enableWarnings) {
        connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
        return;
    }
    
    // COPYBOOK（.cpy）ファイルは診断をスキップ
    const isCopybook = /\.cpy$/i.test(document.uri);
    if (isCopybook) {
        connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
        return;
    }
    
    try {
        const text = document.getText();
        const lines = text.split('\n');
        const diagnostics: Diagnostic[] = [];
        
        // コピーブックを事前にロードしてインデックス化（必要時のみ）
        if (shouldAutoLoadCopybooks()) {
            loadCopybooksOnce(document);
        }
        
        // 現在のドキュメントと参照されているコピーブック内の定義済み変数を取得
        const allDefinedSymbols = new Set<string>();
        const definedSymbols = symbolIndex.getAllSymbols(document.uri);
        definedSymbols.forEach(s => allDefinedSymbols.add(s.name.toUpperCase()));
        
        // 登録されているCOPYBOOK参照からシンボルを取得
        const copybookRefs = symbolIndex.getCopybookReferences(document.uri);
        for (const ref of copybookRefs) {
            const copybookSymbols = symbolIndex.getAllSymbols(ref.uri);
            copybookSymbols.forEach(s => allDefinedSymbols.add(s.name.toUpperCase()));
            logger.debug(`[validateDocument] Added ${copybookSymbols.length} symbols from COPYBOOK "${ref.name}"`);
        }
    
    // 使用された変数を追跡
    const usedVariables = new Set<string>();
    
    let inProcedureDivision = false;
    
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const contentLine = stripSequenceArea(line);
        const trimmed = contentLine.trim();
        const normalizedLine = trimmed.toUpperCase();
        
        // PROCEDURE DIVISION以降で変数使用をチェック
        if (normalizedLine.startsWith('PROCEDURE') && normalizedLine.includes('DIVISION')) {
            inProcedureDivision = true;
            continue;
        }
        
        if (!inProcedureDivision) continue;
        
        // コメント行はスキップ
        if (normalizedLine.startsWith('*')) continue;
        
        // MOVE文の型・サイズチェック
        if (normalizedLine.includes('MOVE') && normalizedLine.includes('TO')) {
            // 修飾名（RECORD.FIELD）と単純名の両方をサポートし、末尾のピリオドは除外
            // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
            const moveMatch = contentLine.match(/MOVE\s+([\w\u0080-\uFFFF\-ー－]+(?:\.[\w\u0080-\uFFFF\-ー－]+)*)\s+TO\s+([\w\u0080-\uFFFF\-ー－]+(?:\.[\w\u0080-\uFFFF\-ー－]+)*)/i);
            if (moveMatch) {
                const sourceName = moveMatch[1];
                const targetName = moveMatch[2];
                
                // 定数（リテラル）や特殊定数は除外
                const specialConstants = ['SPACES', 'SPACE', 'ZEROS', 'ZERO', 'ZEROES', 'HIGH-VALUE', 'HIGH-VALUES', 'LOW-VALUE', 'LOW-VALUES', 'QUOTE', 'QUOTES', 'NULL', 'NULLS'];
                // 数値リテラル（整数または小数）または引用符で囲まれた文字列リテラルを検出
                const isLiteral = /^\d+(\.\d+)?$/.test(sourceName) || /^["'].*["']$/.test(sourceName);
                const isSpecialConstant = specialConstants.includes(sourceName.toUpperCase());
                
                if (!isLiteral && !isSpecialConstant) {
                    // 修飾名の場合は最後の部分のみを使用（簡略化のため）
                    const sourceBaseName = sourceName.includes('.') ? sourceName.split('.').pop()! : sourceName;
                    const targetBaseName = targetName.includes('.') ? targetName.split('.').pop()! : targetName;
                    
                    // 代入元と代入先の変数情報を取得
                    let sourceSymbol = symbolIndex.findSymbol(document.uri, sourceBaseName);
                    if (!sourceSymbol) {
                        // コピーブック内を検索
                        const copybookResult = searchInCopybooksWithPath(document, sourceBaseName);
                        if (copybookResult) {
                            sourceSymbol = copybookResult.symbol;
                        }
                    }
                    
                    let targetSymbol = symbolIndex.findSymbol(document.uri, targetBaseName);
                    if (!targetSymbol) {
                        // コピーブック内を検索
                        const copybookResult = searchInCopybooksWithPath(document, targetBaseName);
                        if (copybookResult) {
                            targetSymbol = copybookResult.symbol;
                        }
                    }
                    
                    // 両方の変数が定義されている場合のみチェック
                    if (sourceSymbol && targetSymbol && sourceSymbol.picture && targetSymbol.picture) {
                        const sourceInfo = parsePicture(sourceSymbol.picture);
                        const targetInfo = parsePicture(targetSymbol.picture);
                        
                        if (sourceInfo && targetInfo) {
                            const warningMessage = checkTypeCompatibility(sourceInfo, targetInfo, sourceBaseName, targetBaseName);
                            if (warningMessage) {
                                const targetIndex = contentLine.toUpperCase().indexOf(targetName.toUpperCase());
                                // シーケンス領域を考慮した文字位置を計算
                                const sequenceAreaLength = Math.min(7, line.length);
                                diagnostics.push({
                                    severity: DiagnosticSeverity.Warning,
                                    range: {
                                        start: { line: i, character: sequenceAreaLength + targetIndex },
                                        end: { line: i, character: sequenceAreaLength + targetIndex + targetName.length }
                                    },
                                    message: warningMessage,
                                    source: 'cobol-lsp'
                                });
                            }
                        }
                    }
                }
            }
        }
        
        // MOVE, ADD, COMPUTE, IF, EVALUATE などで使用される変数を抽出
        // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-ー－]+
        // 注: \b（単語境界）はUnicode文字で正しく動作しないため使用しない
        const words = contentLine.match(/[\w\u0080-\uFFFF\-ー－]+/gi);
        if (!words) continue;
        
        for (const word of words) {
            const upperWord = word.toUpperCase();
            
            // COBOLキーワードは除外
            const keywords = ['MOVE', 'TO', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE', 
                             'IF', 'THEN', 'ELSE', 'END-IF', 'PERFORM', 'UNTIL', 'CALL', 'USING',
                             'DISPLAY', 'ACCEPT', 'EVALUATE', 'WHEN', 'END-EVALUATE', 'SET', 'TRUE', 'FALSE',
                             'READ', 'WRITE', 'OPEN', 'CLOSE', 'VALUE', 'PIC', 'PICTURE', 'AND', 'OR', 'NOT',
                             'EQUAL', 'GREATER', 'LESS', 'SPACES', 'ZERO', 'ZEROS', 'HIGH-VALUE', 'LOW-VALUE'];
            
            if (keywords.includes(upperWord)) continue;
            
            // 使用された変数としてマーク
            usedVariables.add(upperWord);
            
            // 未定義変数のチェック（ただし代入文の場合のみ警告）
            if (!allDefinedSymbols.has(upperWord)) {
                // MOVE TO <変数> や COMPUTE <変数> = の形式で代入される変数をチェック
                const isMoveTarget = /MOVE\s+.*\s+TO\s+/i.test(contentLine) && contentLine.toUpperCase().indexOf(upperWord) > contentLine.toUpperCase().indexOf('TO');
                const isComputeTarget = /COMPUTE\s+/i.test(contentLine) && contentLine.indexOf(word) < contentLine.indexOf('=');
                
                if (isMoveTarget || isComputeTarget) {
                    const columnIndex = contentLine.indexOf(word);
                    diagnostics.push({
                        severity: DiagnosticSeverity.Warning,
                        range: {
                            start: { line: i, character: 8 + columnIndex },
                            end: { line: i, character: 8 + columnIndex + word.length }
                        },
                        message: `未定義の変数 '${word}' への代入`,
                        source: 'cobol-lsp'
                    });
                }
            }
        }
    }
    
    // 未使用変数のチェック
    for (const symbol of definedSymbols) {
        if (symbol.type !== 'variable') continue;
        
        const upperName = symbol.name.toUpperCase();
        
        // 集団項目（PIC句がないまたは配下に項目がある）の場合、配下の項目が使用されているかチェック
        if (!symbol.picture) {
            // 配下の項目を探す
            let hasUsedChild = false;
            const currentLevel = symbol.level || 0;
            
            for (const child of definedSymbols) {
                if (child.type !== 'variable') continue;
                const childLevel = child.level || 0;
                
                // 同じレベル以下が来たら集団の終わり
                if (child.line > symbol.line && childLevel > currentLevel) {
                    // 配下の項目が使用されているかチェック
                    if (usedVariables.has(child.name.toUpperCase())) {
                        hasUsedChild = true;
                        break;
                    }
                } else if (child.line > symbol.line && childLevel <= currentLevel) {
                    // 次の同レベル項目に達したら終了
                    break;
                }
            }
            
            // 配下の項目が使用されている場合は警告しない
            if (hasUsedChild) continue;
        }
        
        // 変数自体が使用されていない場合のみ警告
        if (!usedVariables.has(upperName)) {
            diagnostics.push({
                severity: DiagnosticSeverity.Information,
                range: {
                    start: { line: symbol.line, character: symbol.column },
                    end: { line: symbol.line, character: symbol.column + symbol.name.length }
                },
                message: `変数 '${symbol.name}' は定義されていますが使用されていません`,
                source: 'cobol-lsp'
            });
        }
    }
    
    connection.sendDiagnostics({ uri: document.uri, diagnostics });
    } catch (err) {
        // エラーが発生した場合はログに記録し、空の診断を送信
        const errorMessage = err instanceof Error ? err.message : String(err);
        logger.error(`[validateDocument] ${errorMessage}`);
        connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
    }
}

/**
 * ドキュメント内のCOPY文を解析し、対象コピーブックをロードしてインデックスする。
 * @param document 対象ドキュメント
 */
function loadCopybooksFromDocument(document: TextDocument): void {
    const text = document.getText();
    const lines = text.split('\n');
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    
    const fs = require('fs');
    const resolver = ensureCopybookResolver();
    
    // 複数行にわたるCOPY文を結合
    const copyStatements = collectCopyStatements(lines);
    
    for (const {statement: contentLine, startLine} of copyStatements) {
        // COPY 文から COPYBOOK 名と REPLACING ルールを抽出
        const copybookInfo = resolver.extractCopybookInfo(contentLine);
        if (!copybookInfo.name) continue;
        
        const copybookPath = resolver.resolveCopybook(copybookInfo.name, sourceFileDir);
        if (!copybookPath || !fs.existsSync(copybookPath)) continue;
        
        const copybookUri = URI.file(copybookPath).toString();
        
        try {
            let copybookContent = readFileWithEncoding(copybookPath);
            
            // REPLACING ルールを適用
            if (copybookInfo.replacing.length > 0) {
                copybookContent = resolver.applyReplacingRules(copybookContent, copybookInfo.replacing);
            }
            
            const copybookDoc = TextDocument.create(
                copybookUri,
                'cobol',
                1,
                copybookContent
            );
            
            logger.debug(`[loadCopybooksFromDocument] About to index COPYBOOK: ${copybookInfo.name}`);
            logger.debug(`[loadCopybooksFromDocument] COPYBOOK URI: ${copybookUri}`);
            logger.debug(`[loadCopybooksFromDocument] Content length: ${copybookContent.length} chars`);
            
            symbolIndex.indexDocument(copybookDoc);
            
            const indexedSymbols = symbolIndex.getAllSymbols(copybookUri);
            logger.debug(`[loadCopybooksFromDocument] Indexed ${indexedSymbols.length} symbols from ${copybookInfo.name}`);
            if (indexedSymbols.length > 0) {
                logger.debug(`[loadCopybooksFromDocument] First 5 symbols: ${indexedSymbols.slice(0, 5).map(s => s.name).join(', ')}`);
            }
            
            // COPYBOOK参照を登録
            symbolIndex.registerCopybookReference(document.uri, copybookInfo.name, copybookUri, startLine);
        } catch (err) {
            // エラーは無視
        }
    }
}
