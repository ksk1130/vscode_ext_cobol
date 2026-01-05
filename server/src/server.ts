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
    InsertTextFormat,
    SignatureHelp,
    SignatureHelpParams,
    SignatureInformation,
    ParameterInformation
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import * as path from 'path';
import * as fs from 'fs';

import { CopybookResolver } from './resolver/copybookResolver';
import { ProgramResolver } from './resolver/programResolver';
import { SymbolIndex, SymbolInfo } from './index/symbolIndex';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let copybookResolver: CopybookResolver;
let programResolver: ProgramResolver;
let symbolIndex:  SymbolIndex;
let workspaceRoot: string | null = null;

/**
 * LSP初期化時にワークスペース情報を受け取り、各種リゾルバーを構築する。
 * @param params クライアントから渡される初期化パラメータ
 */
connection.onInitialize((params:  InitializeParams) => {
    workspaceRoot = params.rootUri ?  URI.parse(params.rootUri).fsPath : null;
    
    // リゾルバー初期化
    copybookResolver = new CopybookResolver({
        searchPaths: [
            workspaceRoot ?  path.join(workspaceRoot, 'copybooks') : '',
            workspaceRoot ? path.join(workspaceRoot, 'copy') : '',
            // cobol標準のCOPYBOOKパス
            process.env.COBOL_COPYPATH || ''
        ].filter(p => p),
        extensions: ['.cpy', '.CPY', '.cbl', '.CBL', '']
    });
    
    programResolver = new ProgramResolver();
    symbolIndex = new SymbolIndex();
    
    // ワークスペースインデックス作成
    if (workspaceRoot) {
        programResolver.indexWorkspace(workspaceRoot);
    }
    
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
 * ドキュメント変更時の処理
 * - ドキュメントをインデックス化
 * - 参照されているコピーブックをロードしてインデックス化
 * - 診断を実行
 * @param change 変更イベント
 */
documents.onDidChangeContent(change => {
    symbolIndex.indexDocument(change.document);
    const allSymbols = symbolIndex.getAllSymbols(change.document.uri);
    connection.console.log(`[onDidChangeContent] Document: ${change.document.uri.substring(change.document.uri.lastIndexOf('/'))}, Symbols: ${allSymbols.length}`);
    loadCopybooksFromDocument(change.document);
    
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
    connection.console.log(`[onDidOpen] Document: ${event.document.uri.substring(event.document.uri.lastIndexOf('/'))}, Symbols: ${allSymbols.length}`);
    loadCopybooksFromDocument(event.document);
    
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
    if (normalizedLine.startsWith('COPY')) {
        return handleCopybookJump(document, contentLine);
    }
    
    // 2. PERFORMのパラグラフ/セクションジャンプ
    if (normalizedLine.startsWith('PERFORM')) {
        return handlePerformJump(document, contentLine);
    }
    
    // 3. CALLの参照ジャンプ
    if (normalizedLine.includes('CALL')) {
        return handleProgramCallJump(document, contentLine);
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
        connection.console.log(`[Hover] Document not found: ${params.textDocument.uri}`);
        return null;
    }
    
    // 現在のドキュメントと参照されているコピーブックをインデックス
    symbolIndex.indexDocument(document);
    loadCopybooksFromDocument(document);
    
    const word = getWordAtPosition(document, params.position);
    connection.console.log(`[Hover] Word at position: "${word}"`);
    
    if (!word) {
        connection.console.log(`[Hover] No word found at position`);
        return null;
    }
    
    // 1. 現在のドキュメント内の記号を検索
    let symbol = symbolIndex.findSymbol(document.uri, word);
    connection.console.log(`[Hover] Searching in document: ${document.uri}, found: ${symbol ? 'YES' : 'NO'}`);
    
    if (symbol) {
        return createHoverForSymbol(symbol, document.uri);
    }
    
    // 2. COPY で参照されているコピーブック内の記号を検索
    const copybookResult = searchInCopybooksWithPath(document, word);
    connection.console.log(`[Hover] Searching in copybooks, found: ${copybookResult ? 'YES' : 'NO'}`);
    
    if (copybookResult) {
        const { symbol, copybookPath } = copybookResult;
        return createHoverForSymbol(symbol, URI.file(copybookPath).toString());
    }
    
    connection.console.log(`[Hover] Symbol not found: ${word}`);
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
        connection.console.log(`[DocumentSymbol] Document not found: ${params.textDocument.uri}`);
        return [];
    }
    
    // ドキュメントをインデックス化
    symbolIndex.indexDocument(document);
    loadCopybooksFromDocument(document);
    
    // すべてのシンボルを取得
    const allSymbols = symbolIndex.getAllSymbols(document.uri);
    connection.console.log(`[DocumentSymbol] Found ${allSymbols.length} symbols in document`);
    
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
        connection.console.log('[Completion] Document not found');
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
    
    connection.console.log(`[Completion] Line: "${line}", Content: "${contentLine}", Trimmed: "${trimmedLine}", Position: ${params.position.line}:${params.position.character}`);
    
    // 1. COPY文の場合、COPYBOOKの補完候補を提供（最優先）
    if (trimmedLine.startsWith('COPY')) {
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
    
    connection.console.log(`[Completion] Returning ${completions.length} completions (${variableCompletions.length} variables, ${keywordCompletions.length} keywords)`);
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
            const lineText = lines[symbol.line] || '';
            const lineLength = lineText.length;
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
        for (const [divName, divInfo] of divisions.entries()) {
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
        lines.push(`Defined in: ${fileName}${isCopybook ? ' (COPYBOOK)' : ''}`);
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
 * COPY文の参照先コピーブックへジャンプする位置を解決する。
 * @param document 現在のドキュメント
 * @param line 現在行のテキスト
 * @returns ジャンプ先の位置。解決できない場合は null。
 */
function handleCopybookJump(document: TextDocument, line: string): Definition | null {
    const copybookName = copybookResolver.extractCopybookName(line);
    if (!copybookName) return null;
    
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    const copybookPath = copybookResolver.resolveCopybook(copybookName, sourceFileDir);
    
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
    // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
    const paragraphMatch = line.match(/PERFORM\s+([\w\u0080-\uFFFF\-]+)/i);
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
 * @returns ジャンプ先の位置。見つからない場合は null。
 */
function handleProgramCallJump(document: TextDocument, line: string): Definition | null {
    const programName = programResolver.extractCalledProgram(line);
    if (!programName) return null;
    
    // ワークスペース内のプログラムをインデックス化
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    // ワークスペースルートを推定（document の親ディレクトリから）
    let workspaceDir = workspaceRoot;
    if (!workspaceDir) {
        // workspaceRoot が取得できない場合は、ドキュメント親ディレクトリを使用
        workspaceDir = sourceFileDir;
    }
    
    // ワークスペースをインデックス化（すでにインデックス済みであれば キャッシュが効く）
    programResolver.indexWorkspace(workspaceDir);
    
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
    // 1. 現在のドキュメントで記号を検索
    const symbol = symbolIndex.findSymbol(document.uri, word);
    if (symbol) {
        return Location. create(
            document.uri,
            Range.create(symbol.line, symbol.column, symbol.line, symbol.column + word.length)
        );
    }
    
    // 2. COPY で参照されているコピーブック内の記号を検索
    const copybookSymbol = searchInCopybooks(document, word);
    if (copybookSymbol) {
        return copybookSymbol;
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
    
    // コピーブックは既にロード済みと想定
    for (const line of lines) {
        const contentLine = stripSequenceArea(line);
        const normalizedLine = contentLine.trim().toUpperCase();
        
        if (normalizedLine.startsWith('COPY')) {
            // COPY 文から COPYBOOK 名と REPLACING ルールを抽出
            const copybookInfo = copybookResolver.extractCopybookInfo(contentLine);
            if (!copybookInfo.name) continue;
            
            const copybookPath = copybookResolver.resolveCopybook(copybookInfo.name, sourceFileDir);
            if (!copybookPath) continue;
            
            // コピーブックのドキュメントを取得して記号を検索
            const copybookUri = URI.file(copybookPath).toString();
            
            // REPLACING が適用された記号名で検索
            let searchWord = word;
            if (copybookInfo.replacing.length > 0) {
                // 逆変換：現在のコード内の名前 → COPYBOOK 内の元の名前
                for (const rule of copybookInfo.replacing) {
                    const regex = new RegExp(`\\b${rule.to}\\b`, 'gi');
                    searchWord = searchWord.replace(regex, rule.from);
                }
            }
            
            const copybookSymbol = symbolIndex.findSymbol(copybookUri, searchWord);
            
            if (copybookSymbol) {
                return { symbol: copybookSymbol, copybookPath };
            }
        }
    }
    
    return null;
}

/**
 * COPYで参照されているコピーブック内の記号を検索する。
 * @param document 現在のドキュメント
 * @param word 検索対象のシンボル名
 * @returns 定義位置。見つからない場合は null。
 */
function searchInCopybooks(document: TextDocument, word: string): Definition | null {
    const text = document.getText();
    const lines = text.split('\n');
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    
    // コピーブックは既にロード済みと想定
    for (const line of lines) {
        const contentLine = stripSequenceArea(line);
        const normalizedLine = contentLine.trim().toUpperCase();
        
        if (normalizedLine.startsWith('COPY')) {
            // COPY 文から COPYBOOK 名と REPLACING ルールを抽出
            const copybookInfo = copybookResolver.extractCopybookInfo(contentLine);
            if (!copybookInfo.name) continue;
            
            const copybookPath = copybookResolver.resolveCopybook(copybookInfo.name, sourceFileDir);
            if (!copybookPath) continue;
            
            // コピーブックのドキュメントを取得して記号を検索
            const copybookUri = URI.file(copybookPath).toString();
            
            // REPLACING が適用された記号名で検索
            let searchWord = word;
            if (copybookInfo.replacing.length > 0) {
                // 逆変換：現在のコード内の名前 → COPYBOOK 内の元の名前
                for (const rule of copybookInfo.replacing) {
                    const regex = new RegExp(`\\b${rule.to}\\b`, 'gi');
                    searchWord = searchWord.replace(regex, rule.from);
                }
            }
            
            const copybookSymbol = symbolIndex.findSymbol(copybookUri, searchWord);
            
            if (copybookSymbol) {
                return Location.create(
                    copybookUri,
                    Range.create(copybookSymbol.line, copybookSymbol.column, copybookSymbol.line, copybookSymbol.column + searchWord.length)
                );
            }
        }
    }
    
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
    
    // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
    const words = line.match(/[\w\u0080-\uFFFF\-]+/gi);
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
 * @param document 現在のドキュメント
 * @returns 変数の CompletionItem 配列
 */
function getVariableCompletions(document: TextDocument): CompletionItem[] {
    const completions: CompletionItem[] = [];
    
    // 現在のドキュメントからシンボルを取得
    symbolIndex.indexDocument(document);
    loadCopybooksFromDocument(document);
    
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
        }
    }
    
    // COPY で参照されているコピーブック内の変数も追加
    const text = document.getText();
    const lines = text.split('\n');
    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
    
    for (const line of lines) {
        const contentLine = stripSequenceArea(line);
        const normalizedLine = contentLine.trim().toUpperCase();
        
        if (normalizedLine.startsWith('COPY')) {
            const copybookInfo = copybookResolver.extractCopybookInfo(contentLine);
            if (!copybookInfo.name) continue;
            
            const copybookPath = copybookResolver.resolveCopybook(copybookInfo.name, sourceFileDir);
            if (!copybookPath) continue;
            
            const copybookUri = URI.file(copybookPath).toString();
            const copybookSymbols = symbolIndex.getAllSymbols(copybookUri);
            
            for (const symbol of copybookSymbols) {
                if (symbol.type === 'variable') {
                    let detail = 'From COPYBOOK';
                    if (symbol.level !== undefined) {
                        detail = `Level ${symbol.level} (COPYBOOK)`;
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
        // COPYBOOK 検索パスから候補を取得
        const searchPaths = [
            workspaceRoot ? path.join(workspaceRoot, 'copybooks') : '',
            workspaceRoot ? path.join(workspaceRoot, 'copy') : '',
            workspaceRoot ? path.join(workspaceRoot, 'COPY') : '',
            sourceFileDir
        ].filter(p => p && fs.existsSync(p));
        
        const extensions = ['.cpy', '.CPY', '.cbl', '.CBL'];
        
        for (const searchPath of searchPaths) {
            const files = fs.readdirSync(searchPath);
            
            for (const file of files) {
                const ext = path.extname(file);
                if (extensions.includes(ext) || ext === '') {
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
        connection.console.log(`[getCopybookCompletions] Error reading copybook directories: ${err}`);
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
        
        // コピーブックを事前にロードしてインデックス化
        loadCopybooksFromDocument(document);
        
        // 現在のドキュメントと参照されているコピーブック内の定義済み変数を取得
        const allDefinedSymbols = new Set<string>();
        const definedSymbols = symbolIndex.getAllSymbols(document.uri);
        definedSymbols.forEach(s => allDefinedSymbols.add(s.name.toUpperCase()));
        
        // コピーブック内の定義も含める
        for (const line of lines) {
            const contentLine = stripSequenceArea(line);
            const normalizedLine = contentLine.trim().toUpperCase();
            
            if (normalizedLine.startsWith('COPY')) {
                const copybookName = copybookResolver.extractCopybookName(contentLine);
                if (copybookName) {
                    const sourceFileDir = path.dirname(URI.parse(document.uri).fsPath);
                    const copybookPath = copybookResolver.resolveCopybook(copybookName, sourceFileDir);
                    if (copybookPath) {
                        const copybookUri = URI.file(copybookPath).toString();
                        const copybookSymbols = symbolIndex.getAllSymbols(copybookUri);
                        copybookSymbols.forEach(s => allDefinedSymbols.add(s.name.toUpperCase()));
                    }
                }
            }
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
            // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
            const moveMatch = contentLine.match(/MOVE\s+([\w\u0080-\uFFFF\-]+(?:\.[\w\u0080-\uFFFF\-]+)*)\s+TO\s+([\w\u0080-\uFFFF\-]+(?:\.[\w\u0080-\uFFFF\-]+)*)/i);
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
        // 日本語を含むUnicode文字をサポート: [\w\u0080-\uFFFF\-]+
        const words = contentLine.match(/\b[\w\u0080-\uFFFF\-]+\b/gi);
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
        connection.console.error(`[validateDocument] Error: ${errorMessage}`);
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
    
    for (const line of lines) {
        const contentLine = stripSequenceArea(line);
        const normalizedLine = contentLine.trim().toUpperCase();
        
        if (normalizedLine.startsWith('COPY')) {
            // COPY 文から COPYBOOK 名と REPLACING ルールを抽出
            const copybookInfo = copybookResolver.extractCopybookInfo(contentLine);
            if (!copybookInfo.name) continue;
            
            const copybookPath = copybookResolver.resolveCopybook(copybookInfo.name, sourceFileDir);
            if (!copybookPath || !fs.existsSync(copybookPath)) continue;
            
            try {
                let copybookContent = fs.readFileSync(copybookPath, 'utf-8');
                
                // REPLACING ルールを適用
                if (copybookInfo.replacing.length > 0) {
                    copybookContent = copybookResolver.applyReplacingRules(copybookContent, copybookInfo.replacing);
                }
                
                const copybookDoc = TextDocument.create(
                    URI.file(copybookPath).toString(),
                    'cobol',
                    1,
                    copybookContent
                );
                symbolIndex.indexDocument(copybookDoc);
            } catch (err) {
                // エラーは無視
            }
        }
    }
}
