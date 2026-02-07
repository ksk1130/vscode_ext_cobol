import * as path from 'path';
import { workspace, ExtensionContext, window, ConfigurationTarget, ConfigurationChangeEvent, commands } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';
import { configureLogger, getClientLogger } from './logger';

let client: LanguageClient;
const logger = getClientLogger();

/**
 * COBOLファイル用のルーラー設定を更新
 */
function updateRulersConfiguration() {
    const config = workspace.getConfiguration('cobol');
    const enableRulers = config.get<boolean>('enableRulers', true);
    
    // [cobol]スコープの設定を取得
    const editorConfig = workspace.getConfiguration('', { languageId: 'cobol' });
    
    if (enableRulers) {
        // ルーラーを有効にする
        editorConfig.update('editor.rulers', [7, 8, 12], ConfigurationTarget.Global, true);
    } else {
        // ルーラーを無効にする（空配列に設定）
        editorConfig.update('editor.rulers', [], ConfigurationTarget.Global, true);
    }
}

/**
 * 拡張機能の有効化
 * @param context 拡張機能コンテキスト
 */
export async function activate(context: ExtensionContext) {
    // Configure logger
    try {
        await configureLogger();
    } catch (err) {
        // If logger configuration fails, continue without structured logging
        console.error('Failed to configure LogTape logger:', err);
    }
    
    // 出力チャンネルを作成
    const outputChannel = window.createOutputChannel('COBOL LSP');
    
    // サーバーモジュールのパス
    const serverModule = context.asAbsolutePath(
        path.join('server', 'out', 'server.js')
    );
    
    outputChannel.appendLine(`Server module path: ${serverModule}`);
    logger.debug(`Server module path: ${serverModule}`);

    // 初期ルーラー設定を適用
    updateRulersConfiguration();
    
    // 設定変更を監視
    const configChangeDisposable = workspace.onDidChangeConfiguration((e: ConfigurationChangeEvent) => {
        if (e.affectsConfiguration('cobol.enableRulers')) {
            updateRulersConfiguration();
        }
    });
    context.subscriptions.push(configChangeDisposable);

    // デバッグオプション
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

    // サーバーオプション
    const serverOptions:  ServerOptions = {
        run:  { 
            module: serverModule, 
            transport: TransportKind.ipc 
        },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions
        }
    };

    // クライアントオプション
    const clientOptions: LanguageClientOptions = {
        // COBOLファイルに対してLSPを有効化
        documentSelector: [
            { scheme: 'file', language: 'cobol' },
            { scheme: 'file', pattern: '**/*.{cbl,cob,cobol,cpy,CBL,COB,COBOL,CPY}' }
        ],
        synchronize: {
            // ワークスペース設定の変更を監視
            configurationSection: 'cobol',
            // ファイル変更を監視
            fileEvents: workspace.createFileSystemWatcher('**/*.{cbl,cob,cobol,cpy,CBL,COB,COBOL,CPY}')
        },
        outputChannel: outputChannel
    };

    // Language Clientを作成して起動
    client = new LanguageClient(
        'cobolLsp',
        'cobol Language Server',
        serverOptions,
        clientOptions
    );

    // クライアントを起動
    client.start();

    interface LoadCopybooksResult {
        loadedCopybooks: number;
    }

    const loadCopybooksCommand = commands.registerCommand('cobol.loadCopybooks', async () => {
        const editor = window.activeTextEditor;
        if (!editor) {
            window.showInformationMessage('No active editor to load COPYBOOKs.');
            return;
        }

        if (editor.document.languageId !== 'cobol') {
            window.showInformationMessage('Active file is not a COBOL document.');
            return;
        }

        try {
            const response = await client.sendRequest<LoadCopybooksResult>('cobol/loadCopybooks', {
                documentUri: editor.document.uri.toString()
            });
            const loaded = response?.loadedCopybooks ?? 0;
            window.showInformationMessage(`Loaded ${loaded} COPYBOOK(s) for current file.`);
        } catch (err) {
            window.showErrorMessage(`Failed to load COPYBOOKs: ${err}`);
        }
    });
    context.subscriptions.push(loadCopybooksCommand);

    const openSettingsCommand = commands.registerCommand('cobol.openSettings', async () => {
        await commands.executeCommand('workbench.action.openSettings', 'cobol');
    });
    context.subscriptions.push(openSettingsCommand);

    // ステータスバーに表示
    window.setStatusBarMessage('cobol LSP: Active', 3000);

    outputChannel.appendLine('cobol LSP extension is now active');
    logger.info('cobol LSP extension is now active');
}

/**
 * 拡張機能の無効化
 * @returns Thenable<void> | undefined
 */
export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
