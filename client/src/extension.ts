import * as path from 'path';
import { workspace, ExtensionContext, window, ConfigurationTarget, ConfigurationChangeEvent } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

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
export function activate(context: ExtensionContext) {
    // 出力チャンネルを作成
    const outputChannel = window.createOutputChannel('COBOL LSP');
    
    // サーバーモジュールのパス
    const serverModule = context.asAbsolutePath(
        path.join('server', 'out', 'server.js')
    );
    
    outputChannel.appendLine(`Server module path: ${serverModule}`);

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

    // ステータスバーに表示
    window.setStatusBarMessage('cobol LSP: Active', 3000);

    outputChannel.appendLine('cobol LSP extension is now active');
    console.log('cobol LSP extension is now active');
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
