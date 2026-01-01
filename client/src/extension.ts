import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

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
