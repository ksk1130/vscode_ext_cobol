# COPYBOOK Variable Tracking Implementation

## 概要 (Overview)

この実装では、複数のCOPYBOOKを参照する際に同一の変数名がある場合でも、どのCOPYBOOKからその変数が定義されているかを追跡できるようになりました。

This implementation enables tracking which COPYBOOK defines each variable when multiple COPYBOOKs are referenced that contain variables with the same name.

## 問題 (Problem)

複数のCOPY句を参照する際、異なるCOPYBOOKに同じ変数名が存在するケースがありました。従来の実装では、どのCOPYBOOKから変数が来たのかを明確に追跡できませんでした。

When referencing multiple COPY statements, different copybooks may contain variables with the same name. The previous implementation could not clearly track which COPYBOOK a variable came from.

## 解決策 (Solution)

以下の変更を実施しました：

1. **SymbolInfoインターフェースの拡張**
   - `copybookUri`フィールドを追加し、シンボルがどのCOPYBOOKから来たかを記録

2. **CopybookReferenceインターフェースの追加**
   - ドキュメント内で参照されているCOPYBOOKの情報を保持

3. **SymbolIndexクラスの拡張**
   - `copybookReferences`: ドキュメントごとにCOPYBOOK参照を管理
   - `registerCopybookReference()`: COPYBOOK参照を登録
   - `getCopybookReferences()`: 登録されているCOPYBOOK参照を取得
   - `findSymbolsWithCopybookContext()`: COPYBOOK情報を含めてシンボルを検索
   - `getCopybookName()`: COPYBOOK URIから名前を取得

4. **server.tsの更新**
   - `loadCopybooksFromDocument()`: COPYBOOK参照を自動登録
   - `createHoverForSymbol()`: COPYBOOKから来たシンボルの場合、COPYBOOK名を表示
   - `onHover()`: 新しい`findSymbolsWithCopybookContext()`を使用
   - `handleVariableJump()`: 複数定義がある場合もログに記録
   - `getVariableCompletions()`: COPYBOOK名を含む詳細情報を表示、重複を適切に処理

## 動作例 (Usage Example)

### テストファイル

`examples/multiple-copybooks-test.cbl` と2つのCOPYBOOKファイル:
- `examples/copybooks/CUSTOMER-DATA.cpy`
- `examples/copybooks/PRODUCT-DATA.cpy`

両方のCOPYBOOKに `RECORD-ID` と `RECORD-NAME` という同名の変数が定義されています。

### 機能

1. **ホバー表示**
   - 変数にホバーすると、どのCOPYBOOKから定義されているかが表示されます
   - 例: "Defined in: CUSTOMER-DATA.cpy (COPYBOOK)"

2. **定義へのジャンプ**
   - 変数名をクリックして定義へジャンプすると、対応するCOPYBOOKファイルが開きます
   - 複数の定義がある場合は、最初に見つかったものへジャンプし、ログに全定義を記録

3. **自動補完**
   - COPYBOOK内の変数も補完候補に表示されます
   - 詳細情報にCOPYBOOK名が含まれます
   - 例: "Level 05 (CUSTOMER-DATA.cpy) PIC 9(8)"
   - 複数のCOPYBOOKで同じ変数名がある場合は "Also in ..." と表示

## 実装の詳細 (Implementation Details)

### データ構造

```typescript
interface SymbolInfo {
    name: string;
    type: 'variable' | 'paragraph' | 'section' | 'division';
    level?: number;
    line: number;
    column: number;
    picture?: string;
    endLine?: number;
    copybookUri?: string;  // 新規追加
}

interface CopybookReference {
    name: string;
    uri: string;
    referenceLine: number;
}
```

### 主要なメソッド

```typescript
// COPYBOOK参照を登録
symbolIndex.registerCopybookReference(
    documentUri: string,
    copybookName: string,
    copybookUri: string,
    referenceLine: number
): void

// COPYBOOK情報を含めてシンボルを検索
symbolIndex.findSymbolsWithCopybookContext(
    documentUri: string,
    symbolName: string
): SymbolInfo[]

// 登録されているCOPYBOOK参照を取得
symbolIndex.getCopybookReferences(
    documentUri: string
): CopybookReference[]
```

## テスト方法 (Testing)

1. VS Codeで `examples/multiple-copybooks-test.cbl` を開く
2. `RECORD-ID` や `RECORD-NAME` にカーソルを置く
3. ホバー表示でCOPYBOOK名が表示されることを確認
4. F12キーで定義へジャンプできることを確認
5. Ctrl+Spaceで自動補完にCOPYBOOK名が含まれることを確認

## 影響範囲 (Impact)

### 変更されたファイル
- `server/src/index/symbolIndex.ts`: COPYBOOK追跡機能の追加
- `server/src/server.ts`: COPYBOOK情報を利用するように更新

### 後方互換性
- 既存の機能は維持されています
- 新しいCOPYBOOK情報は追加情報として提供されます
- 既存のコードは影響を受けません

## 今後の改善案 (Future Improvements)

1. 複数の定義がある場合、すべての候補を表示できるUI
2. COPYBOOK内の変数に異なる色やアイコンを表示
3. REPLACING句が適用された変数名の追跡改善
4. パフォーマンス最適化（大規模プロジェクト向け）
