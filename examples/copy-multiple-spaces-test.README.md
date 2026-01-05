# COPY Statement Multiple Spaces Test

このテストは、COPY文とCOPYBOOK名の間に複数のスペースがある場合でも正しくCOPYBOOK名を取得できることを確認するためのものです。

## テストファイル

- `copy-multiple-spaces-test.cbl` - テスト用COBOLプログラム
- `copybooks/TEST-COPY.cpy` - テスト用COPYBOOK

## テストケース

### 1. 正常系: COPY文（様々なスペース数）

以下の COPY 文はすべて正しく認識され、COPYBOOK へのジャンプが可能です:

```cobol
COPY TEST-COPY.        (スペース1つ)
COPY  TEST-COPY.       (スペース2つ)
COPY      TEST-COPY.   (スペース6つ)
```

### 2. 異常系: COPYで始まるが COPY 文ではない

以下は COPY で始まりますが、COPY 文ではないため、COPYBOOK として扱われません:

```cobol
01  COPYBOOK-FILE.     (変数名が COPYBOOK で始まる)
01  COPY-FILE-RECORD.  (変数名が COPY- で始まる)
```

## 修正内容

### 修正前の問題

`.startsWith('COPY')` を使用していたため、以下の問題がありました:
- `COPYBOOK` や `COPY-FILE` などの変数名も COPY 文として誤検出される
- 不要な処理が実行される可能性がある

### 修正後

`/^COPY\s+/i.test()` を使用することで:
- COPY の後に必ずホワイトスペース（スペース、タブなど）が続く場合のみマッチ
- `COPYBOOK`, `COPY-FILE` などは除外される
- 複数のスペースでも正しく動作する

## 確認方法

1. VS Code で `copy-multiple-spaces-test.cbl` を開く
2. COPY 行の上で "Go to Definition" (F12) を実行
3. `copybooks/TEST-COPY.cpy` が開かれることを確認
4. COPY の後ろにカーソルを置いて Ctrl+Space を押す
5. COPYBOOK の補完候補が表示されることを確認

## 関連ファイル

- `/server/src/server.ts` - COPY 文の検出ロジックを含む
- `/server/src/resolver/copybookResolver.ts` - COPYBOOK 名の抽出ロジックを含む
