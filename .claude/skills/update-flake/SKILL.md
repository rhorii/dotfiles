---
name: update-flake
description: flake input を更新し、実機で darwin / home-manager の構成がビルドできることを確認してから PR を作成する。flake.lock の定期更新に使う。
---

# flake 更新

以下の手順を順番どおりに実行する。手順外の調査（過去 PR の書式確認など）は不要。

## 1. 更新

```bash
nix flake update
git diff --quiet flake.lock && echo NO_CHANGE
```

`NO_CHANGE` が出たら、「更新なし」と報告して終了する（commit / PR は作らない）。

## 2. 変更内容の取得

```bash
.claude/skills/update-flake/lock-diff.sh
```

`changed` 行が更新された input、`unchanged` 行が更新なしの input。

## 3. 実機ビルド

```bash
nix build --no-link .#darwinConfigurations.hank.system .#homeConfigurations.rhorii.activationPackage
```

成否は終了コードで判定する。パイプでつながず、出力が長い場合も `tail` などは使わない（zsh では `PIPESTATUS` が使えない）。

**失敗した場合**: commit / PR は作らず、`flake.lock` の変更を `git checkout flake.lock` で戻す。失敗した derivation とエラーの要点を報告して終了する。修正は試みない。

## 4. commit と PR

ブランチ: 現在のブランチが `main` の場合のみ `rhorii/update-flake-lock-<YYYYMMDD>` を作成する。それ以外はそのまま使う。

タイトル: `flake.lock: <更新された input を " / " で連結> を更新`（例: `flake.lock: nixpkgs / home-manager を更新`）

```bash
git add flake.lock
git commit -m "<タイトル>"   # attribution 行は system の指示に従って付ける
git push -u origin HEAD
gh pr create --base main --title "<タイトル>" --body-file -
```

PR 本文（`<...>` を埋める。更新なしの input がない場合は該当行を削除する）:

```markdown
## 概要

`nix flake update` による flake input の更新。

| input | 変更 |
| --- | --- |
| <input> | <変更> |

<更新なしの input を " / " で連結> は更新なし。

## 動作確認

- `nix build --no-link .#darwinConfigurations.hank.system .#homeConfigurations.rhorii.activationPackage` が成功
```

本文末尾の attribution 行も system の指示に従う。

## 5. 報告

PR の URL と変更内容の表を報告する。`darwin-rebuild switch` / `home-manager switch` は実行しない（sudo が必要なため、適用はユーザーが行う）。
