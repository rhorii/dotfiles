# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

System (nix-darwin) と user environment (home-manager) を別コマンドで適用する 2 段構成。

```bash
# macOS システム設定の適用（fonts、Homebrew cask など）
sudo darwin-rebuild switch --flake .

# ユーザー環境（シェル、CLI ツール、dotfiles）の適用
home-manager switch --flake .

# flake input の更新（nixpkgs / nix-darwin / home-manager のロックを更新）
nix flake update

# 初回セットアップ（darwin-rebuild / home-manager コマンドが未インストールのとき）
nix run nix-darwin -- switch --flake .
nix run home-manager -- switch --flake .
```

flake は単一ホスト `hank` / 単一ユーザー `rhorii`、`aarch64-darwin` 固定。現在は別マシンで使う想定はない。

## Architecture

### 2 層構成

`flake.nix` が 2 つの独立した output を公開する:

- `darwinConfigurations.hank` → `nix/darwin/` （システム層: macOS 設定、フォント、Homebrew）
- `homeConfigurations.rhorii` → `nix/home/` （ユーザー層: シェル、CLI ツール、設定ファイル）

両方に `username` を `specialArgs` / `extraSpecialArgs` 経由で注入しているので、新しいモジュールも `{ username, ... }:` で受け取れる。

### システム層 (`nix/darwin/`)

- `default.nix`: `system.primaryUser`、`fonts.packages`、ユーザー定義
- `homebrew.nix`: GUI アプリ（cask）のみを宣言的に管理
- **`nix.enable = false`** ―― 有効にした場合、不安定になるので、意図的に無効化している
- Homebrew は `onActivation.cleanup = "none"`、`autoUpdate = false`。手元でインストールした未管理 cask を勝手に消さない方針

### ユーザー層 (`nix/home/`)

- `default.nix` が `packages.nix` / `programs/` / `scripts.nix` を import するハブ
- `programs/`: プログラムごとに 1 ファイル（`zsh.nix`、`git.nix`、`starship.nix`、`fzf.nix`、`emacs.nix`）。新しいツールの設定を足すときはここに `<tool>.nix` を作って `programs/default.nix` に import 行を追加する
- `scripts.nix`: `bin/` 配下の AppleScript / シェルスクリプトを `~/bin/` に `home.file` で配置（`executable = true` を忘れない）
- `emacs/init.el` は `programs/emacs.nix` 経由で `~/.emacs.d/init.el` にリンクされる。設定本体はリポジトリ直下の `emacs/` を編集する

### パッケージの置き場所

- **GUI アプリ** → `nix/darwin/homebrew.nix` の `casks` （Nix にパッケージがない場合のみ）
- **CLI ツール** → `nix/home/packages.nix`
- **プログラム単位で設定が必要なもの** → `nix/home/programs/<tool>.nix` で `programs.<tool>.enable = true` を使う

## 注意点

- ルートに `Makefile` / `justfile` / `shell.nix` は存在しない。現在は、タスクランナーは入れず、上記の素の `darwin-rebuild` / `home-manager` コマンドを直接使う運用
