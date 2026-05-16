{ ... }:
{
  homebrew.enable = true;

  homebrew.onActivation.autoUpdate = false;

  # "none"      - 未管理の cask はそのまま残す
  # "uninstall" - リストにない cask を削除する
  # "zap"       - リストにない cask を完全削除する（破壊的）
  homebrew.onActivation.cleanup = "none";

  homebrew.casks = [
    "anki"
    "claude"
    "emacs-mac"
    "firefox"
    "google-chrome"
    "karabiner-elements"
    "netnewswire"
    "notion"
    "obsidian"
    "raycast"
    "visual-studio-code"
    "zed"
  ];
}
