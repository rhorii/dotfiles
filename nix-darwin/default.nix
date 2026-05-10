{
  username,
  ...
}:
{
  system = {
    stateVersion = 6;
    primaryUser = username;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";

  # 有効にすると nix-darwin が /etc/nix/nix.conf や nix-daemon の launchd plist を
  # 管理するようになるが、この環境では不安定になるため意図的に無効化している。
  # Nix 自体の管理 (nix.conf / GC / アップグレードなど) は外部のインストーラー側に委ねる。
  nix.enable = false;

  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };

  imports = [
    ./fonts.nix
    ./homebrew.nix
  ];
}
