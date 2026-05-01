# dotfiles

## Setup

### Prerequisites

1. Install [Nix](https://nixos.org/download/)
2. Install [Homebrew](https://brew.sh)

### Initial setup

```bash
# macOS system configuration (first time)
nix run nix-darwin -- switch --flake .

# Shell (first time)
nix run home-manager -- switch --flake .
```

## Usage

### macOS system configuration

```bash
darwin-rebuild switch --flake .
```

### Shell

```bash
home-manager switch --flake .
```

### Update

```bash
nix flake update
```

After updating, run `darwin-rebuild switch` and `home-manager switch` to apply the changes.
