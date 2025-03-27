# Nix

## Requirements

- Install [Nix](https://nixos.org/download.html)
- Enable [Flakes](https://nixos.wiki/wiki/Flakes#Permanent) permanently
- Install [devenv](https://devenv.sh/getting-started/):

## Project Structure

The project uses:
- Nix Flakes for reproducible builds and development environments
- `stacklock2nix` for deriving Nix packages from stack.yaml.lock
- `devenv` for creating consistent development environments

### Flake.nix

The flake.nix file has the following inputs:
- `nixpkgs`: Standard Nix packages repository
- `stacklock2nix`: A tool for generating Nix packages from stack.yaml.lock files

The flake outputs:
- `packages.default`: The Hapistrano package for each supported system
- `overlays.default`: An overlay for integrating Hapistrano into other Nix
  systems

## Development Environments

The project includes a `devenv.nix` configuration that provides a consistent
development environment with all necessary dependencies.

To use devenv:

```
devenv shell
```

The devenv configuration includes:
- Essential tools like git, stack, and zsh
- System-specific dependencies (e.g., gmp for Linux systems)

### Using nix-direnv

If you are using [nix-direnv](https://github.com/nix-community/nix-direnv), run:

```
direnv allow
```

This will enable the development shell according to the contents of
[.envrc](../.envrc). Always check the contents of [.envrc](../.envrc) files
before running `direnv allow` to ensure nothing malicious is executed.
