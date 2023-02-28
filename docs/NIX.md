# Nix

## Requirements

Install Nix following the instructions detailed
[here](https://nixos.org/download.html).

Enable Flakes permanently:

```sh
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

**For macOS users**

Add the following lines to configuration file located at `/etc/nix/nix.conf`:

```
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://cache.iog.io https://cache.nixos.org
```

Restart the `nix-daemon` service:

```
sudo launchctl kickstart -k org.nixos.nix-daemon
```

If the following messages appear running the scripts detailed in the section
below, it means that the Nix is not able to talk with the cache, in which case
is recommended to go over the steps detailed in this section again:

```
warning: ignoring untrusted substituter 'https://cache.iog.io'
```

TODO: direnv

## Switching between different GHC versions

Spawn a new shell with GHC 9.0:

```sh
./bin/ghc90
```

Or with GHC 8.10:

```sh
./bin/ghc810
```

Within the shell, all commands detailed in the [Getting
Started](../README.md#getting-started) section should work as expected.

Alternatively, to run those as inline commands use the `-c` option:

```sh
./bin/ghc90 -c <command>
```
