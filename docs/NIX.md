# Nix

## Requirements

- Install [Nix](https://nixos.org/download.html)
- Enable Flakes [permanently](https://nixos.wiki/wiki/Flakes#Permanent)

**For macOS users**

Add the following lines to configuration file located at `/etc/nix/nix.conf`:

```
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://cache.iog.io https://cache.nixos.org
```

Restart the `nix-daemon` service:

```
sudo launchctl stop org.nixos.nix-daemon
sudo launchctl start org.nixos.nix-daemon
```

If the following messages appear running the scripts detailed in the section
below, it means that the Nix is not able to talk with the cache, in which case
is recommended to go over the steps detailed in this section again:

```
warning: ignoring untrusted substituter 'https://cache.iog.io'
```

## Switching between different GHC versions

The `bin` directory, holds all available shells:

```
ls bin | grep ghc
```

To spawn a new shell with a pre-defined GHC version, choose one of the scripts
listed above and run the following command:

```sh
./bin/ghc90
```

Once inside the shell, verify the GHC version matches the script name:

```
ghc --version
```

After that, all commands detailed in the [Getting
Started](../README.md#getting-started) section should work the same.

Alternatively, to run a command without spawning a new shell use the `-c`
option:

```sh
./bin/ghc90 -c <command>
```
