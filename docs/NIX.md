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
below, it means that Nix is not picking up the substituters we described
earlier and is unable to use them as a derivation cache: 

```
warning: ignoring untrusted substituter 'https://cache.iog.io'
```
in which case
it is recommended to go over the steps detailed in this section again, or look for alternative ways to add extra substituters in [nix.conf](https://nix.dev/manual/nix/2.18/command-ref/conf-file).

## Enabling the development environment

To enable the development environment exposed by the project's [Nix flake](../flake.nix), you can start a development shell by running the following command from within the project's root:

```
nix develop
```

Alternatively, if you are using [nix-direnv](https://github.com/nix-community/nix-direnv) (recommended), you can run:

```
direnv allow
```

that will enable the development shell according to the contents of [.envrc](../.envrc). It is advisable not to run `direnv allow` blindly and always check the contents of [.envrc](../.envrc) files first to ensure nothing malicious is executed.
