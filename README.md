[![Build](https://github.com/stackbuilders/hapistrano/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/stackbuilders/hapistrano/actions/workflows/build.yml)
[![Draft](https://github.com/stackbuilders/hapistrano/actions/workflows/draft.yml/badge.svg)](https://github.com/stackbuilders/hapistrano/actions/workflows/draft.yml)
[![Release](https://github.com/stackbuilders/hapistrano/actions/workflows/release.yml/badge.svg)](https://github.com/stackbuilders/hapistrano/actions/workflows/release.yml)

# Hapistrano

## Description

Hapistrano is a deployment library for Haskell applications similar to
Ruby's [Capistrano](http://capistranorb.com/).

## Purpose

We created Hapistrano because:

* Deploys should be simple, but as close to atomic as possible (eg,
  they shouldn't require much application downtime).
* Rollback should be trivial to achieve to bring the application back
  to the last-deployed state.
* Deploys shouldn't fail because of dependency problems.

## How it Works

Hapistrano (like Capistrano for Ruby) deploys applications to a new
directory marked with a timestamp on the remote host. It creates this
new directory quickly by placing a git repository for caching purposes
on the remote server.

When the build process completes, it switches a symlink to the `current`
release directory, and optionally restarts the web server.

By default, Hapistrano keeps the last five releases on the target host
filesystem and deletes previous releases to avoid filling up the disk.

## Usage

Hapistrano 0.4.0.0 looks for a configuration file called `hap.yaml` that
typically looks like this:

```yaml
deploy_path: '/var/projects/my-project'
host: user@myserver.com
port: 2222
# To perform version control operations
repo: 'https://github.com/stackbuilders/hapistrano.git'
revision: origin/master
# To copy the contents of the directory
local_directory: '/tmp/my-project'
build_script:
  - stack setup
  - stack build
restart_command: systemd restart my-app-service
```

The following parameters are required:

* `deploy_path` — the root of the deploy target on the remote host.
* Related to the `source` of the repository, you have the following options:
  - _Git repository_ **default** — consists of two parameters. When these are set,
    hapistrano will perform version control related operations.
    **Note:** Only GitHub is supported.
    * `repo` — the origin repository.
    * `revision` — the SHA1 or branch to deploy. If a branch, you will need to
      specify it as `origin/branch_name` due to the way that the cache repo is
      configured.
  * `local_directory` — when this parameter is set, hapistrano will copy the
    contents of the directory.

The following parameters are *optional*:

* `host` — the target host, if missing, `localhost` will be assumed (which
  is useful for testing and playing with `hap` locally). You can specify the
  user that is going to connect to the server here. Example: `user@server.com`.
* `port` — SSH port number to use. If missing, 22 will be used.
* `shell` — Shell to use. Currently supported: `zsh` ans `bash`. If missing, `Bash` will be used.
* `ssh_args` — Optional ssh arguments. Only `-p` is passed via the `port` variable.
* `build_script` — instructions how to build the application in the form of
  shell commands.
* `restart_command` — if you need to restart a remote web server after a
  successful rollback, specify the command that you use in this variable. It
  will be run after both deploy and rollback.
* `vc_action` - Controls if version control related activity should
  take place. It defaults to true. When you don't want activity like
  cloning, fetching etc. to take place, set this to `false`.
* `linux` - Specify, whether or not, the target system where Hapistrano will
  deploy to is a GNU/Linux or other UNIX (g.e. BSD, Mac). This is set to `true`
  by default so unless the target system is not GNU/Linux, this should not be
  necessary. The platform where Hapistrano is running won't affect the
  available options for commands (g.e. A Mac deploying to an Ubuntu machine,
  doesn't need this flag)
* `release_format` - The release timestamp format, the
  '--release-format' argument passed via the CLI takes precedence over this
  value. If neither CLI nor configuration file value is specified, it defaults
  to 'short'
* `keep_releases` - The number of releases to keep, the
  '--keep-releases' argument passed via the CLI takes precedence over this
  value. If neither CLI nor configuration file value is specified, it defaults
  to '5'
* `keep_one_failed` - A boolean specifying whether to keep all failed releases
  or just one (the latest failed release), the '--keep-one-failed' flag passed via
  the CLI takes precedence over this value. If neither CLI nor configuration file value is specified,
  it defaults to false (i.e. keep all failed releases).
* `linked_files:`- Listed files that will be symlinked from the `{deploy_path}/shared` folder
into each release directory during deployment. Can be used for configuration files
that need to be persisted (e.g. dotenv files).  **NOTE:** The directory structure _must_
be similar in your release directories in case you need to link a file inside a
nested directory (e.g. `shared/foo/file.txt`).
* `linked_dirs:`- Listed directories that will be symlinked from the `{deploy_path}/shared` folder
into each release directory during deployment. Can be used for data directories
that need to be persisted (e.g. upload directories). **NOTE:** Do not add a slash `/`
at the end of the directory (e.g. `foo/`) because we use `parseRelFile` to create
the symlink.
* `run_locally:`- Instructions to run locally on your machine in the
  form of shell commands. Example:

```
run_locally:
  - pwd
  - bash deploy.sh
```

Note how we are even able to execute a bash script named `deploy.sh`
above. Be sure to use `set -e` in your bash script to avoid
headaches. Hapistrano will stop the execution on non-zero exit
codes. Without the usage of `set -e`, there is a possibility that your
bash script may return a zero exit code even if your intermediate
command resulted in an error.

After creating a configuration file as above, deploying is as simple as:

```bash
$ hap deploy
```

Rollback is also trivial:

```bash
$ hap rollback # to rollback to previous successful deploy
$ hap rollback -n 2 # go two deploys back in time, etc.
```
* `maintenance_directory:`- The name of the directory on which the maintenance file will be placed. `{deploy_path}/{maintenance_directory}`. The default directory name is `maintenance`
* `maintenance_filename:`- The name of the file that is going to be created in the maintenance_directory. It has to have the `.html` extension to be seen in the browser. `{deploy_path}/{maintenance_directory}/{maintenance_filename}`. The default filename is `maintenance.html`

### Environment Variables

Configuration files are parsed using
[loadYamlSettings](http://hackage.haskell.org/package/yaml-0.10.2.0/docs/Data-Yaml-Config.html#v:loadYamlSettings),
therefore, variable substitution is supported. Considering the following configuration file:

```yaml
revision: "_env:HAPISTRANO_REVISION:origin/master
...
```

The `revision` value could be overwritten as follows:

```sh
HAPISTRANO_REVISION=origin/feature_branch hap deploy
```

## What to do when compiling on server is not viable

Sometimes the target machine (server) is not capable of compiling your
application because e.g. it has not enough memory and GHC exhausts it all.
You can copy pre-compiled files from local machine or CI server using
`copy_files` and `copy_dirs` parameters:

```haskell
copy_files:
  - src: '/home/stackbuilders/my-file.txt'
    dest: 'my-file.txt'
copy_dirs:
  - src: .stack-work
    dest: .stack-work
```

`src` maybe absolute or relative, it's path to file or directory on local
machine, `dest` may only be relative (it's expanded relatively to cloned
repo) and specifies where to put the files/directories on target machine.
Directories and files with clashing names will be overwritten. Directories
are copied recursively.

## Deploying to multiple machines concurrently

Beginning with Hapistrano 0.3.1.0 it's possible to deploy to several
machines concurrently. The only things you need to do is to adjust your
configuration file and use `targets` parameter instead of `host` and `port`,
like this:

```yml
targets:
  - host: myserver-a.com
    port: 2222
  - host: myserver-b.com
# the rest is the same
```

Additionally, starting with 0.4.9.0 it is possible to run commands only on the
lead target during a concurrent deploying process ensuring that certain tasks
only get executed once. The lead target is considered the first entry in the
`targets` list:

```yml
targets:
  - host: app1.example.com # lead server
  - host: app2.example.com

build_script:
  - command: ./run_database_migrations
    only_lead: true
  - ./build
# the rest is the same
```

A few things to note here:

* `host` item is required for every target, but `port` may be omitted and
  then it defaults to `22`.

* The deployment will run concurrently and finish when interactions with all
  targets have finished either successfully or not. If at least one
  interaction was unsuccessful, the `hap` tool will exit with non-zero exit
  code.

* The log is printed in such a way that messages from several machines get
  intermixed, but it's guaranteed that they won't overlap (printing itself
  is sequential) and the headers will tell you exactly which machine was
  executing which command.

If you don't specify `host` and `targets`, `hap` will assume `localhost` as
usually, which is mainly useful for testing.

## Docker

Starting with version `0.4.4.0` all new Docker images would be published to
[GitHub's Container Registry][ghcr], while the old versions remain available on
[Docker Hub][dockerhub]. To download the `latest` version available, change the
image reference as follows:

```diff
- stackbuilders/hapistrano:latest
+ ghcr.io/stackbuilders/hapistrano:latest
```

## GH Actions

Check the documentation [here](.github/workflows/README.md)

## Development

### Requirements

- Install [Zsh](https://www.zsh.org/)
- Use [GHCup][ghcup] to install:
  - GHC 8.10.x or 9.0.x (it is recommended to try both for backward
    compatibility)
  - Cabal 3.x

Alternatively, install only Nix following the instructions detailed
[here](docs/NIX.md).

### Getting Started

Update package index:

```sh
cabal update
```

Enable tests:

```sh
cabal configure --enable-tests
```

Install project dependencies:

```sh
cabal build --only-dependencies
```

Compile the project:

```sh
cabal build
```

Run tests:

```sh
cabal test
```

## Enable/disable maintenance mode

Present a maintenance page to visitors. Disables your application's web interface by writing a {maintenance_filename} file to each web server. The servers must be configured to detect the presence of this file, and if it is present, always display it instead of performing the request.

The maintenance page will just say the site is down for maintenance, and will be back shortly.

To enable maintenance mode run:
```bash
hapistrano maintenance enable
```
Disabling maintenance mode will remove the file from the {maintenance_directory} it can be done with the following command:

```bash
hapistrano maintenance disable
```

## Notes

* Hapistrano is not supported on Windows. Please check: [Issue #96](https://github.com/stackbuilders/hapistrano/issues/96).

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

[ghcup]: https://www.haskell.org/ghcup/
