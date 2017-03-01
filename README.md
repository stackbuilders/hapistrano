[![Build Status](https://travis-ci.org/stackbuilders/hapistrano.svg?branch=master)](https://travis-ci.org/stackbuilders/hapistrano) [![Hackage version](https://img.shields.io/hackage/v/hapistrano.svg)](http://hackage.haskell.org/package/hapistrano)

# Hapistrano

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

Hapistrano 0.3.0.0 looks for a configuration file called `hap.yaml` that
typically looks like this:

```yaml
deploy_path: '/var/projects/my-project'
host: myserver.com
port: 2222
repo: 'https://github.com/stackbuilders/hapistrano.git'
revision: origin/master
build_script:
  - stack setup
  - stack build
restart_command: systemd restart my-app-service
```

The following parameters are required:

* `deploy_path` — the root of the deploy target on the remote host.
* `repo` — the origin repository.
* `revision` — the SHA1 or branch to deploy. If a branch, you will need to
  specify it as `origin/branch_name` due to the way that the cache repo is
  configured.

The following parameters are *optional*:

* `host` — the target host, if missing, `localhost` will be assumed (which
  is useful for testing and playing with `hap` locally).
* `port` — SSH port number to use. If missing, 22 will be used.
* `build_script` — instructions how to build the application in the form of
  shell commands.
* `restart_command` — if you need to restart a remote web server after a
  successful rollback, specify the command that you use in this variable. It
  will be run after both deploy and rollback.

After creating a configuration file as above, deploying is as simple as:

```bash
$ hap deploy
```

Rollback is also trivial:

```bash
$ hap rollback # to rollback to previous successful deploy
$ hap rollback -n 2 # go two deploys back in time, etc.
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

```haskell
targets:
  - host: myserver-a.com
    port: 2222
  - host: myserver-b.cmo
# the rest is the same…
```

A few things to note here:

* `host` item is required for every target, but `port` may be omitted and
  then it defaults to `22`.

* The deployment will run concurrently and finish when interactions with all
  targets have finished either successfully or not. If at least one
  interaction was unsuccessful, the `hap` tool will exit with non-zero exit
  code.

* The log is printed is such a way that messages from several machines get
  intermixed, but it's guaranteed that they won't overlap (printing itself
  is sequential) and the headers will tell you exactly which machine was
  executing which command.

If you don't specify `host` and `targets`, `hap` will assume `localhost` as
usually, which is mainly useful for testing.

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Pull requests for modifications to this program are welcome. Fork and
open a PR. Feel free to [email me](mailto:justin@stackbuilders.com) if
you have questions about what may be accepted before working on a PR.

If you're looking for a place to start, you may want to check the
[open issue](https://github.com/stackbuilders/hapistrano/issues).
