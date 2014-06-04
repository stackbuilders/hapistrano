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

When the build process completes, it switches a symlink to the
'current' release directory, and optionally restarts the web server.

By default, Hapistrano keeps the last five releases on the target host
filesystem and deletes previous releases to avoid filling up the disk.

## Usage

The deploy requires the following environment variables:

* DEPLOY_PATH - The root of the deploy target on the remote host
* HOST - The target host
* REPOSITORY - The origin repository
* REVISION - The SHA1 or branch to deploy. If a branch, you will need
  to specify it as origin/branch_name due to the way that the cache
  repo is configured.

The following environment variables are *optional* and affect the
deploy process:

* BUILD_SCRIPT - The local path to a file that should be executed on
  the remote server to build the application. The script isn't
  executed verbatim - instead, every line is joined with `&&` so that
  the script aborts if any component fails. See a sample script for a
  clean build of a Haskell/Cabal application in this project under
  [script/clean-build.sh].
* RESTART_COMMAND - If you need to restart a remote web server after a
  successful deploy, specify the command that you use in this
  variable. It will be run after both deploy and rollback.

You may want to save the environment variables that you need for your
deploy in a shell script that you `source` before deploy. Make sure
you `export` these variables so that they're available in the shell
after you run the script. For example, you could use the following to
configure your deploy:

    export DEPLOY_PATH="/var/project"
    export HOST="my-app-staging"
    export REPOSITORY="git@github.com:yourorg/yourrepo.com.git"
    export REVISION="origin/staging"
    export BUILD_SCRIPT="/home/you/Code/hapistrano/script/clean-build.sh"
    export RESTART_COMMAND="echo Replace me with your restart command"

# License

MIT, see [the LICENSE file](LICENSE).

# Contributing

Pull requests for modifications to this program are welcome. Fork and
open a PR. Feel free to [email me](mailto:justin@stackbuilders.com) if
you have questions about what may be accepted before working on a PR.

If you're looking for a place to start, you may want to check the
[open issue](https://github.com/stackbuilders/hapistrano/issues).
