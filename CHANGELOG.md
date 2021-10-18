## 0.4.3.1
### Added
* Add support for aeson 2.0

## 0.4.3.0
### Added
* Add support for GHC 9.0
* Docker image is built on a newer compiler, cabal and alpine version
### Removed
* Official support for GHC versions older than 8.6

## 0.4.2.0
### Added
* Add support for working directory

### Removed
* GHC support for versions older than 8.0. Bounds for base corrected

## 0.4.1.4
### Changed
* Bump path version upper constraint to 0.9

## 0.4.1.3
### Changed
* Allow formatting-7.0

## 0.4.1.2
### Changed
* Allow optparse-applicative-0.16.0.0

## 0.4.1.1
### Changed
* Allow `ansi-terminal` 0.11

## 0.4.1.0
### Added
* Support for GHC 8.10
* Support for aeson-1.5

## 0.4.0.1
### Changed
* Allow `time` 1.10
* Correct the package to reenable Hapistrano in Stackage

## 0.4.0.0
### Added
* Copy a directory's contents with `local_directory` instead of using _git_ with `repo` and `revision`.

### Changed
* Update upper bounds for `path` and `path-io` packages.

## 0.3.10.1
### Added
* Update Dockerfile and maintainer.

## 0.3.10.0
### Added
* Colorize the output in the terminal.

## 0.3.9.4
### Added
* Support for GHC 8.8
* Support for ssh args in the config file.

## 0.3.9.3
### Changed
* Support for optparse-applicative-0.15
* Replace deprecated function "withProcess" to "withProcessTerm"
  and add the version of "typed-process-0.2.6.0" as extra dependency.

## 0.3.9.2
### Changed
* Update Docker base image from alpine:3.7 to alpine:3.9

## 0.3.9.1
### Added
* Add timestamp to output commands:
```
[16:29:58,  2019-01-23 (-05)] INFO -- : $ find /tmp/hapistrano/releases/ -maxdepth 1 -type d
/tmp/hapistrano/releases/
/tmp/hapistrano/releases/20190123212933
```

## 0.3.9.0
### Added
* Support to deploy to a host that has default `zsh` shell.
* Support to deploy using a different shell. Currently supported: `zsh` and `bash`.
* `linked_files` and `linked_dirs` to link files and directories located in the
`{deploy_path}/shared/` directory.

## 0.3.8.0
### Added
* `execWithInheritStdout` was added to `System.Hapistrano.Core` to stream output children's
to the parent's `stdout`.

### Changed
* `playScript` and `playScriptLocally` use `execWithInheritStdout` to stream children's
stdout to parent's stdout.

## 0.3.7.0
* Read `release-format` and `keep-releases` from the configuration file.

## 0.3.6.1
* Loose upper bound for yaml 0.11

## 0.3.6.0
* Add support to interpolate ENV variables in a configuration file.
* Add support for GHC 8.6.1
* Loose constraint for stm-2.5.0.0

## 0.3.5.10
* Updated upper bound for yaml 0.10

## 0.3.5.9
* Loose upper bound for path-io 1.4

## 0.3.5.8
* Loose upper bound for yaml 0.9

## 0.3.5.7
* Loose upper bound for aeson 1.4

## 0.3.5.6
* Add Dockerfile

## 0.3.5.5
* Adding tested compatibility with GHC 8.4

## 0.3.5.4
* Support for temporary 1.3

## 0.3.5.3
* Support for aeson 1.3

## 0.3.5.2
* Loose uppers bounds for async

## 0.3.5.1
* Standarize style
* When showing version information also show git branch and commit

## 0.3.5.0
* Add support for deploying to other Unix systems, besides GNU/Linux which
  didn't supported all the flags that Hapistrano was using. See issue #63

## 0.3.4.0
* Use `git checkout` instead of `git reset` to set the release revision

## 0.3.3.0

* Correct bounds for base. GHC support for versions older than 7.10 was dropped on 0.3.0.0
* Add `run_locally` to run user defined commands locally before deployment. Thanks to Sibi (GitHub: psibi) for this contribution

## 0.3.2.4

* Allow time 1.8
* Allow process 1.6

## 0.3.2.3

* Allow path-io 1.3

## 0.3.2.2

* Allow optparse-applicative 0.14

## 0.3.2.1

* Add support for help in subcommands. Thanks to Vanessa McHale (GitHub: vmchale) for this contribution

## 0.3.2.0

* Fix `-v` switch for hap. Thanks to Sibi (GitHub: psibi) for this contribution
* Add `vc_action` to control version control related tasks. Thanks to Sibi (GitHub: psibi) for this contribution

## 0.3.1.0

* Fixed a bug with repos not being fetched properly.
* Implemented concurrent deployment to multiple hosts.
* Now completion tokens are dropped automatically like old releases.

## 0.3.0.1

* Reduced verbosity of some commands to make reading logs easier.
* Restart command is now invoked after activation of new release (as it
  should).
* Fix a typo in flag that specifies SSH port for `scp`.
* Ensure that containing directories for files and directories to copy
  exist before invoking `scp`.

## 0.3.0.0

* Add proper set of dependency version constraints.
* Use `optparse-applicative` to parse arguments.
* Allow to specify non-standard SSH port.
* Drop support for GHCs older than 7.10 (because Chris Done's `path` does
  not compile with them, see: https://github.com/chrisdone/path/issues/46).
* Now Hapistrano uses `hap.yaml` file for all its configuration.
* Added the ability to copy arbitrary files and directories verbatim from
  local machine to target host.

## 0.2.1.2

* Add change log (#23).
* Add `README.md` to extra source files.
* Handle missing environment variables more graciously.
* Allow GHC 8 and base 4.9.

## 0.2.1.1

* Fix tests (#31).

## 0.2.1

* Use Stack (#17).
* Clean up package (#20).
* Fix tests (#25).

## 0.2.0.2

* GHC 7.10 support.

## 0.2.0.1

* Refactoring and documentation improvements.

## 0.2.0.0

* Various refactoring and relaxed dependency constraints.

## 0.1.0.2

* Print error messages to `stderr`, return non-zero exit code on failure.

## 0.1.0.1

* Initial release.
