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
