## 0.3.0.0

* Add proper set of dependency version constraints.
* Use `optparse-applicative` to parse arguments.
* Add support for comments and empty lines to scripts.
* Parse ssh port from `PORT` environment variable.
* Drop support for GHCs older than 7.10 (because Chris Done's `path` does
  not compile with them, see: https://github.com/chrisdone/path/issues/46).
* Now Hapistrano uses `hap.yaml` file for all its configuration.

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
