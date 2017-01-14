# 1.1 (unreleased)

* Show message for unhandled exceptions in minibuffer
* Fix `ycmd--conditional-parse`'s current-buffer context for some cases (GoTo)
* New option `ycmd-auto-trigger-semantic-completion` for enabling/disabling
  semantic completion after a semantic trigger.
* Add function `ycmd-filter-and-sort-candidates` to allow to use ycmd's
  filtering and sorting mechanism with arbitrary sets of identifiers

# 1.0 (Nov 21, 2016)

* Server start is non-blocking
* Support for mode/language keywords
* Add Emacs menu entry
* Extended company-ycmd support for go, javascript, typescript and rust
* More completer subcommands
* Support for libclang fixits
* View modes for references and fixits
* Eldoc support
* Test suite and builds on travis-ci.org
* etc.

# 0.9 (Jan 9, 2015)

Initial release.

Support for `company-mode` and `flycheck`.
