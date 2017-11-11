# 1.3 (unreleased)

* Improve regexp for extracting the return type of c-family functions from
  function signature
* If `ycmd-bypass-url-proxy-services` is non-nil, bypass also proxies when
  using `curl` as `request-backend`
* Add defcustom `ycmd-mode-line-prefix` for customizing the mode line appearance
  of ycmd

# 1.2 (Jun 14, 2017)

* New `ycmd-eldoc-mode`.
* `ycmd-eldoc` mode retrieves eldoc message asynchronously.
* `ycmd-eldoc` sends a `GetType` request as fallback if the completion
  request has not returned anything.
* `ycmd-eldoc` gets automatically disabled if there is not semantic completer
  support for major mode. This prevents error messages in minibuffer.
* `*ymcd-fixits*` buffer can now also show responses from `RefactorRename`
  requests
* Add `ycmd-completer` command and new variable `ycmd-completing-read-function`
* Require Emacs 24.4 as minimum version
* Use `GetTypeImprecise` as fallback in `ycmd-eldoc` instead of `GetType` if
  supported by filetype completer

# 1.1 (Mar 29, 2017)

* Show message for unhandled exceptions in minibuffer
* Fix `ycmd--conditional-parse`'s current-buffer context for some cases (GoTo)
* New option `ycmd-auto-trigger-semantic-completion` for enabling/disabling
  semantic completion after a semantic trigger.
* Add function `ycmd-filter-and-sort-candidates` to allow to use ycmd's
  filtering and sorting mechanism with arbitrary sets of identifiers
* Enable semantic diagnostics for typescript

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
