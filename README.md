emacs-ycmd
==========

emacs-ycmd is a client for [ycmd](https://github.com/Valloric/ycmd),
the code completion system. It takes care of managing a ycmd server
and fetching completions from that server.

emacs-ycmd comprises a core set of functionality for communicating
with ycmd as well as integration with emacs completion frameworks like
[company-mode](http://company-mode.github.io/) and
[auto-complete-mode](http://auto-complete.org/).

A lot of the concepts behind emacs-ycmd are actually concepts from
ycmd itself, so if you feel lost you might read
[the ycmd documentation](https://github.com/Valloric/ycmd) and/or the
[the original YouCompleteMe documentation](https://github.com/Valloric/YouCompleteMe).

Quickstart
----------

First make sure that `ycmd` is installed on your system. See [the ycmd instructions](https://github.com/Valloric/ycmd#building) for more details.

To use `ycmd-mode` in all supported modes, add the following to your emacs config:

```
(require 'ycmd)
(ycmd-setup)
```

Or add `ycmd-mode` to a specific supported mode:

```
(require 'ycmd)
(add-hook 'c++-mode-hook 'ycmd-mode)
```

Use the variable `ycmd-server-command` to specify how to run the server. It will typically be something like:

```
(set-variable 'ycmd-server-command '("python" "/path/to/ycmd/package"))
```

If you've got a *global ycmd configuration*, specify that in your
emacs configuration by setting `ycmd-global-config`:

```
(set-variable 'ycmd-global-config "/path/to/global_config.py")
```

If you've got project-specific ycmd configurations (almost certainly
called `.ycm_extra_conf.py`), and if you want them automatically
loaded by ycmd as needed (which you probably do), then you can
*whitelist* them by adding entries to `ycmd-extra-conf-whitelist`. For
example, this will allow automatic loading of all `.ycm_extra_conf.py`
files anywhere under `~/my_projects`

```
(set-variable 'ycmd-extra-conf-whitelist '("~/my_projects/*"))
```

Alternatively, you can set `ycmd-extra-conf-handler` to control how
`ycmd.el` deals with non-whitelisted extra configs. By default this is
set to `'ask`, it will ask the user each time one is encountered. The
other options are `'ignore`, in which case the extra config will be
ignored, and `'load`, in which case the extra config will be loaded.

Now a ycmd server will be automatically launched whenever it's
needed. Generally, this means whenever you visit a file with a
supported major mode. You should not normally need to manually start
or stop a ycmd server.

With a server running, you can now get completions for a point in a
file using `ycmd-get-completions`. This doesn't actually insert the
completions; it just fetches them from the server. It's not even an
interactive function, so you can't really call it while editing. If
you just want to see the possible completions at a point, you can try
`ycmd-display-completions` which will dump a raw completion struct
into a buffer. This is more of a debugging tool than anything.

More likely, you'll want to use a completion framework like
`company-mode` to manage the completions for you. Here's how to do
that:

```emacs
(require 'company-ycmd)
(company-ycmd-setup)
```

After this you can use your standard `company-mode` keybindings to do
completion.

`flycheck` integration
--------------------------

`flycheck-ycmd.el` allows you to use `ycmd` as a backend for
`flycheck`. With this enabled, whenever `ycmd` parses a file the
results will be passed to `flycheck` for display. This is a really
nice way to quick feedback on problems in your code.

The simple way to enable `flycheck` integration is to use `flycheck-ycmd-setup`:

```
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
```

This will make sure that `flycheck` sees the parse results, and that
the `flycheck-ycmd` backend is enabled.

If for some reason you want to do this manually, the instructions are like this:

```
(require 'flycheck-ycmd)

;; Make sure the flycheck cache sees the parse results
(add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

;; Add the ycmd checker to the list of available checkers
(add-to-list 'flycheck-checkers 'ycmd)
```

`next-error` integration
----------------------------
emacs-ycmd reports found errors through emacs buttons; to integrate those with
`next-error` prepend something like
`(require 'ycmd-next-error)` before require'ing ycmd (after adding the
`contrib` directory to your `load-path`).

Running tests
-------------

`emacs-ycmd` comes with a number of tests that you can. This is mostly
useful for developers. They are built with `ert`, so you can run them
using any technique that `ert` provides. For example:

```
(require 'ycmd-test)
(ert-run-tests-interactively "ycmd-test")
```
