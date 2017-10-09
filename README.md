# emacs-ycmd

[![MELPA](http://melpa.org/packages/ycmd-badge.svg)](http://melpa.org/#/ycmd)
[![Build Status](https://travis-ci.org/abingham/emacs-ycmd.png?branch=master)](https://travis-ci.org/abingham/emacs-ycmd)

emacs-ycmd is a client for [ycmd](https://github.com/Valloric/ycmd),
the code completion system. It takes care of managing a ycmd server
and fetching completions from that server.

emacs-ycmd comprises a core set of functionality for communicating with ycmd as well as integration with the Emacs completion framework [company-mode](http://company-mode.github.io/).

A lot of the concepts behind emacs-ycmd are actually concepts from
ycmd itself, so if you feel lost you might read
[the ycmd documentation](https://github.com/Valloric/ycmd) and/or the
[the original YouCompleteMe documentation](https://github.com/Valloric/YouCompleteMe).

**Important:** The `ycmd` package itself doesn't provide a real UI for selecting and inserting completions into your files. For that you need to use [`company-ycmd`](#company-ycmd) or another "completion framework".

## Quickstart

First make sure that `ycmd` is installed on your system. See [the ycmd instructions](https://github.com/Valloric/ycmd#building) for more details.

To use `ycmd-mode` in all supported modes, add the following to your emacs config:

```emacs
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
```

Or add `ycmd-mode` to a specific supported mode:

```emacs
(require 'ycmd)
(add-hook 'c++-mode-hook 'ycmd-mode)
```

Use the variable `ycmd-server-command` to specify how to run the server. It will typically be something like:

```emacs
(set-variable 'ycmd-server-command '("python" "/path/to/ycmd/package/"))
```

NB: We do not do filename expansion on the elements of `ycmd-server-command`. As
a result, paths using "~" to represent the home directory will not work
properly; you need to expand them yourself. For example:

```emacs
(set-variable 'ycmd-server-command `("python" ,(file-truename "~/.emacs.d/ycmd/ycmd/")))
```

If you've got a *global ycmd configuration*, specify that in your
emacs configuration by setting `ycmd-global-config`:

```emacs
(set-variable 'ycmd-global-config "/path/to/global_config.py")
```

**Spacemacs users:** Note that if you don't set `ycmd-global-config`, spacemacs
will set it for you. This is not always what you want! See the spacemacs ycmd
documentation for more info.

If you've got project-specific ycmd configurations (almost certainly
called `.ycm_extra_conf.py`), and if you want them automatically
loaded by ycmd as needed (which you probably do), then you can
*whitelist* them by adding entries to `ycmd-extra-conf-whitelist`. For
example, this will allow automatic loading of all `.ycm_extra_conf.py`
files anywhere under `~/my_projects`

```emacs
(set-variable 'ycmd-extra-conf-whitelist '("~/my_projects/*"))
```

Alternatively, you can set `ycmd-extra-conf-handler` to control how
`ycmd.el` deals with non-whitelisted extra configs. By default this is
set to `'ask`, meaning it will ask the user each time one is encountered. The
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

## completion

It is recommended to use `company-mode` for completion, however there is basic support for Emacs' built-in completion mechanism.

``` emacs
(defun ycmd-setup-completion-at-point-function ()
  "Setup `completion-at-point-functions' for `ycmd-mode'."
  (add-hook 'completion-at-point-functions
            #'ycmd-complete-at-point nil :local))

(add-hook 'ycmd-mode-hook #'ycmd-setup-completion-at-point-function)
```

## company-ycmd

[![MELPA](http://melpa.org/packages/company-ycmd-badge.svg)](http://melpa.org/#/company-ycmd)

More likely, you'll want to use a completion framework like
`company-mode` to manage the completions for you. Here's how to do
that:

```emacs
(require 'company-ycmd)
(company-ycmd-setup)
```

After this you can use your standard `company-mode` keybindings to do
completion.

## IMPORTANT: Unbuffered output on Windows

There have been some reports that `ycmd.el` doesn't work on Windows when Python's output is buffered. See, for example, [issue #104](https://github.com/abingham/emacs-ycmd/issues/104). This is because we rely on the ycmd server printing out its host and port information in a timely (i.e. unbuffered) manner. We will almost certainly update the defaults for `ycmd.el` to force unbuffered output.

In any event, if you are facing problems with ycmd not starting and/or hanging Emacs on Windows, try adding `-u` to your `ycmd-server-command`. For example:
```
(set-variable 'ycmd-server-command '("c:/path/to/python.exe" "-u" "c:/path/to/ycmd"))
```

## `flycheck` integration

[![MELPA](http://melpa.org/packages/flycheck-ycmd-badge.svg)](http://melpa.org/#/flycheck-ycmd)

`flycheck-ycmd.el` allows you to use `ycmd` as a backend for
`flycheck`. With this enabled, whenever `ycmd` parses a file the
results will be passed to `flycheck` for display. This is a really
nice way to get quick feedback on problems in your code.

The simple way to enable `flycheck` integration is to use `flycheck-ycmd-setup`:

```emacs
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
```

This will make sure that `flycheck` sees the parse results, and that
the `flycheck-ycmd` backend is enabled.

If for some reason you want to do this manually, the instructions are like this:

```emacs
(require 'flycheck-ycmd)

;; Make sure the flycheck cache sees the parse results
(add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

;; Add the ycmd checker to the list of available checkers
(add-to-list 'flycheck-checkers 'ycmd)
```

### Disabling ycmd-based flycheck for specific modes

If you use `flycheck-ycmd-setup` or otherwise put `ycmd` at the front of
`flycheck-checkers`, flycheck will use the ycmd checker for every buffer in
`ycmd-mode`. This may not be what you want. For example, even though ycmd
supports completion (and, thus, flycheck) for Python, you may wish to use
pyflakes for flychecking Python code.

To disable ycmd-based flychecking for specific modes, you can modify
the `flycheck-disabled-checkers` list in your mode hook. For example:

```
(add-hook 'python-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
```

With this, the ycmd checker will be ignored in `python-mode`. Since
`flycheck-disabled-checkers` is buffer-local, the ycmd-based checker
will still be available for other modes.

### Making flycheck and company work together

In some cases you may see that `company` and `flycheck` interfere with one another. You can end up with strange completion artifacts in your buffers. This mostly seems to happen when you run emacs in "terminal mode", i.e. with `emacs -nw`.

The short answer for how to deal with this is:
```
(setq flycheck-indication-mode nil)
```

The slightly longer and probably better answer is:
```
(when (not (display-graphic-p))
  (setq flycheck-indication-mode nil))
```

For a full explanation see [the `emacs-ycmd` defect related to this](https://github.com/abingham/emacs-ycmd/issues/144) as well as [the root `flycheck` issue](https://github.com/flycheck/flycheck/issues/526).

## `eldoc` integration

`ycmd-eldoc` adds eldoc support for `ycmd-mode` buffers.

``` emacs-lisp
(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
```

Note: eldoc messages will only be shown for functions which are retrieved via semantic completion.


## `next-error` integration

emacs-ycmd reports found errors through emacs buttons; to integrate those with
`next-error` prepend something like
`(require 'ycmd-next-error)` before require'ing ycmd (after adding the
`contrib` directory to your `load-path`).

## Making `emacs-ycmd` quieter

In some common configurations `emacs-ycmd` can produce lots of messages, and
some people find these noisy and distracting. If you're seeing a lot of messages
like `Contacting host: 127.0.0.1:NNNNN` and you'd like to quiet them, set
`url-show-status` to `nil`. This can effect non-ycmd-related buffers, so
consider using buffer-local settings if this worries you.

You might also see a flurry of messages like this:

```
REQUEST [error] Error (error) while connecting to http://127.0.0.1:38987/completions.
REQUEST [error] Error (error) while connecting to http://127.0.0.1:38987/event_notification. [26 times]
```

These almost never indicate something you need to be concerned about. To quiet
them, you can set `request-message-level` to `-1`.

See [issue #173](https://github.com/abingham/emacs-ycmd/issues/174) for the
initial discussion of this topic.

## Running tests

`emacs-ycmd` comes with a number of tests that you can run. This is mostly
useful for developers. They are built with `ert`, so you can run them using any
technique that `ert` provides. For example:

```emacs
(require 'ycmd-test)
(ert-run-tests-interactively "ycmd-test")
```

It is also possible to run the tests on the command-line with the Makefile
provided in this repository. Before running test, you need to install the
[Cask](http://cask.github.io/) in order to be able to install the package
dependencies.

You can do this by running

``` shell
make deps
```

The other thing that is required is to have the `ycmd` folder right next to
`emacs-ycmd` (`../ycmd`).

To run the tests:

``` shell
make test
```

It is also possible to have the `ycmd` server at a different location. In that
case the path needs to be passed to the `make` command explicitly:

``` shell
make YCMDPATH='/path/to/ycmd/ycmd' test
```

Make sure that you provide the path to the ycmd module and not the ycmd root
directory.
