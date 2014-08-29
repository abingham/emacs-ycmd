emacs-ycmd
==========

emacs-ycmd is a client for [ycmd](https://github.com/Valloric/ycmd),
the code completion system. It takes care of managing a ycmd server
and fetching completions from that server.

emacs-ycmd comprises a core set of functionality for communicating
with ycmd as well as integration with emacs completion frameworks like
[company-mode](http://company-mode.github.io/) and
[auto-complete-mode](http://auto-complete.org/).

Quickstart
----------

The ycmd server generally relies on information provided by a
"configuration file", a small project-specific Python script that it
uses to know how a project is built, configured, etc. This is
typically called ".ycm_extra_conf.py", and it resides with the project
source. Make sure you have one of these; the ycmd project
documentation should describe how to create one.

To use the emacs bindings, put the `.el` from emacs-ycmd files
somewhere that emacs can find them. Then do something like this:

```
(require 'ycmd)

(ycmd-open "/full/path/to/.ycm_extra_conf.py")
(ycmd-display-completions (point))
```

This will try to find possible completions for whatever is at
`point`. This is probably not terribly useful, but it shows how the
system works.

More likely, you'll want to use a completion framework like
`company-mode` to manage the completions for you. Here's how to do
that:

```
(require 'company-ycmd)
(company-ycmd-setup)
```

Afer this you can use your standard `company-mode` keybindings to do completion.
