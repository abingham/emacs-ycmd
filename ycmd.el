;;; ycmd.el --- emacs bindings to the ycmd completion server -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.9.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((emacs "24") (f "0.17.1") (dash "1.2.0") (deferred "0.3.2") (popup "0.5.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd is a modular server that provides completion for C/C++/ObjC
;; and Python, among other languages. This module provides an emacs
;; client for that server.
;;
;; ycmd is a bit peculiar in a few ways. First, communication with the
;; server uses HMAC to authenticate HTTP messages. The server is
;; started with an HMAC secret that the client uses to generate hashes
;; of the content it sends. Second, the server gets this HMAC
;; information (as well as other configuration information) from a
;; file that the server deletes after reading. So when the code in
;; this module starts a server, it has to create a file containing the
;; secret code. Since the server deletes this file, this code has to
;; create a new one for each server it starts. Hopefully by knowing
;; this, you'll be able to make more sense of some of what you see
;; below.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'ycmd)
;;   (ycmd-setup)
;;
;; Basic usage:
;;
;; First you'll want to configure a few things. If you've got a global
;; ycmd config file, you can specify that with `ycmd-global-config':
;;
;;   (set-variable 'ycmd-global-config "/path/to/global_conf.py")
;;
;; Then you'll want to configure your "extra-config whitelist"
;; patterns. These patterns determine which extra-conf files will get
;; loaded automatically by ycmd. So, for example, if you want to make
;; sure that ycmd will automatically load all of the extra-conf files
;; underneath your "~/projects" directory, do this:
;;
;;   (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*"))
;;
;; Now, the first time you open a file for which ycmd can perform
;; completions, a ycmd server will be automatically started.
;;
;; When ycmd encounters an extra-config that's not on the white list,
;; it checks `ycmd-extra-conf-handler' to determine what to do. By
;; default this is set to `ask', in which case the user is asked
;; whether to load the file or ignore it. You can also set it to
;; `load', in which case all extra-confs are loaded (and you don't
;; really need to worry about `ycmd-extra-conf-whitelist'.) Or you can
;; set this to `ignore', in which case all extra-confs are
;; automatically ignored.
;;
;; Use `ycmd-get-completions' to get completions at some point in a
;; file. For example:
;;
;;   (ycmd-get-completions buffer position)
;;
;; You can use `ycmd-display-completions' to toy around with completion
;; interactively and see the shape of the structures in use.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'dash)
(require 'deferred)
(require 'f)
(require 'hmac-def)
(require 'json)
(require 'popup)
;; (require 'request)
;; (require 'request-deferred)
(require 'etags)

;; Allow loading of our bundled third-party modules
(add-to-list 'load-path (f-join (f-dirname (f-this-file)) "third-party"))

(require 'ycmd-request)
(require 'ycmd-request-deferred)

(defgroup ycmd nil
  "a ycmd emacs client"
  :group 'tools
  :group 'programming)

(defcustom ycmd-global-config nil
  "Path to global extra conf file."
  :type '(string)
  :group 'ycmd)

(defcustom ycmd-extra-conf-whitelist nil
  "List of glob expressions which match extra configs to load as
  needed without confirmation."
  :type '(repeat string)
  :group 'ycmd)

(defcustom ycmd-extra-conf-handler 'ask
  "What to do when an un-whitelisted extra config is encountered.

Options are:

`load'
      Automatically load unknown extra confs.

`ignore'
     Ignore unknown extra confs and do not load them.

`ask'
     Ask the user for each unknown extra conf.
"
  :group 'ycmd
  :type '(set (const :tag "Load unknown extra confs" load)
              (const :tag "Ignore unknown extra confs" ignore)
              (const :tag "Ask the user" ask))
  :risky t)

(defcustom ycmd-host "127.0.0.1"
  "The host on which the ycmd server is running."
  :type '(string)
  :group 'ycmd)

; TODO: Figure out the best default value for this.
(defcustom ycmd-server-command '("python" "/path/to/ycmd/package/")
  "The ycmd server program command.

Note that the default value for this variable is intentionally
incorrect. You will almost certainly need to set it to match your
system installation.
"
  :type '(repeat string)
  :group 'ycmd)

(defcustom ycmd-server-args '("--log=debug"
                              "--keep_logfile"
                              "--idle_suicide_seconds=10800")
  "Extra arguments to pass to the ycmd server."
  :type '(repeat string)
  :group 'ycmd)

(defcustom ycmd-file-parse-result-hook nil
  "Functions to run with file-parse results.

Each function will be called with with the results returned from
ycmd when it parses a file in response to
/event_notification. See `ycmd--with-destructured-parse-result'
for some insight into what this structure is shaped like."
  :group 'ycmd
  :type 'hook
  :risky t)

(defcustom ycmd-idle-change-delay 0.5
  "Number of seconds to wait after buffer modification before
re-parsing the contents."
  :group 'ycmd
  :type '(number)
  :safe #'numberp)

(defcustom ycmd-keepalive-period 30
  "Number of seconds between keepalive messages."
  :group 'ycmd
  :type '(number))

(defcustom ycmd-parse-conditions '(save new-line mode-enabled)
  "When ycmd should reparse the buffer.

The variable is a list of events that may trigger parsing the
buffer for new completion:

`save'
      Set buffer-needs-parse flag after the buffer was saved.

`new-line'
      Set buffer-needs-parse flag immediately after a new
      line was inserted into the buffer.

`idle-change'
      Set buffer-needs-parse flag a short time after a
      buffer has changed.  (See `ycmd-idle-change-delay')

`mode-enabled'
      Set buffer-needs-parse flag after `ycmd-mode' has been
      enabled.

If nil, never set buffer-needs-parse flag.  For a manual reparse,
use `ycmd-parse-buffer'."
  :group 'ycmd
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After a buffer was changed and idle" idle-change)
              (const :tag "After a `ycmd-mode' was enabled" mode-enabled))
  :safe #'listp)

(defcustom ycmd-default-tags-file-name "tags"
  "The default tags file name."
  :group 'ycmd
  :type 'string)

(defcustom ycmd-force-semantic-completion nil
  "Whether to use always semantic completion."
  :group 'ycmd
  :type 'boolean)

(defcustom ycmd-tag-files nil
  "Whether to collect identifiers from tags file.

nil
    Do not collect identifiers from tag files.

`auto'
    Look up directory hierarchy for first found tags file with
    `ycmd-default-tags-file-name'.

string
    A tags file name.

list
    A list of tag file names."
  :group 'ycmd
  :type '(choice (const :tag "Don't use tag file." nil)
                 (const :tag "Locate tags file automatically" auto)
                 (string :tag "Tag file name")
                 (repeat :tag "List of tag files"
                         (string :tag "Tag file name")))
  :safe (lambda (obj)
          (or (symbolp obj)
              (stringp obj)
              (ycmd--string-list-p obj))))

(defcustom ycmd-file-type-map
  '((c++-mode . ("cpp"))
    (c-mode . ("cpp"))
    (caml-mode . ("ocaml"))
    (csharp-mode . ("cs"))
    (d-mode . ("d"))
    (erlang-mode . ("erlang"))
    (go-mode . ("go"))
    (js-mode . ("javascript"))
    (js2-mode . ("javascript"))
    (lua-mode . ("lua"))
    (objc-mode . ("objc"))
    (perl-mode . ("perl"))
    (cperl-mode . ("perl"))
    (php-mode . ("php"))
    (python-mode . ("python"))
    (ruby-mode . ("ruby"))
    (scala-mode . ("scala"))
    (tuareg-mode . ("ocaml")))
  "Mapping from major modes to ycmd file-type strings.

Used to determine a) which major modes we support and b) how to
describe them to ycmd."
  :group 'ycmd
  :type '(alist :key-type symbol :value-type (repeat string)))

(defcustom ycmd-max-num-identifier-candidates 10
  "The maximum number of identifier completion results."
  :group 'ycmd
  :type 'integer)

(defcustom ycmd-gocode-binary-path (executable-find "gocode")
  "Gocode binary path."
  :group 'ycmd
  :type 'string)

(defcustom ycmd-global-modes t
  "Modes for which `ycmd-mode' is turned on by `global-ycmd-mode'.

If t, ycmd mode is turned on for all major modes in
`ycmd-file-type-map'.  If set to all, ycmd mode is turned on
for all major-modes.  If a list, ycmd mode is turned on for all
`major-mode' symbols in that list.  If the `car' of the list is
`not', ycmd mode is turned on for all `major-mode' symbols _not_
in that list.  If nil, ycmd mode is never turned on by
`global-ycmd-mode'."
  :group 'ycmd
  :type '(choice (const :tag "none" nil)
                 (const :tag "member in `ycmd-file-type-map'" t)
                 (const :tag "all" all)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode")))))

(defconst ycmd--diagnostic-file-types
  '("c"
    "cpp"
    "objc"
    "objcpp"
    "cs")
  "A list of ycmd file type strings which support semantic completion.")

(defun ycmd-diagnostic-file-types (mode)
  "Find the ycmd file types for MODE which support semantic completion.

Returns a possibly empty list of ycmd file type strings.  If this
is empty, then ycmd doesn't support semantic completion (or
diagnostics) for MODE."
  (-intersection
   ycmd--diagnostic-file-types
   (ycmd-major-mode-to-file-types mode)))

(defun ycmd-open ()
  "Start a new ycmd server.

This kills any ycmd server already running (under ycmd.el's
control.) The newly started server will have a new HMAC secret."
  (interactive)

  (ycmd-close)

  (let ((hmac-secret (ycmd--generate-hmac-secret)))
    (ycmd--start-server hmac-secret)
    (setq ycmd--hmac-secret hmac-secret))

  (ycmd--start-keepalive-timer))

(defun ycmd-close ()
  "Shutdown any running ycmd server.

This does nothing if no server is running."
  (interactive)

  (unwind-protect
      (when (ycmd-running?)
        (delete-process ycmd--server-process)))

  (ycmd--kill-notification-timer))

(defun ycmd-running? ()
  "Tells you if a ycmd server is already running."
  (interactive)
  (if (get-process ycmd--server-process) 't nil))

(defun ycmd--keepalive ()
  "Sends an unspecified message to the server.

This is simply for keepalive functionality."
  (ycmd--request "/healthy" '() :type "GET"))

(defun ycmd-load-conf-file (filename)
  "Tell the ycmd server to load the configuration file FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (let ((filename (expand-file-name filename)))
    (ycmd--request
     "/load_extra_conf_file"
     `(("filepath" . ,filename)))))

(defun ycmd-display-completions ()
  "Get completions at the current point and display them in a buffer.

This is really a utility/debugging function for developers, but
it might be interesting for some users."
  (interactive)
  (deferred:$
    (ycmd-get-completions (current-buffer) (point))
    (deferred:nextc it
      (lambda (completions)
        (pop-to-buffer "*ycmd-completions*")
        (erase-buffer)
        (insert (pp-to-string completions))))))

(defun ycmd-toggle-force-semantic-completion ()
  "Toggle whether to use always semantic completion.

Returns the new value of `ycmd-force-semantic-completion`.
"
  (interactive)
  (let ((force (not ycmd-force-semantic-completion)))
    (message "ycmd: force semantic completion %s."
             (if force "enabled" "disabled"))
    (setq ycmd-force-semantic-completion force)))

(defun ycmd--string-list-p (obj)
  "Return t if OBJ is a list of strings."
  (and (listp obj) (-all? #'stringp obj)))

(defun ycmd--locate-default-tags-file (buffer)
  "Look up directory hierarchy for first found default tags file for BUFFER."
  (-when-let* ((file (buffer-file-name buffer))
               (dir (and file
                         (locate-dominating-file
                          file ycmd-default-tags-file-name))))
    (expand-file-name ycmd-default-tags-file-name dir)))

(defun ycmd--get-tag-files (buffer)
  "Get tag files list for current BUFFER or nil."
  (-when-let (tag-files (cond
                         ((eq ycmd-tag-files 'auto)
                          (ycmd--locate-default-tags-file buffer))
                         ((or (stringp ycmd-tag-files)
                              (ycmd--string-list-p ycmd-tag-files))
                          ycmd-tag-files)))
    (unless (listp tag-files)
      (setq tag-files (list tag-files)))
    (mapcar 'expand-file-name tag-files)))

(defun ycmd-get-completions (buffer pos)
  "Get completions for position POS in BUFFER from the ycmd server.

Returns a deferred object which yields the HTTP message
content. If completions are available, the structure looks like
this:

   ((completion_start_column . 6)
    (completions .
                 [((kind . \"FUNCTION\")
                   (extra_menu_info . \"long double\")
                   (detailed_info . \"long double acoshl( long double )\n\")
                   (insertion_text . \"acoshl\")
                   (menu_text . \"acoshl( long double )\"))
                   . . .]))

If ycmd can't do completion because it's busy parsing, the
structure looks like this:

  ((message . \"Still parsing file, no completions yet.\")
   (traceback . \"long traceback string\")
   (exception
    (TYPE . \"RuntimeError\")))

To see what the returned structure looks like, you can use
`ycmd-display-completions'."
  (with-current-buffer buffer
    (goto-char pos)

    (when (ycmd-parsing-in-progress-p)
      (message "Ycmd completion unavailable while parsing is in progress."))

    (when ycmd-mode
      (let* ((extra-content (and ycmd-force-semantic-completion
                                 'force-semantic))
             (content (ycmd--standard-content-with-extras
                       buffer extra-content)))
        (ycmd--request
         "/completions"
         content
         :parser 'json-read)))))

(defun ycmd--handle-exception (results &optional default-handler)
  (let* ((exception (assoc-default 'exception results))
         (exception-type (assoc-default 'TYPE exception)))
    (cond ((string-equal exception-type "UnknownExtraConf")
           (ycmd--handle-extra-conf-exception results))
          (default-handler (funcall default-handler results)))))

(defun ycmd-goto ()
  "Go to the definition or declaration (whichever is most
sensible) of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoTo"))

(defun ycmd-goto-declaration ()
  "Go to the declaration of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToDeclaration"))

(defun ycmd-goto-definition ()
  "Go to the definition of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToDefinition"))

(defun ycmd-goto-implementation ()
  "Go to the implementation of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToImplementation"))

(defun ycmd-goto-imprecise ()
  "Fast implementation of Go To at the cost of precision,
useful in case compile-time is considerable."
  (interactive)
  (ycmd--goto "GoToImprecise"))

(defun ycmd--handle-goto-exception (results)
  (let ((msg (assoc-default 'message results nil "UNKNOWN")))
    (warn (format "goto exception: %s" msg))))

(defun ycmd--handle-goto-success (location)
  (push-mark)
  (ring-insert find-tag-marker-ring (point-marker))
  (ycmd--goto-location location))

(defun ycmd--goto (type)
  "Implementation of GoTo according to the request type."
  (when ycmd-mode
    (deferred:$

      (ycmd--send-goto-request type (current-buffer) (point))

      (deferred:nextc it
        (lambda (location)
          (when location
            (if (assoc-default 'exception location)
                (ycmd--handle-exception location #'ycmd--handle-goto-exception)
              (ycmd--handle-goto-success location))))))))

(defun ycmd--send-goto-request (type buffer pos)
  (with-current-buffer buffer
    (goto-char pos)
    (let ((content (cons (list "command_arguments" type)
                         (ycmd--standard-content buffer))))
      (ycmd--request
       "/run_completer_command"
       content
       :parser 'json-read))))

(defun ycmd--goto-location (location)
  "Move cursor to LOCATION, a structure as returned from e.g. the
various GoTo commands."
  (find-file (assoc-default 'filepath location))
  (goto-char (ycmd--col-line-to-position
              (assoc-default 'column_num location)
              (assoc-default 'line_num location))))

(defun ycmd--col-line-to-position (col line &optional buff)
  "Convert COL and LINE into a position in the current buffer.

COL and LINE are expected to be as returned from ycmd, e.g. from
notify-file-ready. Apparently COL can be 0 sometimes, in which
case this function returns 0.
"
  (let ((buff (or buff (current-buffer))))
    (if (= col 0)
        0
      (with-current-buffer buff
        (goto-line line)
        (forward-char (- col 1))
        (point)))))

(define-button-type 'ycmd--error-button
  'face '(error bold underline)
  'button 't)

(define-button-type 'ycmd--warning-button
  'face '(warning bold underline)
  'button 't)

(defun ycmd--make-button (start end type message)
  "Make a button of type TYPE from START to STOP in the current buffer.

When clicked, this will popup MESSAGE."
  (make-text-button
   start end
   'type type
   'action (lambda (b) (popup-tip message))))

(defconst ycmd--file-ready-buttons
  '(("ERROR" . ycmd--error-button)
    ("WARNING" . ycmd--warning-button))
  "A mapping from parse 'kind' to button types.")

(defun ycmd--line-start-position (line)
  "Find position at the start of LINE."
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (point)))

(defun ycmd--line-end-position (line)
  "Find position at the end of LINE."
  (save-excursion
    (goto-line line)
    (end-of-line)
    (point)))

(defmacro ycmd--with-destructured-parse-result (result body)
  "Destructure parse RESULT and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let* ((location_extent  (assoc-default 'location_extent ,result))
          (le_end           (assoc-default 'end location_extent))
          (end-line-num     (assoc-default 'line_num le_end))
          (end-column-num   (assoc-default 'column_num le_end))
          (end-filepath     (assoc-default 'filepath le_end))
          (le_start         (assoc-default 'start location_extent))
          (start-line-num   (assoc-default 'line_num le_start))
          (start-column-num (assoc-default 'column_num le_start))
          (start-filepath   (assoc-default 'filepath le_start))
          (location         (assoc-default 'location ,result))
          (line-num         (assoc-default 'line_num location))
          (column-num       (assoc-default 'column_num location))
          (filepath         (assoc-default 'filepath location))
          (kind             (assoc-default 'kind ,result))
          (text             (assoc-default 'text ,result))
          (ranges           (assoc-default 'ranges ,result)))
     ,body))

(defun ycmd--decorate-single-parse-result (r)
  "Decorates a buffer based on the contents of a single parse result struct R.

This is a fairly crude form of decoration, but it does give
reasonable visual feedback on the problems found by ycmd."
  (ycmd--with-destructured-parse-result r
    (--when-let (find-buffer-visiting filepath)
      (with-current-buffer it
        (let* ((start-pos (ycmd--line-start-position line-num))
               (end-pos (ycmd--line-end-position line-num))
               (btype (assoc-default kind ycmd--file-ready-buttons)))
          (when btype
            (with-silent-modifications
              (ycmd--make-button
               start-pos end-pos
               btype
               (concat kind ": " text)))))))))

(defun ycmd--display-error (msg)
  (message "ERROR: %s" msg))

(defun ycmd-decorate-with-parse-results (results)
  "Decorates a buffer using the results of a file-ready parse
list.

This is suitable as an entry in `ycmd-file-parse-result-hook`.
"
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil))
  (mapcar 'ycmd--decorate-single-parse-result results)
  results)

(defun ycmd--display-single-file-parse-result (r)
  (ycmd--with-destructured-parse-result r
    (insert (format "%s:%s - %s - %s\n" filepath line-num kind text))))

(defun ycmd-display-file-parse-results (results)
  (let ((buffer "*ycmd-file-parse-results*"))
    (get-buffer-create buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (mapcar 'ycmd--display-single-file-parse-result results))
    (display-buffer buffer)))

(defun ycmd-parse-buffer ()
  "Parse buffer."
  (interactive)
  (ycmd--report-status 'unparsed)
  (ycmd--conditional-parse))

(defun ycmd--handle-extra-conf-exception (result)
  "Handle an exception of type `UnknownExtraConf'.

Handle configuration file according the value of
`ycmd-extra-conf-handler'."
  (deferred:$
    (let ((conf-file (assoc-default 'extra_conf_file
                                    (assoc-default 'exception result))))
      (cond ((not conf-file)
             (warn "No extra_conf_file included in UnknownExtraConf exception. Consider reporting this."))

            ((or (eq ycmd-extra-conf-handler 'load)
                 (and (eq ycmd-extra-conf-handler 'ask)
                      (y-or-n-p (format "Load YCMD extra conf %s?" conf-file))))
             (ycmd--request "/load_extra_conf_file"
                            `((filepath . ,conf-file))))

            (t
             (ycmd--request "/ignore_extra_conf_file"
                            `((filepath . ,conf-file))))))
    (ycmd-notify-file-ready-to-parse)))

(defun ycmd--handle-runtime-exception (results)
  "Handle exeption for file already being parsed runtime error."
  (let* ((exception (assoc-default 'exception results))
         (exception-type (assoc-default 'TYPE exception))
         (message (assoc-default 'message results)))
    (cond ((and (string-equal exception-type "RuntimeError")
                (string-equal message "File already being parsed."))
           (setq ycmd--file-already-parsing-exception t)))))

(defun ycmd--handle-notify-response (results)
  "If RESULTS is a vector or nil, the response is an acual parse result.
Otherwise the response is probably an exception."
  (if (or (not results)
          (vectorp results))
      (run-hook-with-args 'ycmd-file-parse-result-hook results)
    (when (assoc 'exception results)
      (ycmd--handle-exception
       results #'ycmd--handle-runtime-exception))))

(defvar-local ycmd--file-already-parsing-exception nil)
(defun ycmd--parsing-buffer-finished-p ()
  "Return t if parsing has finished."
  (if ycmd--file-already-parsing-exception
      (setq ycmd--file-already-parsing-exception nil)
    t))

(defun ycmd-notify-file-ready-to-parse ()
  "Send a notification to ycmd that the buffer is ready to be parsed.

Only one active notification is allowed per buffer, and this
function enforces that constraint.

The results of the notification are passed to all of the
functions in `ycmd-file-parse-result-hook'.
"
  (when (and ycmd-mode (not (ycmd-parsing-in-progress-p)))
    (let* ((buff (current-buffer))
           (extra-content (and ycmd-tag-files 'tags))
           (content (cons '("event_name" . "FileReadyToParse")
                          (ycmd--standard-content-with-extras
                           buff extra-content))))

      ;; Record that the buffer is being parsed
      (ycmd--report-status 'parsing)

      (deferred:$
        ;; try
        (deferred:$
          ;; Make the request.
          (ycmd--request "/event_notification"
                         content
                         :parser 'json-read)

          (deferred:nextc it
            (lambda (results)
              (with-current-buffer buff
                (ycmd--handle-notify-response results)))))

        ;; catch
        (deferred:error it
          (lambda (err)
            (message "Error sending notification request: %s" err)))

        ;; finally. As I understand it, this should always be
        ;; executed.
        (deferred:nextc it
          (lambda ()
            (with-current-buffer buff
              (when (ycmd--parsing-buffer-finished-p)
                (ycmd--report-status 'parsed)))))))))

(defun ycmd-display-raw-file-parse-results ()
  "Request file-parse results and display them in a buffer in raw form.

This is primarily a debug/developer tool."
  (interactive)
  (deferred:$
    (ycmd-notify-file-ready-to-parse)
    (deferred:nextc it
      (lambda (content)
        (pop-to-buffer "*ycmd-file-ready*")
        (erase-buffer)
        (insert (pp-to-string content))))))

(defvar ycmd--server-actual-port 0
  "The actual port being used by the ycmd server. This is set
  based on the output from the server itself.")

(defvar ycmd--hmac-secret nil
  "This is populated with the hmac secret of the current
  connection. Users should never need to modify this, hence the
  defconst. It is not, however, treated as a constant by this
  code. This value gets set in ycmd-open.")

(defconst ycmd--server-process "ycmd-server"
  "The emacs name of the server process. This is used by
  functions like start-process, get-process, and delete-process.")

(defvar-local ycmd--notification-timer nil
  "Timer for notifying ycmd server to do work, e.g. parsing files.")

(defvar ycmd--keepalive-timer nil
  "Timer for sending keepalive messages to the server.")

(defconst ycmd--server-buffer-name "*ycmd-server*"
  "Name of the ycmd server buffer.")

(defun ycmd-major-mode-to-file-types (mode)
  "Map a major mode MODE to a list of file-types suitable for ycmd.

If there is no established mapping, return nil."
  (cdr (assoc mode ycmd-file-type-map)))

(defun ycmd--kill-notification-timer ()
  (when ycmd--notification-timer
    (cancel-timer ycmd--notification-timer)
    (setq ycmd--notification-timer nil)))

(defun ycmd--start-keepalive-timer ()
  "Kill any existing keepalive timer and start a new one."
  (ycmd--kill-keepalive-timer)
  (setq ycmd--keepalive-timer
        (run-with-timer
         ycmd-keepalive-period
         ycmd-keepalive-period
         (lambda () (ycmd--keepalive)))))

(defun ycmd--kill-keepalive-timer ()
  (when ycmd--keepalive-timer
    (cancel-timer ycmd--keepalive-timer)
    (setq ycmd--keepalive-timer nil)))

(defun ycmd--generate-hmac-secret ()
  "Generate a new, random 16-byte HMAC secret key."
  (let ((result '()))
    (dotimes (x 16 result)
      (setq result (cons (byte-to-string (random 256)) result)))
    (apply 'concat result)))

(defun ycmd--json-encode (obj)
  "A version of json-encode that uses {} instead of null for nil
values. This produces output for empty alists that ycmd expects."
    (json-encode obj))

;; This defines 'ycmd--hmac-function which we use to combine an HMAC
;; key and message contents.
(define-hmac-function ycmd--hmac-function
  (lambda (x) (secure-hash 'sha256 x nil nil 1))
  64 64)

(defun ycmd--options-contents (hmac-secret)
  "Return a struct which can be JSON encoded into a file to
create a ycmd options file.

When we start a new ycmd server, it needs an options file. It
reads this file and then deletes it since it contains a secret
key. So we need to generate a new options file for each ycmd
instance. This function effectively produces the contents of that
file."
  (let ((hmac-secret (base64-encode-string hmac-secret))
        (global-config (or ycmd-global-config ""))
        (extra-conf-whitelist (or ycmd-extra-conf-whitelist []))
        (max-num-identifier-candidates ycmd-max-num-identifier-candidates)
        (gocode-binary-path (or ycmd-gocode-binary-path "")))
    `((filetype_blacklist (vimwiki . 1) (mail . 1) (qf . 1) (tagbar . 1) (unite . 1) (infolog . 1) (notes . 1) (text . 1) (pandoc . 1) (markdown . 1))
      (auto_start_csharp_server . 1)
      (filetype_whitelist (* . 1))
      (csharp_server_port . 2000)
      (seed_identifiers_with_syntax . 0)
      (auto_stop_csharp_server . 1)
      (max_diagnostics_to_display . 30)
      (min_num_identifier_candidate_chars . 0)
      (max_num_identifier_candidates . ,max-num-identifier-candidates)
      (use_ultisnips_completer . 1)
      (complete_in_strings . 1)
      (complete_in_comments . 0)
      (confirm_extra_conf . 1)
      (server_keep_logfiles . 1)
      (global_ycm_extra_conf . ,global-config)
      (extra_conf_globlist . ,extra-conf-whitelist)
      (hmac_secret . ,hmac-secret)
      (collect_identifiers_from_tags_files . ,(if ycmd-tag-files 1 0))
      (filetype_specific_completion_to_disable (gitcommit . 1))
      (collect_identifiers_from_comments_and_strings . 0)
      (min_num_of_chars_for_completion . 2)
      (filepath_completion_use_working_dir . 0)
      (semantic_triggers . ())
      (auto_trigger . 1)
      (gocode_binary_path . ,gocode-binary-path))))

(defun ycmd--create-options-file (hmac-secret)
  "This creates a new options file for a ycmd server.

This creates a new tempfile and fills it with options. Returns
the name of the newly created file."
  (let ((options-file (make-temp-file "ycmd-options"))
        (options (ycmd--options-contents hmac-secret)))
    (with-temp-file options-file
      (insert (ycmd--json-encode options)))
    options-file))

(defun ycmd--start-server (hmac-secret)
  "This starts a new server using HMAC-SECRET as its HMAC secret."
  (let ((proc-buff (get-buffer-create ycmd--server-buffer-name)))
    (with-current-buffer proc-buff
      (erase-buffer)

      (let* ((options-file (ycmd--create-options-file hmac-secret))
             (server-command ycmd-server-command)
             (args (apply 'list (concat "--options_file=" options-file) ycmd-server-args))
             (server-program+args (append server-command args))
             (proc (apply #'start-process ycmd--server-process proc-buff server-program+args))
             (cont 1))
        (while cont
          (set-process-query-on-exit-flag proc nil)
          (accept-process-output proc 0 100 t)
          (let ((proc-output (with-current-buffer proc-buff
                               (buffer-string))))
            (cond
             ((string-match "^serving on http://.*:\\\([0-9]+\\\)$" proc-output)
              (progn
                (set-variable 'ycmd--server-actual-port
                              (string-to-number (match-string 1 proc-output)))
                (setq cont nil)))
             (t
              (incf cont)
              (when (< 3000 cont) ; timeout after 3 seconds
                (set-window-buffer nil proc-buff)
                (error "Server timeout."))))))))))

(defun ycmd--column-in-bytes ()
  "Calculate column offset in bytes for the current position and buffer."
  (- (position-bytes (point))
     (position-bytes (line-beginning-position))))

(defun ycmd--standard-content (&optional buffer)
  "Generate the 'standard' content for ycmd posts.

This extracts a bunch of information from BUFFER. If BUFFER is
nil, this uses the current buffer.
"
  (with-current-buffer (or buffer (current-buffer))
    (let* ((column-num (+ 1 (ycmd--column-in-bytes)))
           (line-num (line-number-at-pos (point)))
           (full-path (or (buffer-file-name) ""))
           (file-contents (buffer-substring-no-properties (point-min) (point-max)))
           (file-types (or (ycmd-major-mode-to-file-types major-mode)
                           '("generic"))))
      `(("file_data" .
         ((,full-path . (("contents" . ,file-contents)
                         ("filetypes" . ,file-types)))))
        ("filepath" . ,full-path)
        ("line_num" . ,line-num)
        ("column_num" . ,column-num)))))

(defun ycmd--standard-content-with-extras (buffer &optional extra)
  "Generate 'standard' content for BUFFER with EXTRA-CONTENT."
  (let ((standard-content (ycmd--standard-content buffer))
        (extra-content (pcase extra
                         (`force-semantic '(("force_semantic" . "true")))
                         (`tags (-when-let (tag-files (ycmd--get-tag-files buffer))
                                  `(("tag_files" . ,tag-files)))))))
    (append extra-content standard-content)))


(defvar ycmd--log-enabled nil
  "Whether http content should be logged. This is useful for
  debugging.")

(defun ycmd--log-content (header content)
  (when ycmd--log-enabled
    (let ((buffer (get-buffer-create "*ycmd-content-log*")))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n%s\n\n" header))
          (insert (pp-to-string content)))))))

(defun ycmd-show-debug-info ()
  "Show debug information."
  (interactive)
  (when ycmd-mode
    (let ((buffer (current-buffer)))

      (deferred:$
        (let ((content (ycmd--standard-content buffer)))
          (ycmd--request
           "/debug_info"
           content
           :parser 'json-read))

        (deferred:nextc it
          (lambda (res)
            (when res
              (with-help-window (get-buffer-create " *ycmd-debug-info*")
                (with-current-buffer standard-output
                  (princ "ycmd debug information for buffer ")
                  (insert (propertize (buffer-name buffer) 'face 'bold))
                  (princ " in ")
                  (let ((mode (buffer-local-value 'major-mode buffer)))
                    (insert-button (symbol-name mode)
                                   'type 'help-function
                                   'help-args (list mode)))
                  (princ ":\n\n")
                  (insert res)
                  (princ "\n\n")
                  (insert (format "Server running at: %s:%d"
                                  ycmd-host ycmd--server-actual-port)))))))))))

(defun ycmd--get-request-hmac (method path body)
  "Generate HMAC for request from METHOD, PATH and BODY."
  (let* ((hmac-secret (encode-coding-string
                       ycmd--hmac-secret 'utf-8 t))
         (joined-hmac-input
          (mapconcat
           (lambda (val) (ycmd--hmac-function
                          (encode-coding-string val 'utf-8 t)
                          hmac-secret))
           `(,method ,path ,(or body "")) "")))
    (ycmd--hmac-function joined-hmac-input hmac-secret)))

(defun* ycmd--request (location
                       content
                       &key (parser 'buffer-string) (type "POST"))
  "Send an asynchronous HTTP request to the ycmd server.

This starts the server if necessary.

Returns a deferred object which resolves to the content of the
response message.

LOCATION specifies the location portion of the URL. For example,
if LOCATION is '/feed_llama', the request URL is
'http://host:port/feed_llama'.

CONTENT will be JSON-encoded and sent over at the content of the
HTTP message.

PARSER specifies the function that will be used to parse the
response to the message. Typical values are buffer-string and
json-read. This function will be passed an the completely
unmodified contents of the response (i.e. not JSON-decoded or
anything like that.)
"
  (unless (ycmd-running?) (ycmd-open))

  (let* ((ycmd-request-backend 'url-retrieve)
         (content (json-encode content))
         (hmac (ycmd--get-request-hmac type location content))
         (encoded-hmac (base64-encode-string hmac 't)))
    (ycmd--log-content "HTTP REQUEST CONTENT" content)

    (deferred:$

      (ycmd-request-deferred
       (format "http://%s:%s%s" ycmd-host ycmd--server-actual-port location)
       :headers `(("Content-Type" . "application/json")
                  ("X-Ycm-Hmac" . ,encoded-hmac))
       :parser parser
       :data content
       :type type)

      (deferred:nextc it
        (lambda (req)
          (let ((content (ycmd-request-response-data req)))
            (ycmd--log-content "HTTP RESPONSE CONTENT" content)
            content))))))

(defun ycmd--conditional-parse (&optional condition)
  "Reparse the buffer if CONDITION is in `ycmd-parse-conditions'
or is nil."
  (when (and ycmd-mode
             (or (not condition)
                 (memq condition ycmd-parse-conditions)))
    (ycmd-notify-file-ready-to-parse)))

(defun ycmd--on-save ()
  "Function to run when the buffer has been saved."
  (ycmd--conditional-parse 'save))

(defun ycmd--on-idle-change ()
  "Function to run on idle-change."
  (ycmd--kill-notification-timer)
  (ycmd--conditional-parse 'idle-change))

(defun ycmd--on-change (beg end _len)
  "Function to run when a buffer change between BEG and END."
  (save-match-data
    (when ycmd-mode
      (ycmd--kill-notification-timer)
      (if (string-match-p "\n" (buffer-substring beg end))
          (ycmd--conditional-parse 'new-line)
        (setq ycmd--notification-timer
              (run-at-time ycmd-idle-change-delay nil
                           #'ycmd--on-idle-change))))))

(defconst ycmd-hooks-alist
  '((after-save-hook        . ycmd--on-save)
    (after-change-functions . ycmd--on-change))
  "Hooks which ycmd hooks in.")

(add-hook 'kill-emacs-hook 'ycmd-close)

(defvar ycmd-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'ycmd-parse-buffer)
    (define-key map "o" 'ycmd-open)
    (define-key map "c" 'ycmd-close)
    (define-key map "." 'ycmd-goto)
    (define-key map "f" 'ycmd-goto-definition)
    (define-key map "d" 'ycmd-goto-declaration)
    (define-key map "i" 'ycmd-goto-implementation)
    (define-key map "I" 'ycmd-goto-imprecise)
    (define-key map "s" 'ycmd-toggle-force-semantic-completion)
    (define-key map "v" 'ycmd-show-debug-info)
    map)
  "Keymap for `ycmd-mode' interactive commands.")

(defcustom ycmd-keymap-prefix (kbd "C-c Y")
  "Prefix for key bindings of `ycmd-mode'.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, you need to
explicitly re-define the prefix key:

    (define-key ycmd-mode-map ycmd-keymap-prefix nil)
    (setq ycmd-keymap-prefix (kbd \"C-c ,\"))
    (define-key ycmd-mode-map ycmd-keymap-prefix
                ycmd-command-map)"
  :group 'ycmd
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'ycmd-mode-map))
      (define-key ycmd-mode-map (symbol-value variable) nil)
      (define-key ycmd-mode-map key ycmd-command-map))
    (set-default variable key)))

(defvar ycmd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ycmd-keymap-prefix ycmd-command-map)
    map)
  "Keymap for `ycmd-mode'.")

(defvar-local ycmd--last-status-change 'unparsed
  "The last status of the current buffer.")

(defun ycmd-parsing-in-progress-p ()
  "Return t if parsing is in progress."
  (equal ycmd--last-status-change 'parsing))

(defun ycmd--report-status (status)
  "Report ycmd STATUS."
  (setq ycmd--last-status-change status)
  (force-mode-line-update))

(defun ycmd--mode-line-status-text ()
  "Get text for the mode line."
  (let ((force-semantic
         (when ycmd-force-semantic-completion "/s"))
        (text (pcase ycmd--last-status-change
                ((or `unparsed `parsed) "")
                (`parsing "*"))))
    (concat " ycmd" force-semantic text)))

;;;###autoload
(define-minor-mode ycmd-mode
  "Minor mode for interaction with the ycmd completion server.

When called interactively, toggle `ycmd-mode'.  With prefix ARG,
enable `ycmd-mode' if ARG is positive, otherwise disable it.

When called from Lisp, enable `ycmd-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `ycmd-mode'.
Otherwise behave as if called interactively.

\\{ycmd-mode-map}"
  :init-value nil
  :keymap ycmd-mode-map
  :lighter (:eval (ycmd--mode-line-status-text))
  :group 'ycmd
  :require 'ycmd
  :after-hook (ycmd--conditional-parse 'mode-enabled)
  (cond
   (ycmd-mode
    (dolist (hook ycmd-hooks-alist)
      (add-hook (car hook) (cdr hook) nil 'local)))
   (t
    (dolist (hook ycmd-hooks-alist)
      (remove-hook (car hook) (cdr hook) 'local)))))

;;;###autoload
(defun ycmd-setup ()
  "Setup `ycmd-mode'.

Hook `ycmd-mode' into modes in `ycmd-file-type-map'."
  (interactive)
  (dolist (it ycmd-file-type-map)
    (add-hook (intern (format "%s-hook" (symbol-name (car it)))) 'ycmd-mode)))
(make-obsolete 'ycmd-setup 'global-ycmd-mode "0.9.1")

(defun ycmd--maybe-enable-mode ()
  "Enable `ycmd-mode' according `ycmd-global-modes'."
  (when (pcase ycmd-global-modes
          (`t (ycmd-major-mode-to-file-types major-mode))
          (`all t)
          (`(not . ,modes) (not (memq major-mode modes)))
          (modes (memq major-mode modes)))
    (ycmd-mode)))

;;;###autoload
(define-globalized-minor-mode global-ycmd-mode ycmd-mode
  ycmd--maybe-enable-mode
  :init-value nil)

(provide 'ycmd)

;;; ycmd.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
