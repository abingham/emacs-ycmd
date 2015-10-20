;;; ycmd-request.el --- Compatible layer for URL request in Emacs

;; Copyright (C) 2012 Takafumi Arakaki
;; Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
;;   Free Software Foundation, Inc.

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Version: 0.2.0

;; This file is NOT part of GNU Emacs.

;; request.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; request.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; IMPORTANT: This is a patched version of tkf's request.el. It
;; includes a few patches that fix serious defects in request, but
;; which tkf has been unable to merge in yet. When/if he merges them
;; in, we can get rid of this file.

;; Request.el is a HTTP request library with multiple backends.  It
;; supports url.el which is shipped with Emacs and curl command line
;; program.  User can use curl when s/he has it, as curl is more reliable
;; than url.el.  Library author can use request.el to avoid imposing
;; external dependencies such as curl to users while giving richer
;; experience for users who have curl.

;; Following functions are adapted from GNU Emacs source code.
;; Free Software Foundation holds the copyright of them.
;; * `request--process-live-p'
;; * `request--url-default-expander'

;;; Code:

(eval-when-compile (require 'cl))
(require 'url)
(require 'mail-utils)

(defgroup ycmd-request nil
  "Compatible layer for URL request in Emacs."
  :group 'comm
  :prefix "ycmd-request-")

(defconst ycmd-request-version "0.2.0")

;;; Customize variables

(defcustom ycmd-request-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "request")
  "Directory to store data related to request.el."
  :group 'ycmd-request)

(defcustom ycmd-request-curl "curl"
  "Executable for curl command."
  :group 'ycmd-request)

(defcustom ycmd-request-backend (if (executable-find ycmd-request-curl)
                               'curl
                             'url-retrieve)
  "Backend to be used for HTTP request.
Automatically set to `curl' if curl command is found."
  :group 'ycmd-request)

(defcustom ycmd-request-timeout nil
  "Default request timeout in second.
`nil' means no timeout."
  :group 'ycmd-request)

(defcustom ycmd-request-log-level -1
  "Logging level for request.
One of `error'/`warn'/`info'/`verbose'/`debug'.
-1 means no logging."
  :group 'ycmd-request)

(defcustom ycmd-request-message-level 'warn
  "Logging level for request.
See `ycmd-request-log-level'."
  :group 'ycmd-request)


;;; Utilities

(defun ycmd-request--safe-apply (function &rest arguments)
  (condition-case err
      (apply #'apply function arguments)
    ((debug error))))

(defun ycmd-request--safe-call (function &rest arguments)
  (ycmd-request--safe-apply function arguments))

;; (defun ycmd-request--url-no-cache (url)
;;   "Imitate `cache=false' of `jQuery.ajax'.
;; See: http://api.jquery.com/jQuery.ajax/"
;;   ;; FIXME: parse URL before adding ?_=TIME.
;;   (concat url (format-time-string "?_=%s")))

(defmacro ycmd-request--document-function (function docstring)
  "Document FUNCTION with DOCSTRING.  Use this for defstruct accessor etc."
  (declare (indent defun)
           (doc-string 2))
  `(put ',function 'function-documentation ,docstring))

(defun ycmd-request--process-live-p (process)
  "Copied from `process-live-p' for backward compatibility (Emacs < 24).
Adapted from lisp/subr.el.
FSF holds the copyright of this function:
  Copyright (C) 1985-1986, 1992, 1994-1995, 1999-2012
    Free Software Foundation, Inc."
  (memq (process-status process) '(run open listen connect stop)))


;;; Logging

(defconst ycmd-request--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defun ycmd-request--log-level-as-int (level)
  (if (integerp level)
      level
    (or (cdr (assq level ycmd-request--log-level-def))
        0)))

(defvar ycmd-request-log-buffer-name " *ycmd-request-log*")

(defun ycmd-request--log-buffer ()
  (get-buffer-create ycmd-request-log-buffer-name))

(defmacro ycmd-request-log (level fmt &rest args)
  (declare (indent 1))
  `(let ((level (ycmd-request--log-level-as-int ,level))
         (log-level (ycmd-request--log-level-as-int ycmd-request-log-level))
         (msg-level (ycmd-request--log-level-as-int ycmd-request-message-level)))
     (when (<= level (max log-level msg-level))
       (let ((msg (format "[%s] %s" ,level
                          (condition-case err
                              (format ,fmt ,@args)
                            (error (format "
!!! Logging error while executing:
%S
!!! Error:
%S"
                                           ',args err))))))
         (when (<= level log-level)
           (with-current-buffer (ycmd-request--log-buffer)
             (setq buffer-read-only t)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert msg "\n"))))
         (when (<= level msg-level)
           (message "REQUEST %s" msg))))))


;;; HTTP specific utilities

(defconst ycmd-request--url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?~)
  "`url-unreserved-chars' copied from Emacs 24.3 release candidate.
This is used for making `ycmd-request--urlencode-alist' RFC 3986 compliant
for older Emacs versions.")

(defun ycmd-request--urlencode-alist (alist)
  ;; FIXME: make monkey patching `url-unreserved-chars' optional
  (let ((url-unreserved-chars ycmd-request--url-unreserved-chars))
    (loop for sep = "" then "&"
          for (k . v) in alist
          concat sep
          concat (url-hexify-string (format "%s" k))
          concat "="
          concat (url-hexify-string v))))


;;; Header parser

(defun ycmd-request--parse-response-at-point ()
  "Parse the first header line such as \"HTTP/1.1 200 OK\"."
  (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)")
  (list :version (match-string 1)
        :code (string-to-number (match-string 2))))

(defun ycmd-request--goto-next-body ()
  (re-search-forward "^\r\n"))


;;; Response object

(defstruct ycmd-request-response
  "A structure holding all relevant information of a request."
  status-code history data error-thrown symbol-status url
  done-p settings
  ;; internal variables
  -buffer -raw-header -timer -backend -tempfiles)

(defmacro ycmd-request--document-response (function docstring)
  (declare (indent defun)
           (doc-string 2))
  `(ycmd-request--document-function ,function ,(concat docstring "

.. This is an accessor for `ycmd-request-response' object.

\(fn RESPONSE)")))

(ycmd-request--document-response ycmd-request-response-status-code
  "Integer HTTP response code (e.g., 200).")

(ycmd-request--document-response ycmd-request-response-history
  "Redirection history (a list of response object).
The first element is the oldest redirection.

You can use restricted portion of functions for the response
objects in the history slot.  It also depends on backend.  Here
is the table showing what functions you can use for the response
objects in the history slot.

==================================== ============== ==============
Slots                                          Backends
------------------------------------ -----------------------------
\\                                    curl           url-retrieve
==================================== ============== ==============
ycmd-request-response-url                  yes            yes
ycmd-request-response-header               yes            no
other functions                       no             no
==================================== ============== ==============
")

(ycmd-request--document-response ycmd-request-response-data
  "Response parsed by the given parser.")

(ycmd-request--document-response ycmd-request-response-error-thrown
  "Error thrown during request.
It takes the form of ``(ERROR-SYMBOL . DATA)``, which can be
re-raised (`signal'ed) by ``(signal ERROR-SYMBOL DATA)``.")

(ycmd-request--document-response ycmd-request-response-symbol-status
  "A symbol representing the status of request (not HTTP response code).
One of success/error/timeout/abort/parse-error.")

(ycmd-request--document-response ycmd-request-response-url
  "Final URL location of response.")

(ycmd-request--document-response ycmd-request-response-done-p
  "Return t when the request is finished or aborted.")

(ycmd-request--document-response ycmd-request-response-settings
  "Keyword arguments passed to `request' function.
Some arguments such as HEADERS is changed to the one actually
passed to the backend.  Also, it has additional keywords such
as URL which is the requested URL.")

(defun ycmd-request-response-header (response field-name)
  "Fetch the values of RESPONSE header field named FIELD-NAME.

It returns comma separated values when the header has multiple
field with the same name, as :RFC:`2616` specifies.

Examples::

  (ycmd-request-response-header response
                           \"content-type\") ; => \"text/html; charset=utf-8\"
  (ycmd-request-response-header response
                           \"unknown-field\") ; => nil
"
  (let ((raw-header (ycmd-request-response--raw-header response)))
    (when raw-header
      (with-temp-buffer
        (erase-buffer)
        (insert raw-header)
        ;; ALL=t to fetch all fields with the same name to get comma
        ;; separated value [#rfc2616-sec4]_.
        (mail-fetch-field field-name nil t)))))
;; .. [#rfc2616-sec4] RFC2616 says this is the right thing to do
;;    (see http://tools.ietf.org/html/rfc2616.html#section-4.2).
;;    Python's requests module does this too.


;;; Backend dispatcher

(defconst ycmd-request--backend-alist
  '((url-retrieve
     . ((request             . ycmd-request--url-retrieve)
        (ycmd-request-sync        . ycmd-request--url-retrieve-sync)
        (terminate-process   . delete-process)
        (get-cookies         . ycmd-request--url-retrieve-get-cookies)))
    (curl
     . ((request             . ycmd-request--curl)
        (ycmd-request-sync        . ycmd-request--curl-sync)
        (terminate-process   . interrupt-process)
        (get-cookies         . ycmd-request--curl-get-cookies))))
  "Map backend and method name to actual method (symbol).

It's alist of alist, of the following form::

    ((BACKEND . ((METHOD . FUNCTION) ...)) ...)

It would be nicer if I can use EIEIO.  But as CEDET is included
in Emacs by 23.2, using EIEIO means abandon older Emacs versions.
It is probably necessary if I need to support more backends.  But
let's stick to manual dispatch for now.")
;; See: (view-emacs-news "23.2")

(defun ycmd-request--choose-backend (method)
  "Return `fucall'able object for METHOD of current `ycmd-request-backend'."
  (assoc-default
   method
   (or (assoc-default ycmd-request-backend ycmd-request--backend-alist)
       (error "%S is not valid `ycmd-request-backend'." ycmd-request-backend))))


;;; Cookie

(defun ycmd-request-cookie-string (host &optional localpart secure)
  "Return cookie string (like `document.cookie').

Example::

 (ycmd-request-cookie-string \"127.0.0.1\" \"/\")  ; => \"key=value; key2=value2\"
"
  (mapconcat (lambda (nv) (concat (car nv) "=" (cdr nv)))
             (ycmd-request-cookie-alist host localpart secure)
             "; "))

(defun ycmd-request-cookie-alist (host &optional localpart secure)
  "Return cookies as an alist.

Example::

 (ycmd-request-cookie-alist \"127.0.0.1\" \"/\")  ; => ((\"key\" . \"value\") ...)
"
  (funcall (ycmd-request--choose-backend 'get-cookies) host localpart secure))


;;; Main

(defun* ycmd-request-default-error-callback (url &key symbol-status
                                            &allow-other-keys)
  (ycmd-request-log 'error
    "Error (%s) while connecting to %s." symbol-status url))

(defun* ycmd-request (url &rest settings
                     &key
                     (type "GET")
                     (params nil)
                     (data nil)
                     (files nil)
                     (parser nil)
                     (headers nil)
                     (success nil)
                     (error nil)
                     (complete nil)
                     (timeout ycmd-request-timeout)
                     (status-code nil)
                     (sync nil)
                     (response (make-ycmd-request-response)))
  "Send request to URL.

Request.el has a single entry point.  It is `request'.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE       (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS      (alist)   set \"?key=val\" part in URL
DATA (string/alist)   data to be sent to the server
FILES       (alist)   files to be sent to the server (see below)
PARSER     (symbol)   a function that reads current buffer and return data
HEADERS     (alist)   additional headers to send with the request
SUCCESS  (function)   called on success
ERROR    (function)   called on error
COMPLETE (function)   called on both success and error
TIMEOUT    (number)   timeout in second
STATUS-CODE (alist)   map status code (int) to callback
SYNC         (bool)   If `t', wait until request is done.  Default is `nil'.
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr's in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it's better to use `&allow-other-keys' [#]_).::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success/error/timeout/abort/parse-error
     :response      response       ; ycmd-request-response object
     ...)

.. [#] `&allow-other-keys' is a special \"markers\" available in macros
   in the CL library for function definition such as `defun*' and
   `function*'.  Without this marker, you need to specify all arguments
   to be passed.  This becomes problem when request.el adds new arguments
   when calling callback functions.  If you use `&allow-other-keys'
   (or manually ignore other arguments), your code is free from this
   problem.  See info node `(cl) Argument Lists' for more information.

Arguments data, error-thrown, symbol-status can be accessed by
`ycmd-request-response-data', `ycmd-request-response-error-thrown',
`ycmd-request-response-symbol-status' accessors, i.e.::

    (ycmd-request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following accessors:
`ycmd-request-response-status-code',
`ycmd-request-response-url' and
`ycmd-request-response-settings'

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME &key PATH BUFFER STRING MIME-TYPE)

FILE-N can also be a string (path to the file) or a buffer object.
In that case, FILENAME is set to the file name or buffer name.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password.txt\" :file \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch.txt\"  :buffer ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\"     :data \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: http://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response body.  The current position in the HTTP
response buffer is at the beginning of the buffer.  As the HTTP
header is stripped off, the cursor is actually at the beginning
of the response body.  So, for example, you can pass `json-read'
to parse JSON object in the buffer.  To fetch whole response as a
string, pass `buffer-string'.

When using `json-read', it is useful to know that the returned
type can be modified by `json-object-type', `json-array-type',
`json-key-type', `json-false' and `json-null'.  See docstring of
each function for what it does.  For example, to convert JSON
objects to plist instead of alist, wrap `json-read' by `lambda'
like this.::

    (request
     \"http://...\"
     :parser (lambda ()
               (let ((json-object-type 'plist))
                 (json-read)))
     ...)

This is analogous to the `dataType' argument of jQuery.ajax_.
Only this function can access to the process buffer, which
is killed immediately after the execution of this function.

* SYNC

Synchronous request is functional, but *please* don't use it
other than testing or debugging.  Emacs users have better things
to do rather than waiting for HTTP request.  If you want a better
way to write callback chains, use `ycmd-request-deferred'.

If you can't avoid using it (e.g., you are inside of some hook
which must return some value), make sure to set TIMEOUT to
relatively small value.

Due to limitation of `url-retrieve-synchronously', response slots
`ycmd-request-response-error-thrown', `ycmd-request-response-history' and
`ycmd-request-response-url' are unknown (always `nil') when using
synchronous request with `url-retrieve' backend.

* Note

API of `request' is somewhat mixture of jQuery.ajax_ (Javascript)
and requests.request_ (Python).

.. _jQuery.ajax: http://api.jquery.com/jQuery.ajax/
.. _requests.request: http://docs.python-requests.org
"
  (ycmd-request-log 'debug "REQUEST")
  ;; FIXME: support CACHE argument (if possible)
  ;; (unless cache
  ;;   (setq url (ycmd-request--url-no-cache url)))
  (unless error
    (setq error (apply-partially #'ycmd-request-default-error-callback url))
    (setq settings (plist-put settings :error error)))
  (unless (or (stringp data)
              (null data)
              (assoc-string "Content-Type" headers t))
    (setq data (ycmd-request--urlencode-alist data))
    (setq settings (plist-put settings :data data)))
  (when params
    (assert (listp params) nil "PARAMS must be an alist.  Given: %S" params)
    (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                      (ycmd-request--urlencode-alist params))))
  (setq settings (plist-put settings :url url))
  (setq settings (plist-put settings :response response))
  (setf (ycmd-request-response-settings response) settings)
  (setf (ycmd-request-response-url      response) url)
  (setf (ycmd-request-response--backend response) ycmd-request-backend)
  ;; Call `ycmd-request--url-retrieve'(`-sync') or `ycmd-request--curl'(`-sync').
  (apply (if sync
             (ycmd-request--choose-backend 'ycmd-request-sync)
           (ycmd-request--choose-backend 'request))
         url settings)
  (when timeout
    (ycmd-request-log 'debug "Start timer: timeout=%s sec" timeout)
    (setf (ycmd-request-response--timer response)
          (run-at-time timeout nil
                       #'ycmd-request-response--timeout-callback response)))
  response)

(defun ycmd-request--clean-header (response)
  "Strip off carriage returns in the header of REQUEST."
  (ycmd-request-log 'debug "-CLEAN-HEADER")
  (let ((buffer       (ycmd-request-response--buffer      response))
        (backend      (ycmd-request-response--backend     response))
        sep-regexp)
    (if (eq backend 'url-retrieve)
        ;; FIXME: make this workaround optional.
        ;; But it looks like sometimes `url-http-clean-headers'
        ;; fails to cleanup.  So, let's be bit permissive here...
        (setq sep-regexp "^\r?$")
      (setq sep-regexp "^\r$"))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ycmd-request-log 'trace
          "(buffer-string) at %S =\n%s" buffer (buffer-string))
        (goto-char (point-min))
        (when (and (re-search-forward sep-regexp nil t)
                   ;; Are \r characters stripped off already?:
                   (not (equal (match-string 0) "")))
          (while (re-search-backward "\r$" (point-min) t)
            (replace-match "")))))))

(defun ycmd-request--cut-header (response)
  "Cut the first header part in the buffer of RESPONSE and move it to
raw-header slot."
  (ycmd-request-log 'debug "-CUT-HEADER")
  (let ((buffer (ycmd-request-response--buffer response)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t)
          (setf (ycmd-request-response--raw-header response)
                (buffer-substring (point-min) (point)))
          (delete-region (point-min) (min (1+ (point)) (point-max))))))))

(defun ycmd-request--parse-data (response parser)
  "Run PARSER in current buffer if ERROR-THROWN is nil,
then kill the current buffer."
  (ycmd-request-log 'debug "-PARSE-DATA")
  (let ((buffer (ycmd-request-response--buffer response)))
    (ycmd-request-log 'debug "parser = %s" parser)
    (when (and (buffer-live-p buffer) parser)
      (with-current-buffer buffer
        (ycmd-request-log 'trace
          "(buffer-string) at %S =\n%s" buffer (buffer-string))
        (goto-char (point-min))
        (setf (ycmd-request-response-data response) (funcall parser))))))

(defun* ycmd-request--callback (buffer &key parser success error complete
                                  timeout status-code response
                                  &allow-other-keys)
  (ycmd-request-log 'debug "YCMD-REQUEST--CALLBACK")
  (ycmd-request-log 'debug "(buffer-string) =\n%s"
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer (buffer-string))))

  ;; Sometimes BUFFER given as the argument is different from the
  ;; buffer already set in RESPONSE.  That's why it is reset here.
  ;; FIXME: Refactor how BUFFER is passed around.
  (setf (ycmd-request-response--buffer response) buffer)
  (ycmd-request-response--cancel-timer response)
  (symbol-macrolet
      ((error-thrown (ycmd-request-response-error-thrown response))
       (symbol-status (ycmd-request-response-symbol-status response))
       (data (ycmd-request-response-data response))
       (done-p (ycmd-request-response-done-p response)))

    ;; Parse response header
    (ycmd-request--clean-header response)
    (ycmd-request--cut-header response)
    ;; Note: Try to do this even `error-thrown' is set.  For example,
    ;; timeout error can occur while downloading response body and
    ;; header is there in that case.

    ;; Parse response body
    (ycmd-request-log 'debug "error-thrown = %S" error-thrown)
    (condition-case err
        (ycmd-request--parse-data response parser)
      (error
       (setq symbol-status 'parse-error)
       (setq error-thrown err)
       (ycmd-request-log 'error "Error from parser %S: %S" parser err)))
    (kill-buffer buffer)
    (ycmd-request-log 'debug "data = %s" data)

    ;; Determine `symbol-status'
    (unless symbol-status
      (setq symbol-status (if error-thrown 'error 'success)))
    (ycmd-request-log 'debug "symbol-status = %s" symbol-status)

    ;; Call callbacks
    (let ((args (list :data data
                      :symbol-status symbol-status
                      :error-thrown error-thrown
                      :response response)))
      (let* ((success-p (eq symbol-status 'success))
             (cb (if success-p success error))
             (name (if success-p "success" "error")))
        (when cb
          (ycmd-request-log 'debug "Executing %s callback." name)
          (ycmd-request--safe-apply cb args)))

      (let ((cb (cdr (assq (ycmd-request-response-status-code response)
                           status-code))))
        (when cb
          (ycmd-request-log 'debug "Executing status-code callback.")
          (ycmd-request--safe-apply cb args)))

      (when complete
        (ycmd-request-log 'debug "Executing complete callback.")
        (ycmd-request--safe-apply complete args)))

    (setq done-p t)

    ;; Remove temporary files
    ;; FIXME: Make tempfile cleanup more reliable.  It is possible
    ;;        callback is never called.
    (ycmd-request--safe-delete-files (ycmd-request-response--tempfiles response))))

(defun* ycmd-request-response--timeout-callback (response)
  (ycmd-request-log 'debug "-TIMEOUT-CALLBACK")
  (setf (ycmd-request-response-symbol-status response) 'timeout)
  (setf (ycmd-request-response-error-thrown response)  '(error . ("Timeout")))
  (let* ((buffer (ycmd-request-response--buffer response))
         (proc (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (when proc
      ;; This will call `ycmd-request--callback':
      (funcall (ycmd-request--choose-backend 'terminate-process) proc))

    (symbol-macrolet ((done-p (ycmd-request-response-done-p response)))
      (unless done-p
        ;; This code should never be executed.  However, it occurs
        ;; sometimes with `url-retrieve' backend.
        ;; FIXME: In Emacs 24.3.50 or later, this is always executed in
        ;;        ycmd-request-get-timeout test.  Find out if it is fine.
        (ycmd-request-log 'error "Callback is not called when stopping process! \
Explicitly calling from timer.")
        (when (buffer-live-p buffer)
          (destructuring-bind (&key code &allow-other-keys)
              (with-current-buffer buffer
                (goto-char (point-min))
                (ignore-errors (ycmd-request--parse-response-at-point)))
            (setf (ycmd-request-response-status-code response) code)))
        (apply #'ycmd-request--callback
               buffer
               (ycmd-request-response-settings response))
        (setq done-p t)))))

(defun ycmd-request-response--cancel-timer (response)
  (ycmd-request-log 'debug "YCMD-REQUEST-RESPONSE--CANCEL-TIMER")
  (symbol-macrolet ((timer (ycmd-request-response--timer response)))
    (when timer
      (cancel-timer timer)
      (setq timer nil))))


(defun ycmd-request-abort (response)
  "Abort request for RESPONSE (the object returned by `request').
Note that this function invoke ERROR and COMPLETE callbacks.
Callbacks may not be called immediately but called later when
associated process is exited."
  (symbol-macrolet ((buffer (ycmd-request-response--buffer response))
                    (symbol-status (ycmd-request-response-symbol-status response))
                    (done-p (ycmd-request-response-done-p response)))
    (let ((process (get-buffer-process buffer)))
      (unless symbol-status             ; should I use done-p here?
        (setq symbol-status 'abort)
        (setq done-p t)
        (when (and
               (processp process) ; process can be nil when buffer is killed
               (ycmd-request--process-live-p process))
          (funcall (ycmd-request--choose-backend 'terminate-process) process))))))


;;; Backend: `url-retrieve'

(defun* ycmd-request--url-retrieve-preprocess-settings
    (&rest settings &key type data files headers &allow-other-keys)
  (when files
    (error "`url-retrieve' backend does not support FILES."))
  (when (and (equal type "POST")
             data
             (not (assoc-string "Content-Type" headers t)))
    (push '("Content-Type" . "application/x-www-form-urlencoded") headers)
    (setq settings (plist-put settings :headers headers)))
  settings)

(defun* ycmd-request--url-retrieve (url &rest settings
                                   &key type data timeout response
                                   &allow-other-keys
                                   &aux headers)
  (setq settings (apply #'ycmd-request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (url-retrieve url #'ycmd-request--url-retrieve-callback
                               (nconc (list :response response) settings)))
         (proc (get-buffer-process buffer)))
    (setf (ycmd-request-response--buffer response) buffer)
    (process-put proc :ycmd-request-response response)
    (ycmd-request-log 'debug "Start querying: %s" url)
    (set-process-query-on-exit-flag proc nil)))

(defun* ycmd-request--url-retrieve-callback (status &rest settings
                                               &key response url
                                               &allow-other-keys)
  (declare (special url-http-method
                    url-http-response-status))
  (ycmd-request-log 'debug "-URL-RETRIEVE-CALLBACK")
  (ycmd-request-log 'debug "status = %S" status)
  (ycmd-request-log 'debug "url-http-method = %s" url-http-method)
  (ycmd-request-log 'debug "url-http-response-status = %s" url-http-response-status)

  (setf (ycmd-request-response-status-code response) url-http-response-status)
  (let ((redirect (plist-get status :redirect)))
    (when redirect
      (setf (ycmd-request-response-url response) redirect)))
  ;; Construct history slot
  (loop for v in
        (loop with first = t
              with l = nil
              for (k v) on status by 'cddr
              when (eq k :redirect)
              if first
              do (setq first nil)
              else
              do (push v l)
              finally do (cons url l))
        do (let ((r (make-ycmd-request-response :-backend 'url-retrieve)))
             (setf (ycmd-request-response-url r) v)
             (push r (ycmd-request-response-history response))))

  (symbol-macrolet ((error-thrown (ycmd-request-response-error-thrown response))
                    (status-error (plist-get status :error)))
    (when (and error-thrown status-error)
      (ycmd-request-log 'warn
        "Error %S thrown already but got another error %S from \
`url-retrieve'.  Ignoring it..." error-thrown status-error))
    (unless error-thrown
      (setq error-thrown status-error)))

  (apply #'ycmd-request--callback (current-buffer) settings))

(defun* ycmd-request--url-retrieve-sync (url &rest settings
                                        &key type data timeout response
                                        &allow-other-keys
                                        &aux headers)
  (setq settings (apply #'ycmd-request--url-retrieve-preprocess-settings settings))
  (setq headers (plist-get settings :headers))
  (let* ((url-request-extra-headers headers)
         (url-request-method type)
         (url-request-data data)
         (buffer (if timeout
                     (with-timeout
                         (timeout
                          (setf (ycmd-request-response-symbol-status response)
                                'timeout)
                          (setf (ycmd-request-response-done-p response) t)
                          nil)
                       (url-retrieve-synchronously url))
                   (url-retrieve-synchronously url))))
    (setf (ycmd-request-response--buffer response) buffer)
    ;; It seems there is no way to get redirects and URL here...
    (when buffer
      ;; Fetch HTTP response code
      (with-current-buffer buffer
        (goto-char (point-min))
        (destructuring-bind (&key version code)
            (ycmd-request--parse-response-at-point)
          (setf (ycmd-request-response-status-code response) code)))
      ;; Parse response body, etc.
      (apply #'ycmd-request--callback buffer settings)))
  response)

(defun ycmd-request--url-retrieve-get-cookies (host localpart secure)
  (mapcar
   (lambda (c) (cons (url-cookie-name c) (url-cookie-value c)))
   (url-cookie-retrieve host localpart secure)))


;;; Backend: curl

(defvar ycmd-request--curl-cookie-jar nil
  "Override what the function `ycmd-request--curl-cookie-jar' returns.
Currently it is used only for testing.")

(defun ycmd-request--curl-cookie-jar ()
  "Cookie storage for curl backend."
  (or ycmd-request--curl-cookie-jar
      (expand-file-name "curl-cookie-jar" ycmd-request-storage-directory)))

(defconst ycmd-request--curl-write-out-template
  (if (eq system-type 'windows-nt)
      "\\n(:num-redirects %{num_redirects} :url-effective %{url_effective})"
    "\\n(:num-redirects %{num_redirects} :url-effective \"%{url_effective}\")"))

(defun ycmd-request--curl-mkdir-for-cookie-jar ()
  (ignore-errors
    (make-directory (file-name-directory (ycmd-request--curl-cookie-jar)) t)))

(defun* ycmd-request--curl-command
    (url &key type data headers timeout files*
         &allow-other-keys
         &aux
         (cookie-jar (convert-standard-filename
                      (expand-file-name (ycmd-request--curl-cookie-jar)))))
  (append
   (list ycmd-request-curl "--silent" "--include"
         "--location"
         ;; FIXME: test automatic decompression
         "--compressed"
         ;; FIMXE: this way of using cookie might be problem when
         ;;        running multiple requests.
         "--cookie" cookie-jar "--cookie-jar" cookie-jar
         "--write-out" ycmd-request--curl-write-out-template)
   (loop for (name filename path mime-type) in files*
         collect "--form"
         collect (format "%s=@%s;filename=%s%s" name path filename
                         (if mime-type
                             (format ";type=%s" mime-type)
                           "")))
   (when data (list "--data-binary" "@-"))
   (when type (list "--request" type))
   (loop for (k . v) in headers
         collect "--header"
         collect (format "%s: %s" k v))
   (list url)))

(defun ycmd-request--curl-normalize-files-1 (files get-temp-file)
  (loop for (name . item) in files
        collect
        (destructuring-bind (filename &key file buffer data mime-type)
            (cond
             ((stringp item) (list (file-name-nondirectory item) :file item))
             ((bufferp item) (list (buffer-name item) :buffer item))
             (t item))
          (unless (= (loop for v in (list file buffer data) if v sum 1) 1)
            (error "Only one of :file/:buffer/:data must be given.  Got: %S"
                   (cons name item)))
          (cond
           (file
            (list name filename file mime-type))
           (buffer
            (let ((tf (funcall get-temp-file)))
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tf nil 'silent))
              (list name filename tf mime-type)))
           (data
            (let ((tf (funcall get-temp-file)))
              (with-temp-buffer
                (erase-buffer)
                (insert data)
                (write-region (point-min) (point-max) tf nil 'silent))
              (list name filename tf mime-type)))))))

(defun ycmd-request--curl-normalize-files (files)
  "Change FILES into a list of (NAME FILENAME PATH MIME-TYPE).
This is to make `ycmd-request--curl-command' cleaner by converting
FILES to a homogeneous list.  It returns a list (FILES* TEMPFILES)
where FILES* is a converted FILES and TEMPFILES is a list of
temporary file paths."
  (let (tempfiles noerror)
    (unwind-protect
        (let* ((get-temp-file (lambda ()
                                (let ((tf (make-temp-file "emacs-ycmd-request-")))
                                  (push tf tempfiles)
                                  tf)))
               (files* (ycmd-request--curl-normalize-files-1 files get-temp-file)))
          (setq noerror t)
          (list files* tempfiles))
      (unless noerror
        ;; Remove temporary files only when an error occurs
        (ycmd-request--safe-delete-files tempfiles)))))

(defun ycmd-request--safe-delete-files (files)
  "Remove FILES but do not raise error when failed to do so."
  (mapc (lambda (f) (condition-case err
                        (delete-file f)
                      (error (ycmd-request-log 'error
                               "Failed delete file %s. Got: %S" f err))))
        files))

(defun* ycmd-request--curl (url &rest settings
                           &key type data files headers timeout response
                           &allow-other-keys)
  "cURL-based request backend.

Redirection handling strategy
-----------------------------

curl follows redirection when --location is given.  However,
all headers are printed when it is used with --include option.
Number of redirects is printed out sexp-based message using
--write-out option (see `ycmd-request--curl-write-out-template').
This number is used for removing extra headers and parse
location header from the last redirection header.

Sexp at the end of buffer and extra headers for redirects are
removed from the buffer before it is shown to the parser function.
"
  (ycmd-request--curl-mkdir-for-cookie-jar)
  (let* (;; Use pipe instead of pty.  Otherwise, curl process hangs.
         (process-connection-type nil)
         ;; Avoid starting program in non-existing directory.
         (default-directory (expand-file-name "~/"))
         (buffer (generate-new-buffer " *request curl*"))
         (command (destructuring-bind
                      (files* tempfiles)
                      (ycmd-request--curl-normalize-files files)
                    (setf (ycmd-request-response--tempfiles response) tempfiles)
                    (apply #'ycmd-request--curl-command url :files* files*
                           settings)))
         (proc (apply #'start-process "request curl" buffer command)))
    (ycmd-request-log 'debug "Run: %s" (mapconcat 'identity command " "))
    (setf (ycmd-request-response--buffer response) buffer)
    (process-put proc :ycmd-request-response response)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'ycmd-request--curl-callback)
    (when data
      (process-send-string proc data)
      (process-send-eof proc))))

(defun ycmd-request--curl-read-and-delete-tail-info ()
  "Read a sexp at the end of buffer and remove it and preceding character.
This function moves the point at the end of buffer by side effect.
See also `ycmd-request--curl-write-out-template'."
  (let (forward-sexp-function)
    (goto-char (point-max))
    (forward-sexp -1)
    (let ((beg (1- (point))))
      (prog1
          (read (current-buffer))
        (delete-region beg (point-max))))))

(defconst ycmd-request--cookie-reserved-re
  (mapconcat
   (lambda (x) (concat "\\(^" x "\\'\\)"))
   '("comment" "commenturl" "discard" "domain" "max-age" "path" "port"
     "secure" "version" "expires")
   "\\|")
  "Uninterested keys in cookie.
See \"set-cookie-av\" in http://www.ietf.org/rfc/rfc2965.txt")

(defun ycmd-request--consume-100-continue ()
  "Remove \"HTTP/* 100 Continue\" header at the point."
  (destructuring-bind (&key code &allow-other-keys)
      (save-excursion (ignore-errors (ycmd-request--parse-response-at-point)))
    (when (equal code 100)
      (delete-region (point) (progn (ycmd-request--goto-next-body) (point)))
      ;; FIXME: Does this make sense?  Is it possible to have multiple 100?
      (ycmd-request--consume-100-continue))))

(defun ycmd-request--consume-200-connection-established ()
  "Remove \"HTTP/* 200 Connection established\" header at the point."
  (when (looking-at-p "HTTP/1\\.0 200 Connection established")
    (delete-region (point) (progn (ycmd-request--goto-next-body) (point)))))

(defun ycmd-request--curl-preprocess ()
  "Pre-process current buffer before showing it to user."
  (let (history)
    (destructuring-bind (&key num-redirects url-effective)
        (ycmd-request--curl-read-and-delete-tail-info)
      (goto-char (point-min))
      (ycmd-request--consume-100-continue)
      (ycmd-request--consume-200-connection-established)
      (when (> num-redirects 0)
        (loop with case-fold-search = t
              repeat num-redirects
              ;; Do not store code=100 headers:
              do (ycmd-request--consume-100-continue)
              do (let ((response (make-ycmd-request-response
                                  :-buffer (current-buffer)
                                  :-backend 'curl)))
                   (ycmd-request--clean-header response)
                   (ycmd-request--cut-header response)
                   (push response history))))

      (goto-char (point-min))
      (nconc (list :num-redirects num-redirects :url-effective url-effective
                   :history (nreverse history))
             (ycmd-request--parse-response-at-point)))))

(defun ycmd-request--curl-absolutify-redirects (start-url redirects)
  "Convert relative paths in REDIRECTS to absolute URLs.
START-URL is the URL requested."
  (loop for prev-url = start-url then url
        for url in redirects
        unless (string-match url-nonrelative-link url)
        do (setq url (url-expand-file-name url prev-url))
        collect url))

(defun ycmd-request--curl-absolutify-location-history (start-url history)
  "Convert relative paths in HISTORY to absolute URLs.
START-URL is the URL requested."
  (when history
    (setf (ycmd-request-response-url (car history)) start-url))
  (loop for url in (ycmd-request--curl-absolutify-redirects
                    start-url
                    (mapcar (lambda (response)
                              (ycmd-request-response-header response "location"))
                            history))
        for response in (cdr history)
        do (setf (ycmd-request-response-url response) url)))

(defun ycmd-request--curl-callback (proc event)
  (let* ((buffer (process-buffer proc))
         (response (process-get proc :ycmd-request-response))
         (symbol-status (ycmd-request-response-symbol-status response))
         (settings (ycmd-request-response-settings response)))
    (ycmd-request-log 'debug "YCMD-REQUEST--CURL-CALLBACK event = %s" event)
    (ycmd-request-log 'debug "YCMD-REQUEST--CURL-CALLBACK proc = %S" proc)
    (ycmd-request-log 'debug "YCMD-REQUEST--CURL-CALLBACK buffer = %S" buffer)
    (ycmd-request-log 'debug "YCMD-REQUEST--CURL-CALLBACK symbol-status = %S"
                 symbol-status)
    (cond
     ((and (memq (process-status proc) '(exit signal))
           (/= (process-exit-status proc) 0))
      (setf (ycmd-request-response-error-thrown response) (cons 'error event))
      (apply #'ycmd-request--callback buffer settings))
     ((equal event "finished\n")
      (destructuring-bind (&key version code num-redirects history error
                                url-effective)
          (condition-case err
              (with-current-buffer buffer
                (ycmd-request--curl-preprocess))
            ((debug error)
             (list :error err)))
        (ycmd-request--curl-absolutify-location-history (plist-get settings :url)
                                                   history)
        (setf (ycmd-request-response-status-code  response) code)
        (setf (ycmd-request-response-url          response) url-effective)
        (setf (ycmd-request-response-history      response) history)
        (setf (ycmd-request-response-error-thrown response)
              (or error (when (>= code 400) `(error . (http ,code)))))
        (apply #'ycmd-request--callback buffer settings))))))

(defun* ycmd-request--curl-sync (url &rest settings &key response &allow-other-keys)
  ;; To make timeout work, use polling approach rather than using
  ;; `call-process'.
  (lexical-let (finished)
    (prog1 (apply #'ycmd-request--curl url
                  :complete (lambda (&rest _) (setq finished t))
                  settings)
      (let ((proc (get-buffer-process (ycmd-request-response--buffer response))))
        (while (and (not finished) (ycmd-request--process-live-p proc))
          (accept-process-output proc))))))

(defun ycmd-request--curl-get-cookies (host localpart secure)
  (ycmd-request--netscape-get-cookies (ycmd-request--curl-cookie-jar)
                                 host localpart secure))


;;; Netscape cookie.txt parser

(defun ycmd-request--netscape-cookie-parse ()
  "Parse Netscape/Mozilla cookie format."
  (goto-char (point-min))
  (let ((tsv-re (concat "^\\="
                        (loop repeat 6 concat "\\([^\t\n]+\\)\t")
                        "\\(.*\\)"))
        cookies)
    (while
        (and
         (cond
          ((re-search-forward "^\\=#" nil t))
          ((re-search-forward "^\\=$" nil t))
          ((re-search-forward tsv-re)
           (push (loop for i from 1 to 7 collect (match-string i))
                 cookies)
           t))
         (= (forward-line 1) 0)
         (not (= (point) (point-max)))))
    (setq cookies (nreverse cookies))
    (loop for (domain flag path secure expiration name value) in cookies
          collect (list domain
                        (equal flag "TRUE")
                        path
                        (equal secure "TRUE")
                        (string-to-number expiration)
                        name
                        value))))

(defun ycmd-request--netscape-filter-cookies (cookies host localpart secure)
  (loop for (domain flag path secure-1 expiration name value) in cookies
        when (and (equal domain host)
                  (equal path localpart)
                  (or secure (not secure-1)))
        collect (cons name value)))

(defun ycmd-request--netscape-get-cookies (filename host localpart secure)
  (when (file-readable-p filename)
    (with-temp-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (ycmd-request--netscape-filter-cookies (ycmd-request--netscape-cookie-parse)
                                        host localpart secure))))


;;; Monkey patches for url.el

(defun ycmd-request--url-default-expander (urlobj defobj)
  "Adapted from lisp/url/url-expand.el.
FSF holds the copyright of this function:
  Copyright (C) 1999, 2004-2012  Free Software Foundation, Inc."
  ;; The default expansion routine - urlobj is modified by side effect!
  (if (url-type urlobj)
      ;; Well, they told us the scheme, let's just go with it.
      nil
    (setf (url-type urlobj) (or (url-type urlobj) (url-type defobj)))
    (setf (url-port urlobj) (or (url-portspec urlobj)
                                (and (string= (url-type urlobj)
                                              (url-type defobj))
				     (url-port defobj))))
    (if (not (string= "file" (url-type urlobj)))
	(setf (url-host urlobj) (or (url-host urlobj) (url-host defobj))))
    (if (string= "ftp"  (url-type urlobj))
	(setf (url-user urlobj) (or (url-user urlobj) (url-user defobj))))
    (if (string= (url-filename urlobj) "")
	(setf (url-filename urlobj) "/"))
    ;; If the object we're expanding from is full, then we are now
    ;; full.
    (unless (url-fullness urlobj)
      (setf (url-fullness urlobj) (url-fullness defobj)))
    (if (string-match "^/" (url-filename urlobj))
	nil
      (let ((query nil)
	    (file nil)
	    (sepchar nil))
	(if (string-match "[?#]" (url-filename urlobj))
	    (setq query (substring (url-filename urlobj) (match-end 0))
		  file (substring (url-filename urlobj) 0 (match-beginning 0))
		  sepchar (substring (url-filename urlobj) (match-beginning 0) (match-end 0)))
	  (setq file (url-filename urlobj)))
	;; We use concat rather than expand-file-name to combine
	;; directory and file name, since urls do not follow the same
	;; rules as local files on all platforms.
	(setq file (url-expander-remove-relative-links
		    (concat (url-file-directory (url-filename defobj)) file)))
	(setf (url-filename urlobj)
              (if query (concat file sepchar query) file))))))

(defadvice url-default-expander
  (around ycmd-request-monkey-patch-url-default-expander (urlobj defobj))
  "Monkey patch `url-default-expander' to fix bug #12374.
This patch is applied to Emacs trunk at revno 111291:
  http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/111291.
Without this patch, port number is not treated when using
`url-expand-file-name'.
See: http://thread.gmane.org/gmane.emacs.devel/155698"
  (setq ad-return-value (ycmd-request--url-default-expander urlobj defobj)))

(unless (equal (url-expand-file-name "/path" "http://127.0.0.1:8000")
               "http://127.0.0.1:8000/path")
  (ad-enable-advice 'url-default-expander
                    'around
                    'ycmd-request-monkey-patch-url-default-expander)
  (ad-activate 'url-default-expander))


(eval-when-compile (require 'url-http)
                   (defvar url-http-no-retry)
                   (defvar url-http-extra-headers)
                   (defvar url-http-data)
                   (defvar url-callback-function)
                   (defvar url-callback-arguments))
(declare-function url-http-idle-sentinel "url-http")
(declare-function url-http-activate-callback "url-http")
(declare-function url-http "url-http")
(declare-function url-http-parse-headers "url-http")

(defun ycmd-request--url-http-end-of-document-sentinel (proc why)
  "Adapted from lisp/url/url-http.el.
FSF holds the copyright of this function:
  Copyright (C) 1999, 2001, 2004-2012  Free Software Foundation, Inc."
  (url-http-debug "url-http-end-of-document-sentinel in buffer (%s)"
		  (process-buffer proc))
  (url-http-idle-sentinel proc why)
  (when (buffer-name (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (cond ((not (looking-at "HTTP/"))
	     (if url-http-no-retry
		 ;; HTTP/0.9 just gets passed back no matter what
		 (url-http-activate-callback)
	       ;; Call `url-http' again if our connection expired.
	       (erase-buffer)
               (let ((url-request-method url-http-method)
                     (url-request-extra-headers url-http-extra-headers)
                     (url-request-data url-http-data))
                 (url-http url-current-object url-callback-function
                           url-callback-arguments (current-buffer)))))
	    ((url-http-parse-headers)
	     (url-http-activate-callback))))))

(defadvice url-http-end-of-document-sentinel
  (around ycmd-request-monkey-patch-url-http-end-of-document-sentinel (proc why))
  "Monkey patch `url-http-end-of-document-sentinel' to fix bug #11469.
This patch is applied to Emacs trunk at revno 111291:
  http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/111291.
Without this patch, PUT method fails every two times.
See: http://thread.gmane.org/gmane.emacs.devel/155697"
  (setq ad-return-value (ycmd-request--url-http-end-of-document-sentinel proc why)))

(when (and (version< "24" emacs-version)
           (version< emacs-version "24.3.50.1"))
  (ad-enable-advice 'url-http-end-of-document-sentinel
                    'around
                    'ycmd-request-monkey-patch-url-http-end-of-document-sentinel)
  (ad-activate 'url-http-end-of-document-sentinel))


(provide 'ycmd-request)

;;; request.el ends here
