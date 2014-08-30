;;; company-ycmd.el --- company-mode backend for ycmd
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "0.1") (company "0.8.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd is a modular code-completion framework. It includes, for
;; example, completion for C/C++/ObjC and Python. This module supplies
;; a company-mode backend for these completions.
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
;; company-ycmd depends on the following packages:
;;
;;   ycmd
;;   company
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'company-ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'company-ycmd)
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

(require 'cc-cmds)
(require 'company)
(require 'ycmd)

(defconst company-ycmd-completion-properties
  '(kind
    extra_menu_info
    detailed_info
    menu_text))

(defcustom company-ycmd-modes '(c++-mode python-mode csharp-mode)
  "The list of modes for which company-ycmd will attempt completions.")

(defun company-ycmd-construct-candidate (src)
  (let ((candidate (assoc-default 'insertion_text src)))
    (dolist (prop company-ycmd-completion-properties candidate)
      (put-text-property 0 1 prop (assoc-default prop src) candidate))))

(defun company-ycmd-candidates ()
  "Get list of completion candidate structs at point."
  (if (ycmd-running?)
      (mapcar
       'company-ycmd-construct-candidate
       (assoc-default
        'completions
        (with-local-quit
          (ycmd-get-completions (point)))))
    nil))

(defun company-ycmd-get-metadata (candidate)
  (get-text-property 0 'detailed_info candidate))

(defun company-ycmd-get-annotation (candidate)
  (format " [%s]" (get-text-property 0 'kind candidate)))

(defun company-ycmd-get-prefix ()
  (and (memq major-mode company-ycmd-modes)
       buffer-file-name
       (ycmd-running?)
       (not (company-in-string-or-comment))
       (if (looking-back "\\.\\|->\\|::")
           (company-grab-symbol-cons "\\.\\|->\\|::" 2))))

(defun company-ycmd-get-candidates (prefix)
  (cons :async
        (lambda (cb) (funcall cb (company-ycmd-candidates)))))

(defun company-ycmd-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ycmd-backend))
    (prefix      (company-ycmd-get-prefix))
    (candidates  (company-ycmd-get-candidates arg))
    (meta        (company-ycmd-get-metadata arg))
    (annotation  (company-ycmd-get-annotation arg))
    (no-cache    't) ; Don't cache. It interferes with fuzzy matching.
    )) 

(defun company-ycmd-enable-comprehensive-automatic-completion ()
  "This updates company-begin-commands so that automatic
completion will occur after typing :: and ->. By default
company-mode will not start automatic completion after : and >
characters, so you need to call this if you want full automatic
completion for C/C++."
  (interactive)
  (mapcar
   (lambda (x)
     (unless (memq x company-begin-commands)
       (push x company-begin-commands)))
   '(c-electric-colon c-electric-lt-gt)))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd-backend to the front of company-backends"
  (add-to-list 'company-backends 'company-ycmd-backend))

(provide 'company-ycmd)

;;; company-ycmd.el ends here
