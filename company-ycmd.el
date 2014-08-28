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
;; ycmd provides clang-based completion for C/C++/ObjC. This module
;; supplies a company-mode backend for these completions.
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

(require 'company)
(require 'ycmd)

(defconst company-ycmd-completion-properties
  '(kind
    extra_menu_info
    detailed_info
    menu_text))

(defun company-ycmd-construct-candidate (src)
  (let ((candidate (assoc-default 'insertion_text src)))
    (dolist (prop company-ycmd-completion-properties candidate)
      (put-text-property 0 1 prop (assoc-default prop src) candidate))))

(defun company-ycmd-candidates ()
  "Get list of completion candidate structs at point."
  (if (ycmd-running?)
      (mapcar
       'company-ycmd-construct-candidate
       (assoc-default 'completions (ycmd-get-completions (point))))
    nil))

(defun company-ycmd-get-metadata (candidate)
  (get-text-property 0 'detailed_info candidate))

(defun company-ycmd-get-annotation (candidate)
  (format " [%s]" (get-text-property 0 'kind candidate)))

(defun company-ycmd-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  ; See company.el for more info.
  (case command
    (interactive (company-begin-backend 'company-ycmd-backend))
    (prefix (and (eq major-mode 'c++-mode)
                 buffer-file-name
                 (ycmd-running?)
                 (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates
     (cons :async
           (lambda (cb)
             (apply cb
                    (company-ycmd-candidates)
                    '()))))
    (meta (company-ycmd-get-metadata arg))
    (annotation (company-ycmd-get-annotation arg))
    (no-cache 't))) ; No caching until everything is stable.

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd-backend to company-backends"
  (add-to-list 'company-backends 'company-ycmd-backend))

(provide 'company-ycmd)

;;; company-ycmd.el ends here
