;;; ycmd-autocomplete.el --- autocomplete-mode source for ycmd
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "0.1") (auto-complete "1.4.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd provides completion for C/C++/ObjC and Python, among other
;; languages. This module supplies an auto-complete-mode source for
;; these completions.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
;;
;; ycmd-autocomplete depends on the following packages:
;;
;;   ycmd
;;   auto-complete
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'auto-complete-ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'auto-complete-ycmd)
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


(require 'auto-complete)
(require 'ycmd)

(warn "ycmd-autocomplete is incomplete and probably doesn't work right now!")

;; (defvar ac-ycmd-cache nil
;;   "Hold the results of the last ycmd-get-completions.")

(defun ac-ycmd-candidates ()
  "Get the list of completions at point."
  (if (ycmd-running?)
      (let ((completions (assoc-default 'completions (ycmd-get-completions (point)))))
        (mapcar (lambda (c) (cdr (assoc 'insertion_text c))) completions))
    nil))
  ;; (if (ycmd-running?)
  ;;     (progn
  ;;       (setq ac-ycmd-cache (assoc-default 'completions (ycmd-get-completions (point))))
  ;;       (mapcar (lambda (c) (cdr (assoc 'insertion_text c))) ac-ycmd-cache))
  ;;   (setq ac-ycmd-cache nil))

;; The autocomplete source for ycmd
(defvar ac-source-ycmd
  '(;(depends ycmd)
    (candidates . ac-ycmd-candidates)
    ;(cache)
    ;(document)
    ;(symbol . "s")
    (requires . 0)
    ))

;;;###autoload
(defun ac-ycmd-setup ()
  "Add ac-source-ycmd to autocomplete list."
  (setq ac-sources (append '(ac-source-ycmd) ac-sources)))

(provide 'auto-complete-ycmd)

;;; ycmd-autocomplete.el ends here
