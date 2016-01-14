;;; flycheck-ycmd.el --- flycheck integration for ycmd
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((emacs "24") (dash "1.2.0") (flycheck "0.22") (ycmd "0.9"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This provides flycheck integration for ycmd. It allows flycheck to
;; use ycmd's parse results for it display. It essentially works by
;; caching the ycmd parse results and then using them when the checker
;; is invoked.
;;
;; Basic usage:
;;
;;  (require 'flycheck-ycmd)
;;  (flycheck-ycmd-setup)
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
;;
;;; Code:

(require 'dash)
(require 'flycheck)
(require 'ycmd)

;; See http://www.lunaryorn.com/2014/12/03/generic-syntax-checkers-in-flycheck.html for more info

;; This maps ycmd result 'kinds' to flycheck 'levels'.
(defconst flycheck-ycmd--level-map
  '(("ERROR" . error)
    ("WARNING" . warning)))

(defun flycheck-ycmd--result-to-error (result checker)
  "Convert ycmd parse RESULT for CHECKER into a flycheck error object."
  (ycmd--with-destructured-parse-result result
    (if (string-equal filepath (buffer-file-name))
        (flycheck-error-new
         :line line-num
         :column column-num
         :buffer (current-buffer)
         :filename filepath
         :message (concat text (when (eq fixit-available t) " (FixIt)"))
         :checker checker
         :level (assoc-default kind flycheck-ycmd--level-map 'string-equal 'error)))))

(defun flycheck-ycmd--start (checker callback)
  "Start ycmd flycheck CHECKER using CALLBACK to communicate with flycheck."
  (let ((errors (delq
                 nil
                 (mapcar (lambda (result)
                           (flycheck-ycmd--result-to-error result checker))
                         flycheck-ycmd--cache))))
    (funcall callback 'finished errors))

  ;; OR call (callback 'errored some-message)
  )

(defvar flycheck-ycmd--cache '())

(defun flycheck-ycmd--cache-parse-results (results)
  "Cache ycmd output RESULTS for error display.

We cache the results and use them as the basis for the error
display."
  (setq flycheck-ycmd--cache results)
  (flycheck-buffer-automatically))

(defun flycheck-ycmd--in-supported-mode ()
  "Determines if buffer is in `ycmd-mode` and another mode supported by ycmd."
  (and ycmd-mode (ycmd-diagnostic-file-types major-mode)))

;;;###autoload
(defun flycheck-ycmd-setup ()
  "Convenience function to setup the ycmd flycheck checker.

This adds a hook to watch for ycmd parse results, and it adds the
ycmd checker to the list of flycheck checkers."
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
  (add-to-list 'flycheck-checkers 'ycmd))

(flycheck-define-generic-checker 'ycmd
  "A flycheck checker using parse results from ycmd."
  :start #'flycheck-ycmd--start
  :predicate #'flycheck-ycmd--in-supported-mode)

(provide 'flycheck-ycmd)

;;; flycheck-ycmd.el ends here
