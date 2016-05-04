;;; ycmd-eldoc.el --- Eldoc support for ycmd-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; URL: https://github.com/abingham/emacs-ycmd
;; Version: 0.1
;; Package-Requires: ((ycmd "0.1") (deferred "0.2.0") (s "1.9.0") (dash "1.2.0") (cl-lib "0.5") (let-alist "1.0.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, add these lines to your init.el file:
;;
;;     (require 'ycmd-eldoc)
;;     (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
;;

;;; Code:

(eval-when-compile
  (require 'let-alist))
(require 'cl-lib)
(require 'eldoc)
(require 'ycmd)
(require 'deferred)
(require 'dash)
(require 's)

(defgroup ycmd-eldoc nil
  "Eldoc support for `ycmd-mode'."
  :group 'ycmd
  :group 'eldoc)

(defcustom ycmd-eldoc-always-semantic-server-query-modes
  '(not c-mode c++-mode objc-mode)
  "Modes for which `ycmd-eldoc' always queries semantic completion.

If t, the ycmd server query is always semantic.  If a list, server
query is semantic for all `major-mode' symbols in that list.  If
the `car' of the list is `not', server query is sematic for all
`major-mode' symbols _not_ in that list.  If nil, the server query
is only semantic after a semantic trigger.")

(defvar-local ycmd-eldoc--cache nil)

(defun ycmd-eldoc--documentation-function ()
  "Eldoc function for `ycmd-mode'."
  (when ycmd-mode
    (--when-let (ycmd-eldoc--info-at-point)
      (eldoc-message it))))

(defun ycmd-eldoc-always-semantic-server-query-p ()
  "Check whether server query should be semantic."
  (pcase ycmd-eldoc-always-semantic-server-query-modes
    (`t t)
    (`(not . ,modes) (not (memq major-mode modes)))
    (modes (memq major-mode modes))))

(defun ycmd-eldoc--info-at-point ()
  "Get function info at point."
  (save-excursion
    (ycmd-eldoc--goto-func-name)
    (-when-let (symbol (symbol-at-point))
      (if (eq symbol (car ycmd-eldoc--cache))
          (cadr ycmd-eldoc--cache)
        (-when-let* ((completions
                      (let ((ycmd-force-semantic-completion
                             (or ycmd-force-semantic-completion
                                 (ycmd-eldoc-always-semantic-server-query-p))))
                        (ycmd-get-completions :sync)))
                     (candidates (cdr (assq 'completions completions)))
                     (text (ycmd-eldoc--generate-message
                            (symbol-name symbol) candidates)))
          (setq text (ycmd--fontify-code text))
          (setq ycmd-eldoc--cache (list symbol text))
          text)))))

;; Source: https://github.com/racer-rust/emacs-racer/blob/master/racer.el
(defun ycmd-eldoc--goto-func-name ()
  "If point is inside a function call, move to the function name.
foo(bar, |baz); -> foo|(bar, baz);"
  (let ((last-paren-pos (nth 1 (syntax-ppss)))
        (start-pos (point)))
    (when last-paren-pos
      ;; Move to just before the last paren.
      (goto-char last-paren-pos)
      ;; If we're inside a round paren, we're inside a function call.
      (unless (looking-at "(")
        ;; Otherwise, return to our start position, as point may have been on a
        ;; function already:
        ;; foo|(bar, baz);
        (goto-char start-pos)))))

(defun ycmd-eldoc--generate-message (symbol result)
  "Generate eldoc message for SYMBOL from RESULT."
  (-when-let* ((filtered-list
                (cl-remove-if-not
                 (lambda (val)
                   (let-alist val
                     (and (s-equals? .insertion_text symbol)
                          (or (not .extra_menu_info)
                              (not (-contains?
                                    '("[ID]" "[File]" "[Dir]" "[File&Dir]")
                                    .extra_menu_info))))))
                 ;; Convert vector to list
                 (append result nil)))
               (item (car filtered-list))
               (msg (or (cdr (assq 'detailed_info item))
                        (cdr (assq 'extra_menu_info item)))))
    (unless (s-blank? msg)
      (car (s-split-up-to "\n" msg 1)))))

;;;###autoload
(defun ycmd-eldoc-setup ()
  "Setup eldoc for `ycmd-mode'."
  (interactive)
  (if (eval-when-compile (fboundp 'add-function))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'ycmd-eldoc--documentation-function)
    (set (make-local-variable 'eldoc-documentation-function)
         'ycmd-eldoc--documentation-function))
  (eldoc-mode +1))

(defadvice ycmd--teardown (after ycmd-teardown-after activate)
  "Reset ycmd-eldoc--cache on `ycmd--teardown'."
  (setq ycmd-eldoc--cache nil))

(provide 'ycmd-eldoc)

;;; ycmd-eldoc.el ends here
