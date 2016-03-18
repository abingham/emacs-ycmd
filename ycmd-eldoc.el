;;; ycmd-eldoc.el --- Eldoc support for ycmd-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; URL: https://github.com/abingham/emacs-ycmd
;; Version: 0.1
;; Package-Requires: ((ycmd "0.1") (deferred "0.2.0") (s "1.9.0") (dash "1.2.0") (cl-lib "0.5"))

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

(require 'cl-lib)
(require 'eldoc)
(require 'ycmd)
(require 'deferred)
(require 'dash)
(require 's)


(defvar-local ycmd-eldoc--cache nil)

(defun ycmd-eldoc--documentation-function ()
  "Eldoc function for `ycmd-mode'."
  (when ycmd-mode
    (--when-let (ycmd-eldoc--info-at-point)
      (eldoc-message it))))

(defun ycmd-eldoc--info-at-point ()
  "Get function info at point."
  (save-excursion
    (ycmd-eldoc--goto-func-name)
    (-when-let (symbol (symbol-at-point))
      (if (eq symbol (car ycmd-eldoc--cache))
          (cadr ycmd-eldoc--cache)
        (-when-let* ((completions (ycmd-get-completions :sync))
                     (candidates
                      (assoc-default 'completions completions))
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
                   (let ((text (assoc-default 'insertion_text val))
                         (extra-info (assoc-default 'extra_menu_info val)))
                     (and (s-equals? text symbol)
                          (or (not extra-info)
                              (not (-contains?
                                    '("[ID]" "[File]" "[Dir]" "[File&Dir]")
                                    extra-info))))))
                 ;; Convert vector to list
                 (append result nil)))
               (item (car filtered-list))
               (msg (or (assoc-default 'detailed_info item)
                        (assoc-default 'extra_menu_info item))))
    (unless (s-blank? msg)
      (car (s-split-up-to "\n" msg 1)))))

;;;###autoload
(defun ycmd-eldoc-setup ()
  "Setup eldoc for `ycmd-mode'."
  (interactive)
  (if (fboundp 'add-function)
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
