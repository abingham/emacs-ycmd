;;; company-ycmd.el --- company-mode backend for ycmd -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "0.1") (company "0.8.3") (deferred "0.2.0") (s "1.0.0"))
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
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
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
(require 'cl-lib)
(require 'company)
(require 'company-template)
(require 'deferred)
(require 'ycmd)
(require 's)

(defgroup company-ycmd nil
  "Company-mode completion backend for ycmd."
  :group 'company
  :group 'ycmd)

(defconst company-ycmd-completion-properties
  '(kind extra_menu_info detailed_info menu_text)
  "Fields from ycmd completions structures that we attach as text
  properties to company completion strings.")

(defcustom company-ycmd-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :group 'company-ycmd)

(defcustom company-ycmd-disable-caching t
  "When non-nil, disable `company-mode' internal caching feature.

When setting this to non-nil, a completion request is sent to
`ycmd' when completion progresses, e.g. the user inserts a new
character."
  :type 'boolean
  :group 'company-ycmd)

(defun company-ycmd--construct-candidates (completion-vector)
  "Contruct candidates list from COMPLETION-VECTOR."
  (let ((completion-list (append completion-vector nil))
        (candidates '()))
    (dolist (candidate completion-list (nreverse candidates))
      (let* ((detailed-info (assoc-default 'detailed_info candidate))
             (function-signatures (and (stringp detailed-info)
                                       (s-split "\n" detailed-info t))))
        (if (and company-ycmd-insert-arguments function-signatures)
            (dolist (meta function-signatures)
              (push (company-ycmd--construct-candidate candidate meta)
                    candidates))
          (push (company-ycmd--construct-candidate candidate)
                candidates))))))

(defun company-ycmd--construct-candidate (src &optional meta)
  "Converts a ycmd completion structure SRC to a candidate string.

META is a string containig the function signature and is used to
generate cotent for meta and annotation functions.

Takes a ycmd completion structure SRC,
extracts the 'insertion_text', attaches other properties to that
string as text-properties, and returns the string."
  (let ((candidate (substring-no-properties
                    (assoc-default 'insertion_text src))))

    (put-text-property 0 1 'meta meta candidate)
    (when (and meta
               (string-match
                (concat "\\(.*\\) " (regexp-quote candidate)) meta))
      (put-text-property
       0 1 'return_type (match-string 1 meta) candidate))

    (dolist (prop company-ycmd-completion-properties candidate)
      (put-text-property 0 1 prop (assoc-default prop src) candidate))))

(defun company-ycmd--get-candidates (cb)
  "Get list of completion candidate strings at point.

The returned strings have annotation, metadata, and other pieces
of information added as text-properties.
"
  (deferred:$
    
    (deferred:try
      (deferred:$
        (if (ycmd-running?)
            (ycmd-get-completions)))
      :catch (lambda (err) nil))
    
    (deferred:nextc it
      (lambda (c)
        (if (assoc-default 'exception c)

            (let ((msg (assoc-default 'message c nil "unknown error")))
              (message "Exception while fetching candidates: %s" msg)
              '())
          
          (funcall
           cb
           (company-ycmd--construct-candidates
            (assoc-default 'completions c))))))))

(cl-defun company-ycmd--fontify-code (code &optional (mode major-mode))
  "Fontify CODE."
  (cl-check-type mode function)
  (if (not (stringp code))
      code
    (with-temp-buffer
      (delay-mode-hooks (funcall mode))
      (setq font-lock-mode t)
      (funcall font-lock-function font-lock-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert code)
        (font-lock-default-fontify-region
         (point-min) (point-max) nil))
      (buffer-string))))

(defun company-ycmd--get-meta-or-fallback (candidate fallback)
  "Return a CANDIDATE's meta property or a FALLBACK property."
  (or (get-text-property 0 'meta candidate)
      (get-text-property 0 fallback candidate)))

(defun company-ycmd--meta (candidate)
  "Fetch the metadata text-property from a CANDIDATE string."
  (let ((meta (company-ycmd--get-meta-or-fallback
               candidate 'detailed_info)))
    (if (stringp meta)
        (company-ycmd--fontify-code (s-trim meta))
      meta)))

(defun company-ycmd--params (candidate)
  "Fetch function parameters from a CANDIDATE string if possible."
  (let ((params (company-ycmd--get-meta-or-fallback
                 candidate 'menu_text)))
    (cond
     ((null params) nil)
     ((string-match "[^:]:[^:]" params)
      (substring params (1+ (match-beginning 0))))
     ((string-match "\\((.*)[ a-z]*\\'\\)" params)
      (match-string 1 params)))))

(defun company-ycmd--annotation (candidate)
  "Fetch the annotation text-property from a candidate string."
  (let ((kind (get-text-property 0 'kind candidate))
        (return-type (or (get-text-property
                          0 'return_type candidate)
                         (get-text-property
                          0 'extra_menu_info candidate))))
    (concat (company-ycmd--params candidate)
            (unless (zerop (length return-type))
              (concat " -> " return-type))
            (unless (zerop (length kind))
              (format " [%s]" (downcase (substring kind 0 1)))))))

(defun company-ycmd--prefix ()
  "Prefix-command handler for the company backend."
  (when ycmd--notification-in-progress
    (message "Ycmd completion unavailable while parsing is in progress."))
  
  (and ycmd-mode
       buffer-file-name
       (ycmd-running?)
       (not ycmd--notification-in-progress)
       (not (company-in-string-or-comment))
       (or (let ((triggers-re "\\.\\|->\\|::"))
             (if (looking-back triggers-re)
                 (company-grab-symbol-cons triggers-re 2)
               (company-grab-symbol)))
           'stop)))

(defun company-ycmd--candidates (prefix)
  "Candidates-command handler for the company backend."
  (cons :async 'company-ycmd--get-candidates))

(defun company-ycmd--post-completion (arg)
  (let ((params (company-ycmd--params arg)))
    (when (and company-ycmd-insert-arguments params)
      (insert params)
      (company-template-c-like-templatify
       (concat arg params)))))

(defun company-ycmd (command &optional arg &rest ignored)
  "The company-backend command handler for ycmd."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'company-ycmd))
    (prefix          (company-ycmd--prefix))
    (candidates      (company-ycmd--candidates arg))
    (meta            (company-ycmd--meta arg))
    (annotation      (company-ycmd--annotation arg))
    (no-cache        company-ycmd-disable-caching)
    (sorted          't)
    (post-completion (company-ycmd--post-completion arg))))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd to the front of company-backends"
  (add-to-list 'company-backends 'company-ycmd))

(provide 'company-ycmd)

;;; company-ycmd.el ends here
