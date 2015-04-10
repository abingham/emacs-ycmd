;;; company-ycmd.el --- company-mode backend for ycmd -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "0.1") (company "0.8.3") (deferred "0.2.0") (s "1.0.0") (dash "1.2.0"))
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
(require 'dash)
(require 'rx)

(defgroup company-ycmd nil
  "Company-mode completion backend for ycmd."
  :group 'company
  :group 'ycmd)

(defconst company-ycmd-completion-properties
  '(kind extra_menu_info detailed_info menu_text extra_data)
  "Fields from ycmd completions structures that we attach as text
  properties to company completion strings.")

(defcustom company-ycmd-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.

Only supported by modes in `company-ycmd--extended-features-modes'"
  :type 'boolean
  :group 'company-ycmd)

(defcustom company-ycmd-enable-fuzzy-matching t
  "When non-nil, use fuzzy matching for completion candidates.

Setting this to nil enables the `company-mode' internal cache
feature."
  :type 'boolean
  :group 'company-ycmd)

(defcustom company-ycmd-show-completion-kind t
  "Show kind of completion entry."
  :type 'boolean
  :group 'company-ycmd)

(defconst company-ycmd--extended-features-modes
  '(c++-mode c-mode)
  "Major modes which have extended features in `company-ycmd'.")

(defun company-ycmd--extended-features-p ()
  "Check whether to use extended features."
  (memq major-mode company-ycmd--extended-features-modes))

(defun company-ycmd--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (assoc-default 'insertion_text candidate))
        (case-fold-search t))
    (equal (string-match (regexp-quote prefix) insertion-text) 0)))

(defun company-ycmd--construct-candidates (start-col
					   completion-vector
					   prefix)
  "Construct candidates list from COMPLETION-VECTOR.

PREFIX is the prefix we calculated for doing the completion, and
START-COL is the column on which ycmd indicates we should place
the completion candidates.

When `company-ycmd-enable-fuzzy-matching' is nil, check if
candidate starts with PREFIX, whether to include candidate in
candidates list."
  (let ((completion-list (append completion-vector nil))
        (candidates '()))
    (dolist (candidate completion-list (nreverse candidates))
      (let* ((detailed-info (assoc-default 'detailed_info candidate))
             (function-signatures (and (company-ycmd--extended-features-p)
                                        company-ycmd-insert-arguments
                                       (stringp detailed-info)
                                       (s-split "\n" detailed-info t))))
        (when (or company-ycmd-enable-fuzzy-matching
                  (company-ycmd--prefix-candidate-p candidate prefix))
          (if function-signatures
              (dolist (meta (delete-dups function-signatures))
                (push (company-ycmd--construct-candidate candidate prefix start-col meta)
                      candidates))
            (push (company-ycmd--construct-candidate candidate prefix start-col)
                  candidates)))))))

(defun company-ycmd--construct-candidate (src prefix start-col &optional meta)
  "Convert a ycmd completion structure SRC to a candidate string.

META is a string containig the function signature and is used to
generate content for meta and annotation functions.

Takes a ycmd completion structure SRC,
extracts the 'insertion_text', attaches other properties to that
string as text-properties, and returns the string."
  (let* ((current-col (- (point) (line-beginning-position))) ; ≠ current-column
         (prefix-start-col (- (+ 1  current-col) (length prefix)))
         (prefix-size (- start-col prefix-start-col))
	 (candidate (concat (substring-no-properties prefix 0 prefix-size)
			    (substring-no-properties
			     (assoc-default 'insertion_text src)))))
    (put-text-property 0 1 'meta meta candidate)
    (when (and meta
               (string-match
                (concat "\\(.*\\) " (regexp-quote candidate)) meta))
      (put-text-property
       0 1 'return_type (match-string 1 meta) candidate))

    (dolist (prop company-ycmd-completion-properties candidate)
      (put-text-property 0 1 prop (assoc-default prop src) candidate))))

(defun company-ycmd--get-candidates (cb prefix)
  "Call CB with completion candidates for PREFIX at the current point."
  (deferred:$
    
    (deferred:try
      (deferred:$
        (if (ycmd-running?)
            (ycmd-get-completions (current-buffer) (point))))
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
	    (assoc-default 'completion_start_column c)
            (assoc-default 'completions c)
	    prefix)))))))

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
        (let ((meta-trimmed (s-trim meta)))
          (if (company-ycmd--extended-features-p)
              (company-ycmd--fontify-code meta-trimmed)
            meta-trimmed))
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

(defun company-ycmd--get-kind (candidate)
  "Get information about completion kind from CANDIDATE."
  (-when-let (kind (get-text-property 0 'kind candidate))
    (pcase kind
      ("STRUCT" "struct")
      ("CLASS" "class")
      ("ENUM" "enum")
      ("TYPE" "type")
      ("MEMBER" "member")
      ("FUNCTION" "fn")
      ("VARIABLE" "var")
      ("MACRO" "macro")
      ("PARAMETER" "parameter")
      ("NAMESPACE" "namespace"))))

(defun company-ycmd--get-return-type (candidate)
  "Get return type of CANDIDATE."
  (or (get-text-property 0 'return_type candidate)
      (get-text-property 0 'extra_menu_info candidate)))

(defun company-ycmd--annotation (candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (let ((kind (and company-ycmd-show-completion-kind
                   (company-ycmd--get-kind candidate)))
        (return-type (company-ycmd--get-return-type candidate)))
    (concat (company-ycmd--params candidate)
            (when (s-present? return-type)
              (concat " -> " return-type))
            (when kind (format " [%s]" kind)))))

(defconst company-ycmd--include-declaration
  (rx line-start "#" (zero-or-more blank) (or "include" "import")
      (one-or-more blank)
      (submatch (in "<\"") (zero-or-more (not (in ">\"")))))
  "Regular expression to find C/C++/ObjC include directives.")

(defun company-ycmd--in-include ()
  "Check if text before point is an include statement."
  (looking-back company-ycmd--include-declaration
                (line-beginning-position)))

(defun company-ycmd--prefix ()
  "Prefix-command handler for the company backend."
  (when (ycmd-parsing-in-progress-p)
    (message "Ycmd completion unavailable while parsing is in progress."))

  (and ycmd-mode
       buffer-file-name
       (ycmd-running?)
       (or (not (company-in-string-or-comment))
           (company-ycmd--in-include))
       (or (and (not (ycmd-parsing-in-progress-p))
                (company-grab-symbol-cons "\\.\\|->\\|::" 2))
           'stop)))

(defun company-ycmd--candidates (prefix)
  "Candidates-command handler for the company backend."
  (cons :async (lambda (cb)
                 (company-ycmd--get-candidates cb prefix))))

(defun company-ycmd--post-completion (arg)
  (when (and (company-ycmd--extended-features-p)
             company-ycmd-insert-arguments)
    (let ((params (company-ycmd--params arg)))
      (when params
        (insert params)
        (company-template-c-like-templatify
         (concat arg params))))))

(defun company-ycmd--doc-buffer (candidate)
  "Return buffer with docstring for CANDIDATE if it is available."
  (let* ((extra-data (get-text-property 0 'extra_data candidate))
         (doc (cdr (assoc 'doc_string extra-data))))
    (when (s-present? doc)
      (company-doc-buffer doc))))

(defun company-ycmd (command &optional arg &rest ignored)
  "The company-backend command handler for ycmd."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'company-ycmd))
    (prefix          (company-ycmd--prefix))
    (candidates      (company-ycmd--candidates arg))
    (meta            (company-ycmd--meta arg))
    (annotation      (company-ycmd--annotation arg))
    (no-cache        company-ycmd-enable-fuzzy-matching)
    (sorted          't)
    (post-completion (company-ycmd--post-completion arg))
    (doc-buffer      (company-ycmd--doc-buffer arg))))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd to the front of company-backends"
  (add-to-list 'company-backends 'company-ycmd))

(provide 'company-ycmd)

;;; company-ycmd.el ends here
