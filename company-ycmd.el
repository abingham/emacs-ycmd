;;; company-ycmd.el --- company-mode backend for ycmd -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "0.1") (company "0.8.3") (deferred "0.2.0") (s "1.9.0") (dash "1.2.0"))
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
  '(c++-mode c-mode go-mode objc-mode rust-mode)
  "Major modes which have extended features in `company-ycmd'.")

(defun company-ycmd--extended-features-p ()
  "Check whether to use extended features."
  (memq major-mode company-ycmd--extended-features-modes))

(defun company-ycmd--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (assoc-default 'insertion_text candidate)))
    (s-starts-with? prefix insertion-text t)))

(defun company-ycmd--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun company-ycmd--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

(defmacro company-ycmd--with-destructured-candidate (candidate body)
  "Destructure CANDIDATE and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((insertion-text (assoc-default 'insertion_text candidate))
         (detailed-info (assoc-default 'detailed_info candidate))
         (kind (assoc-default 'kind candidate))
         (extra-menu-info (assoc-default 'extra_menu_info candidate))
         (menu-text (assoc-default 'menu_text candidate))
         (extra-data (assoc-default 'extra_data candidate)))
     (if (or (company-ycmd--identifier-completer-p extra-menu-info)
             (company-ycmd--filename-completer-p extra-menu-info))
         (propertize insertion-text 'return_type extra-menu-info)
       ,body)))

(defun company-ycmd--extract-params-clang (function-signature)
  "Extract parameters from FUNCTION-SIGNATURE if possible."
  (cond
   ((null function-signature) nil)
   ((string-match "[^:]:[^:]" function-signature)
    (substring function-signature (1+ (match-beginning 0))))
   ((string-match "\\((.*)[ a-z]*\\'\\)" function-signature)
    (let ((paren (match-beginning 1)))
      (if (not (and (eq (aref function-signature (1- paren)) ?>)
                    (s-contains?
                     "<" (substring function-signature 0 (1- paren)))))
          (match-string 1 function-signature)
        (with-temp-buffer
          (insert function-signature)
          (goto-char paren)
          (substring function-signature (1- (search-backward "<")))))))))

(defun company-ycmd--convert-kind-clang (kind)
  "Convert KIND string for display."
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
    ("NAMESPACE" "namespace")))

(defun company-ycmd--construct-candidate-clang (candidate)
  "Construct a completion string(s) from a CANDIDATE for cpp file-types.

Returns a list with one candidate or multiple candidates for
overloaded functions."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((overloaded-functions (and (company-ycmd--extended-features-p)
                                      company-ycmd-insert-arguments
                                      (stringp detailed-info)
                                      (s-split "\n" detailed-info t)))
           (items (or overloaded-functions (list menu-text)))
           candidates)
      (when (eq major-mode 'objc-mode)
        (setq insertion-text (s-chop-suffix ":" insertion-text)))
      (dolist (it (delete-dups items) candidates)
        (let* ((meta (if overloaded-functions it detailed-info))
               (params (company-ycmd--extract-params-clang it))
               (return-type (or (and overloaded-functions
                                     (string-match
                                      (concat "\\(.*\\) "
                                              (regexp-quote insertion-text))
                                      it)
                                     (match-string 1 it))
                                extra-menu-info))
               (kind (company-ycmd--convert-kind-clang kind))
               (doc (cdr (assoc 'doc_string extra-data))))
          (setq candidates
                (cons (propertize insertion-text 'return_type return-type
                                  'meta meta 'kind kind 'doc doc 'params params)
                      candidates)))))))

(defun company-ycmd--construct-candidate-go (candidate)
  "Construct completion string from a CANDIDATE for go file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((is-func (and extra-menu-info
                         (string-prefix-p "func" extra-menu-info)))
           (meta (and kind menu-text extra-menu-info
                      (concat kind " " menu-text
                              (if is-func
                                  (substring extra-menu-info 4 nil)
                                (concat " " extra-menu-info)))))
           (return-type (and extra-menu-info
                             (string-match "^func(.*) \\(.*\\)" extra-menu-info)
                             (match-string 1 extra-menu-info)))
           (params (and extra-menu-info
                        (or (string-match "^func\\((.*)\\) .*" extra-menu-info)
                            (string-match "^func\\((.*)\\)\\'" extra-menu-info))
                        (match-string 1 extra-menu-info)))
           (kind (if (and extra-menu-info (not is-func))
                     (concat kind ": " extra-menu-info)
                   kind)))
      (propertize insertion-text 'return_type return-type
                  'meta meta 'kind kind 'params params))))

(defun company-ycmd--construct-candidate-python (candidate)
  "Construct completion string from a CANDIDATE for python file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((kind extra-menu-info)
           (meta (and detailed-info
                      (or (and (string-match "\n" detailed-info)
                               (substring detailed-info 0 (match-beginning 0)))
                          detailed-info)))
           (location (assoc-default 'location extra-data))
           (filepath (assoc-default 'filepath location))
           (line-num (assoc-default 'line_num location)))
      (propertize insertion-text 'meta meta 'doc detailed-info 'kind kind
                  'filepath filepath 'line_num line-num))))

(defun company-ycmd--construct-candidate-rust (candidate)
  "Construct completion string from CANDIDATE for rust file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((meta extra-menu-info)
           (params (and extra-menu-info
                        (string-match
                         (concat "^fn " (regexp-quote insertion-text)
                                 "(\\(.*\\)).*")
                         extra-menu-info)
                        (->>
                         (s-split "," (match-string 1 extra-menu-info) t)
                         (cl-remove-if (lambda (it)
                                         (string-match-p "self" it)))
                         (s-join ",")
                         (s-trim-left)
                         (format "(%s)"))))
           (return-type (and extra-menu-info
                             (if (string-match
                                  (concat "^fn " (regexp-quote insertion-text)
                                          "(.*) -> \\(.*\\)")
                                  extra-menu-info)
                                 (match-string 1 extra-menu-info)
                               (and (string-prefix-p "fn" extra-menu-info)
                                    "void"))))
           (location (assoc-default 'location extra-data))
           (filepath (assoc-default 'filepath location))
           (line-num (assoc-default 'line_num location))
           (column-num (assoc-default 'column_num location)))
      (propertize insertion-text 'meta meta 'kind kind
                  'params params 'return_type return-type
                  'filepath filepath 'line_num line-num
                  'column_num column-num))))

(defun company-ycmd--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (company-ycmd--with-destructured-candidate candidate insertion-text))

(defun company-ycmd--construct-candidates (completion-vector
                                           prefix
                                           start-col
                                           construct-candidate-fn)
  "Construct candidates list from COMPLETION-VECTOR.

PREFIX is the prefix we calculated for doing the completion, and
START-COL is the column on which ycmd indicates we should place
the completion candidates.  If START-COL differs from start column
offset of PREFIX, we need to calculate the substring from PREFIX
for that difference and prepend it to the insertion-text.
CONSTRUCT-CANDIDATE-FN is a function to construct a completion
candidate.  See `company-ycmd--get-construct-candidate-fn'.

When `company-ycmd-enable-fuzzy-matching' is nil, check if
candidate starts with PREFIX, whether to include candidate in
candidates list."
  (let* ((prefix-start-col (- (+ 1 (ycmd--column-in-bytes)) (length prefix)))
         (prefix-size (- start-col prefix-start-col))
         (prefix-diff (substring-no-properties prefix 0 prefix-size))
         candidates)
    (dolist (candidate (append completion-vector nil) (nreverse candidates))
      (when (s-present? prefix-diff)
        (let ((it (assoc 'insertion_text candidate)))
          (setcdr it (concat prefix-diff
                             (substring-no-properties (cdr it))))))
      (when (or company-ycmd-enable-fuzzy-matching
                (company-ycmd--prefix-candidate-p candidate prefix))
        (let ((result (funcall construct-candidate-fn candidate)))
          (if (listp result)
              (setq candidates (append result candidates))
            (setq candidates (cons result candidates))))))))

(defun company-ycmd--get-construct-candidate-fn ()
  "Return function to construct candidate(s) for current `major-mode'."
  (pcase (car-safe (ycmd-major-mode-to-file-types major-mode))
    ((or "cpp" "c" "objc") 'company-ycmd--construct-candidate-clang)
    ("go" 'company-ycmd--construct-candidate-go)
    ("python" 'company-ycmd--construct-candidate-python)
    ("rust" 'company-ycmd--construct-candidate-rust)
    (_ 'company-ycmd--construct-candidate-generic)))

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
            (assoc-default 'completions c)
            prefix
            (assoc-default 'completion_start_column c)
            (company-ycmd--get-construct-candidate-fn))))))))

(defun company-ycmd--meta (candidate)
  "Fetch the metadata text-property from a CANDIDATE string."
  (let ((meta (get-text-property 0 'meta candidate)))
    (if (stringp meta)
        (let ((meta-trimmed (s-trim meta)))
          (if (company-ycmd--extended-features-p)
              (ycmd--fontify-code meta-trimmed)
            meta-trimmed))
      meta)))

(defun company-ycmd--annotation (candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (let ((kind (and company-ycmd-show-completion-kind
                   (get-text-property 0 'kind candidate)))
        (return-type (get-text-property 0 'return_type candidate))
        (params (get-text-property 0 'params candidate)))
    (concat params
            (when (s-present? return-type)
              (s-prepend " -> " return-type))
            (when (s-present? kind)
              (format " [%s]" kind)))))

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

(defun company-ycmd--post-completion (candidate)
  "Insert function arguments after completion for CANDIDATE."
  (--when-let (and (company-ycmd--extended-features-p)
                   company-ycmd-insert-arguments
                   (get-text-property 0 'params candidate))
    (insert it)
    (if (string-match "\\`:[^:]" it)
        ;; The function `company-clang-objc-templatify' has been renamed to
        ;; `company-template-objc-templatify' in company-mode commit
        ;; 6bf24912a8a3c2cfc5e72073b8eb0f1137ab7728. Remove check once a new
        ;; stable version is released.
        (if (fboundp 'company-template-objc-templatify)
            (company-template-objc-templatify it)
          (company-clang-objc-templatify it))
      (company-template-c-like-templatify
       (concat candidate it)))))

(defun company-ycmd--doc-buffer (candidate)
  "Return buffer with docstring for CANDIDATE if it is available."
  (let ((doc (get-text-property 0 'doc candidate)))
    (when (s-present? doc)
      (company-doc-buffer doc))))

(defun company-ycmd--location (candidate)
  "Return location for CANDIDATE."
  (-when-let* ((filepath (get-text-property 0 'filepath candidate))
               (line-num (get-text-property 0 'line_num candidate)))
    (cons filepath line-num)))

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
    (doc-buffer      (company-ycmd--doc-buffer arg))
    (location        (company-ycmd--location arg))))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd to the front of company-backends"
  (add-to-list 'company-backends 'company-ycmd))

(provide 'company-ycmd)

;;; company-ycmd.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
