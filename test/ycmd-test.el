;;; ycmd-test.el --- tests for ycmd.el -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((emacs "24") (f "0.17.1") (ycmd "0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; These are some tests for ycmd.el.
;;
;; The tests are built using the standard `ert' module, so you can run
;; them using a number of techniques. For example:
;;
;;     (require 'ycmd-test)
;;     (ert-run-tests-interactively "ycmd-test")
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
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


(require 'ert)
(require 'f)
(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ert)
(require 'flycheck-ycmd)

(defconst ycmd-test-location
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst ycmd-test-resources-location
  (f-join ycmd-test-location "resources"))

(defconst ycmd-test-global-conf
  (f-join ycmd-test-resources-location ".ycm_extra_conf.py"))

(defun ycmd-test-prepare-file (filename mode)
  "Create a new temporary file containing CONTENT, put that file
into MODE, and wait for initial ycmd parsing of the file to
complete.

This has the side-effect of (re)starting ycmd.

Return the buffer.
"
  (let ((buff (find-file-noselect
               (f-join ycmd-test-resources-location filename)))
        (ycmd-global-config ycmd-test-global-conf)
        (ycmd-extra-conf-handler 'load)
        (ycmd-parse-conditions '(mode-enabled)))
    (with-current-buffer buff
      (delay-mode-hooks (funcall mode))
      (ycmd-open)
      (ycmd-mode t)
      (while (ycmd-parsing-in-progress-p) (sit-for 0.1)))
    buff))

(defmacro ycmd-ert-test-deferred (name filename mode request-func column line &rest body)
  "Define a test case for a deferred request to the ycmd.

NAME is a symbol denoting the local name of the test.  The test
itself is named `ycmd-test-NAME'.  FILENAME is the input file with
the `major-mode' MODE.  REQUEST-FUNC is the function with the
request to the ycmd server.  COLUMN and LINE is the file location
for the request.  The remaining forms of BODY are used to evaluate
the server's response,"
  (declare (indent 3) (debug t))
  (let ((full-name (intern (format "ycmd-test-%s" name))))
    `(ert-deftest ,full-name ()
       (let* ((buff (ycmd-test-prepare-file ,filename ,mode))
              (current-position (ycmd--col-line-to-position ,column ,line buff)))
         (deferred:sync!
           (deferred:$
             (funcall ,request-func buff current-position)
             (deferred:nextc it
               (lambda (response)
                 ,@body))))
         (kill-buffer buff)))))

(ycmd-ert-test-deferred get-completions "test.cpp" 'c++-mode
  'ycmd-get-completions
  7 8
  (let ((start-col (assoc-default 'completion_start_column response))
        (completions (assoc-default 'completions response)))
    (should (some (lambda (c)
                    (string-equal
                     "llurp" (assoc-default 'insertion_text c)))
                  completions))
    (should (= start-col 7))))

(ycmd-ert-test-deferred goto-declaration "test_goto.cpp" 'c++-mode
  (apply-partially #'ycmd--send-completer-command-request "GoToDeclaration")
  7 9
  (if (assoc-default 'exception response)
      (should nil)
    (progn
      (should (= (assoc-default 'column_num response) 10))
      (should (= (assoc-default 'line_num response) 2)))))

(ycmd-ert-test-deferred goto-definition "test_goto.cpp" 'c++-mode
  (apply-partially #'ycmd--send-completer-command-request "GoToDefinition")
  7 9
  (if (assoc-default 'exception response)
      (should nil)
    (progn
      (should (= (assoc-default 'column_num response) 11))
      (should (= (assoc-default 'line_num response) 5)))))


(defmacro ycmd-test-with-buffer (filename mode &rest body)
  "Create temporary current buffer with FILENAME and MODE and execute BODY."
  (declare (indent 2) (debug t))
  `(let ((it (ycmd-test-prepare-file ,filename ,mode)))
     (save-excursion
       (with-current-buffer it
         ,@body))
     (kill-buffer it)))

(ert-deftest ycmd-test-col-line-to-position ()
  (ycmd-test-with-buffer "test_goto.cpp" 'c++-mode
    (should (= (ycmd--col-line-to-position 10 2) 23))))

(defmacro ycmd-test-with-temp-buffer (mode &rest body)
  "Create temporary current buffer MODE and execute BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks (funcall ,mode))
     ,@body))

(defun ycmd-test-has-property-with-value (property value item)
  (let ((pred (pcase value
                ((pred stringp) 'string=)
                (_ '=))))
    (funcall pred value (get-text-property 0 property item))))

(ert-deftest company-ycmd-test-construct-candidate-clang ()
  (ycmd-test-with-temp-buffer 'c++-mode
    (let* ((data '((menu_text . "foo()")
                   (insertion_text . "foo")
                   (detailed_info . "void foo()\nint foo( int i )\n")
                   (extra_menu_info . "void")
                   (kind . "FUNCTION")
                   (extra_data (doc_string . "A docstring"))))
           (candidates (company-ycmd--construct-candidate-clang data))
           (candidate-1 (nth 0 candidates))
           (candidate-2 (nth 1 candidates)))
      (should (= (length candidates) 2))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate-1)))
      (should (ycmd-test-has-property-with-value 'kind "fn" candidate-1))
      (should (ycmd-test-has-property-with-value 'meta "int foo( int i )" candidate-1))
      (should (ycmd-test-has-property-with-value 'return_type "int" candidate-1))
      (should (ycmd-test-has-property-with-value 'params "( int i )" candidate-1))
      (should (ycmd-test-has-property-with-value 'doc "A docstring" candidate-1))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate-2)))
      (should (ycmd-test-has-property-with-value 'kind "fn" candidate-2))
      (should (ycmd-test-has-property-with-value 'meta "void foo()" candidate-2))
      (should (ycmd-test-has-property-with-value 'return_type "void" candidate-2))
      (should (ycmd-test-has-property-with-value 'params "()" candidate-2))
      (should (ycmd-test-has-property-with-value 'doc "A docstring" candidate-2)))))

(ert-deftest company-ycmd-test-construct-candidate-go ()
  (ycmd-test-with-temp-buffer 'go-mode
    (let* ((data '((menu_text . "Print")
                   (insertion_text . "Print")
                   (detailed_info . "Print func(a ...interface{}) (n int, err error) func")
                   (extra_menu_info . "func(a ...interface{}) (n int, err error)")
                   (kind . "func")))
           (candidate (company-ycmd--construct-candidate-go data)))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "func" candidate))
      (should (ycmd-test-has-property-with-value 'params "(a ...interface{})" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "func Print(a ...interface{}) (n int, err error)" candidate))
      (should (ycmd-test-has-property-with-value
               'return_type "(n int, err error)" candidate)))))

(ert-deftest company-ycmd-test-contruct-candidate-python ()
  (ycmd-test-with-temp-buffer 'python-mode
    (let* ((data '((insertion_text . "foo")
                   (detailed_info . "foo(self)\n\nA function")
                   (extra_data
                    (location
                     (line_num . 6)
                     (column_num . 7)
                     (filepath . "/foo/bar.py")))
                   (extra_menu_info . "function: bar.Foo.foo")))
           (candidate (company-ycmd--construct-candidate-python data)))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'meta "foo(self)" candidate))
      (should (ycmd-test-has-property-with-value
               'doc (assoc-default 'detailed_info data) candidate))
      (should (ycmd-test-has-property-with-value
               'kind (assoc-default 'extra_menu_info data) candidate))
      (should (ycmd-test-has-property-with-value
               'filepath (assoc-default
                          'filepath
                          (assoc-default
                           'location (assoc-default 'extra_data data)))
               candidate))
      (should (ycmd-test-has-property-with-value
               'line_num (assoc-default
                          'line_num
                          (assoc-default
                           'location (assoc-default 'extra_data data)))
               candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-rust ()
  (ycmd-test-with-temp-buffer 'rust-mode
    (let* ((data '((insertion_text . "foo")
                   (kind . "Function")
                   (extra_data
                    (location
                     (line_num . 40)
                     (column_num . 8)
                     (filepath . "/foo/bar.rs")))
                   (extra_menu_info . "fn foo(&self, x: f64) -> f64")))
           (candidate (company-ycmd--construct-candidate-rust data)))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value
               'meta (assoc-default 'extra_menu_info data) candidate))
      (should (ycmd-test-has-property-with-value
               'kind (assoc-default 'kind data) candidate))
      (should (ycmd-test-has-property-with-value 'params "(x: f64)" candidate))
      (should (ycmd-test-has-property-with-value 'return_type "f64" candidate))
      (should (ycmd-test-has-property-with-value
               'filepath (assoc-default
                          'filepath
                          (assoc-default
                           'location (assoc-default 'extra_data data)))
               candidate))
      (should (ycmd-test-has-property-with-value
               'line_num (assoc-default
                          'line_num
                          (assoc-default
                           'location (assoc-default 'extra_data data)))
               candidate))
      (should (ycmd-test-has-property-with-value
               'column_num (assoc-default
                            'column_num
                            (assoc-default
                             'location (assoc-default 'extra_data data)))
               candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-generic ()
  (ycmd-test-with-temp-buffer 'c++-mode
    (let* ((data '((insertion_text . "foo")
                   (extra_menu_info . "[ID]")))
           (candidate (company-ycmd--construct-candidate-generic data)))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value
               'return_type (assoc-default 'extra_menu_info data) candidate)))))

(ert-deftest company-ycmd-test-extract-params-clang-simple ()
  (let ((data "wchar_t * wmemchr(wchar_t *__p, wchar_t __c, size_t __n)"))
    (should (equal (company-ycmd--extract-params-clang data)
                   "(wchar_t *__p, wchar_t __c, size_t __n)"))))

(ert-deftest company-ycmd-test-extract-params-clang-template ()
  (let ((data "shared_ptr<_Tp> make_shared<typename _Tp>(_Args &&__args...)"))
    (should (equal (company-ycmd--extract-params-clang data)
                   "<typename _Tp>(_Args &&__args...)"))))

(ert-deftest company-ycmd-test-extract-params-clang-func-ptr ()
  (let ((data "void (*)(int) foo"))
    (should (equal (company-ycmd--extract-params-clang data)
                   "(*)(int)"))))

(ert-deftest company-ycmd-test-extract-params-clang-null ()
  (let ((data "char "))
    (should (null (company-ycmd--extract-params-clang data)))))

(ert-deftest company-ycmd-test-in-include-quoted-true ()
  (with-temp-buffer
    (insert "#include \"foo.h\"")
    (goto-char 15)
    (should (company-ycmd--in-include))))

(ert-deftest company-ycmd-test-in-include-angle-bracket-true ()
  (with-temp-buffer
    (insert "#include <foo>")
    (goto-char 14)
    (should (company-ycmd--in-include))))

(ert-deftest company-ycmd-test-in-include-false ()
  (with-temp-buffer
    (insert "#include \"foo.h\"")
    (should (not (company-ycmd--in-include)))))

(defun flycheck-ycmd-test-ycmd-mode ()
  (let ((ycmd-global-config ycmd-test-global-conf)
        (ycmd-extra-conf-handler 'load)
        (ycmd-parse-conditions nil))
    (ycmd-mode +1)
    (flycheck-ycmd-setup)
    (deferred:sync!
      (ycmd-notify-file-ready-to-parse))))

(ert-deftest flycheck-ycmd-test-error ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-error.cpp" 'flycheck-ycmd-test-ycmd-mode
     '(3 15 error "expected ';' at end of declaration list (FixIt)"
         :checker ycmd))))

(ert-deftest flycheck-ycmd-test-warning ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-warning.cpp" 'flycheck-ycmd-test-ycmd-mode
     '(5 13 warning "unused variable 'a'" :checker ycmd))))

(flycheck-ert-initialize ycmd-test-resources-location)


(provide 'ycmd-test)

;;; ycmd-test.el ends here
