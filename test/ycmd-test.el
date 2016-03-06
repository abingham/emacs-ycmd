;;; ycmd-test.el --- tests for ycmd.el -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014-2016 Austin Bingham, Peter Vasil
;;
;; Authors: Austin Bingham <austin.bingham@gmail.com>
;;          Peter Vasil <mail@petervasil.net>
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

(defconst ycmd-test-extra-conf
  (f-join ycmd-test-resources-location ".ycm_extra_conf.py"))

(defun ycmd-test-mode ()
  "Setup `ycmd-mode' for test in current buffer."
  (let ((ycmd-parse-conditions nil))
    (ycmd-mode)
    (deferred:sync!
      (ycmd-load-conf-file ycmd-test-extra-conf))
    (deferred:sync!
      (ycmd-notify-file-ready-to-parse))))

(defun ycmd-test-prepare-buffer-for-file (filename mode)
  "Read FILENAME into new temporary buffer and return in.
Put the buffer into MODE, and run `ycmd-test-mode' on it."
  (let ((buff (find-file-noselect
               (f-join ycmd-test-resources-location filename))))
    (with-current-buffer buff
      (delay-mode-hooks (funcall mode))
      (ycmd-test-mode))
    buff))

(defmacro ycmd-ert-test-deferred-request (name filename mode &rest keys-and-body)
  "Define a test case for a deferred request to the ycmd server.

NAME is a symbol denoting the local name of the test.  The test
itself is named `ycmd-test-NAME'.  FILENAME is the input file with
the `major-mode' MODE.  The rest should contain the keywords
`:request-func' specifying the function with the request to the
ycmd server.  `:line' and `:column' is the file location for the
request.  The remaining forms of KEYS-AND-BODY are used to
evaluate the server's response."
  (declare (indent 3) (debug t))
  (let* ((full-name (intern (format "ycmd-test-%s" name)))
         (keys-and-body (ert--parse-keys-and-body keys-and-body))
         (body (cadr keys-and-body))
         (keys (car keys-and-body)))
    `(ert-deftest ,full-name ()
       (let* ((buff (ycmd-test-prepare-buffer-for-file
                     ,filename ,mode))
              (current-position
               (ycmd--col-line-to-position
                ,(plist-get keys :column)
                ,(plist-get keys :line)
                buff)))
         (deferred:sync!
           (deferred:$
             (funcall ,(plist-get keys :request-func)
                      buff current-position)
             (deferred:nextc it
               (lambda (response)
                 ,@body))))
         (kill-buffer buff)))))

(ycmd-ert-test-deferred-request get-completions "test.cpp" 'c++-mode
  :request-func 'ycmd-get-completions
  :line 8 :column 7
  (let ((start-col (assoc-default 'completion_start_column response))
        (completions (assoc-default 'completions response)))
    (should (some (lambda (c)
                    (string-equal
                     "llurp" (assoc-default 'insertion_text c)))
                  completions))
    (should (= start-col 7))))

(ycmd-ert-test-deferred-request goto-declaration "test-goto.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "GoToDeclaration")
  :line 9 :column 7
  (if (assoc-default 'exception response)
      (should nil)
    (progn
      (should (= (assoc-default 'column_num response) 10))
      (should (= (assoc-default 'line_num response) 2)))))

(ycmd-ert-test-deferred-request goto-definition "test-goto.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "GoToDefinition")
  :line 9 :column 7
  (if (assoc-default 'exception response)
      (should nil)
    (progn
      (should (= (assoc-default 'column_num response) 11))
      (should (= (assoc-default 'line_num response) 5)))))

(defun ycmd-test-fixit-handler (buffer response file-name)
  (let ((ycmd-confirm-fixit nil))
    (when (assoc-default 'fixits response)
      (with-current-buffer buffer
        (ycmd--handle-fixit-success response)
        (let ((actual (buffer-string))
              (expected (with-current-buffer
                            (find-file-noselect file-name)
                          (buffer-string))))
          (set-buffer-modified-p nil)
          (set-visited-file-name nil 'no-query)
          (string= actual expected))))))

(ycmd-ert-test-deferred-request fixit-insert1
    "test-fixit-cpp11-insert1.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 7 :column 6
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-insert1-expected.cpp")))

(ycmd-ert-test-deferred-request fixit-insert2
    "test-fixit-cpp11-insert2.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 7 :column 5
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-insert2-expected.cpp")))

(ycmd-ert-test-deferred-request fixit-delete
    "test-fixit-cpp11-delete.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 1 :column 6
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-delete-expected.cpp")))

(ycmd-ert-test-deferred-request fixit-replace
    "test-fixit-cpp11-replace.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 3 :column 6
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-replace-expected.cpp")))

(ycmd-ert-test-deferred-request fixit-delete-add
    "test-fixit-cpp11-delete-add.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 5 :column 3
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-delete-add-expected.cpp")))

(ycmd-ert-test-deferred-request fixit-multiple
    "test-fixit-cpp11-multiple.cpp" 'c++-mode
  :request-func (apply-partially #'ycmd--send-completer-command-request
                                 "FixIt")
  :line 2 :column 15
  (should
   (ycmd-test-fixit-handler
    buff response "test-fixit-cpp11-multiple-expected.cpp")))

(defmacro ycmd-test-with-buffer (filename mode &rest body)
  "Create temporary current buffer with FILENAME and MODE and execute BODY."
  (declare (indent 2) (debug t))
  `(let ((it (ycmd-test-prepare-buffer-for-file ,filename ,mode)))
     (save-excursion
       (with-current-buffer it
         ,@body))
     (kill-buffer it)))

(ert-deftest ycmd-test-col-line-to-position ()
  (ycmd-test-with-buffer "test-goto.cpp" 'c++-mode
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
                   (detailed_info . "foo(self, a, b)\n\nA function")
                   (extra_data
                    (location
                     (line_num . 6)
                     (column_num . 7)
                     (filepath . "/foo/bar.py")))
                   (extra_menu_info . "function: bar.Foo.foo")))
           (candidate (company-ycmd--construct-candidate-python data)))
      (should (string= (assoc-default 'insertion_text data)
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'meta "foo(self, a, b)" candidate))
      (should (ycmd-test-has-property-with-value 'params "(a, b)" candidate))
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

(ert-deftest company-ycmd-test-remove-self-from-function-args ()
  (let ((data "self, a, b"))
    (should (equal (company-ycmd--remove-self-from-function-args data)
                   "(a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-simple ()
  (let ((data "foo(self, a, b)"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "(a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-multiline ()
  (let ((data "foo(self, a,\nb)"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "(a, b)"))))

(ert-deftest company-ycmd-test-extract-meta-python-simple ()
  (let ((data "foo(self, a, b)"))
    (should (equal (company-ycmd--extract-meta-python data)
                   "foo(self, a, b)"))))

(ert-deftest company-ycmd-test-extract-meta-python-newline ()
  (let ((data "foo(self, a,\nb)"))
    (should (equal (company-ycmd--extract-meta-python data)
                   "foo(self, a, b)"))))

(ert-deftest company-ycmd-test-extract-meta-python-extended ()
  (let ((data "foo(self, a, b) -> bar\n\nbar"))
    (should (equal (company-ycmd--extract-meta-python data)
                   "foo(self, a, b) -> bar"))))

(ert-deftest company-ycmd-test-extract-meta-python-multiline ()
  (let ((data "foo(self, a,\nb) -> bar\n\nbar"))
    (should (equal (company-ycmd--extract-meta-python data)
                   "foo(self, a, b) -> bar"))))

(defun flycheck-ycmd-test-mode ()
  (flycheck-ycmd-setup)
  (ycmd-test-mode))

(ert-deftest flycheck-ycmd-test-error ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-error.cpp" 'flycheck-ycmd-test-mode
     '(3 15 error "expected ';' at end of declaration list (FixIt)"
         :checker ycmd))))

(ert-deftest flycheck-ycmd-test-warning ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-warning.cpp" 'flycheck-ycmd-test-mode
     '(5 13 warning "unused variable 'a'" :checker ycmd))))

(flycheck-ert-initialize ycmd-test-resources-location)


(provide 'ycmd-test)

;;; ycmd-test.el ends here
