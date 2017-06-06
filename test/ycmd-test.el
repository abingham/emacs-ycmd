;;; ycmd-test.el --- tests for ycmd.el -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014-2017 Austin Bingham, Peter Vasil
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


(require 'let-alist)
(require 'ert)
(require 'f)
(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ert)
(require 'flycheck-ycmd)
(require 'ycmd-eldoc)
(require 'macroexp)
(require 'go-mode)
(require 'rust-mode)
(require 'typescript-mode)

(eval-and-compile
  (defconst ycmd-test-location
    (file-name-directory (f-this-file)))

  (defconst ycmd-test-resources-location
    (f-join ycmd-test-location "resources"))

  (defconst ycmd-test-extra-conf
    (f-join ycmd-test-resources-location ".ycm_extra_conf.py")))

(defun ycmd-test-mode ()
  "Setup `ycmd-mode' for test in current buffer."
  (let ((ycmd-parse-conditions nil))
    (ycmd-open)
    (ycmd-wait-until-server-is-ready)
    (when (eq (ycmd--send-completer-available-request) t) ; retruns :json-false
      (ycmd-wait-until-server-is-ready 'include-subserver))
    (ycmd-mode)
    (ycmd-load-conf-file ycmd-test-extra-conf)
    (ycmd-deferred:sync!
     (ycmd--event-notification "BufferVisit")))
    (ycmd-deferred:sync!
     (ycmd-notify-file-ready-to-parse)))

(defmacro ycmd-ert-with-temp-buffer (mode &rest body)
  "Set MODE and eval BODY within a temporary buffer.

Like `flycheck-ert-with-temp-buffer', but sets MODE before
evaluating BODY."
  (declare (indent 1))
  `(flycheck-ert-with-temp-buffer
     (delay-mode-hooks (funcall ,mode))
     ,@body))

(defmacro ycmd-ert-with-resource-buffer (resource-file mode &rest body)
  (declare (indent 2))
  `(flycheck-ert-with-file-buffer
       (f-join ,ycmd-test-resources-location ,resource-file)
     (delay-mode-hooks (funcall ,mode))
     (ycmd-test-mode)
     ,@body
     (ycmd-close)))

(defmacro with-ycmd-test-mode (&rest body)
  `(progn
     (ycmd-test-mode)
     ,@body
     (ycmd-close)))

(defmacro ycmd-ert-deftest (name filename mode &rest keys-and-body)
  "Define a test case for a deferred request to the ycmd server.

NAME is a symbol denoting the local name of the test.  The test
itself is named `ycmd-test-NAME'.  FILENAME is the input file with
the `major-mode' MODE.  The rest should contain keywords.  `:line'
and `:column' is the file location for the request.  The remaining
forms of KEYS-AND-BODY are used to evaluate the server's
response."
  (declare (indent 3) (debug t))
  (let* ((full-name (intern (format "ycmd-test-%s" name)))
         (keys-and-body (ert--parse-keys-and-body keys-and-body))
         (body (cadr keys-and-body))
         (keys (car keys-and-body)))
    `(ert-deftest ,full-name ()
       (ycmd-ert-with-resource-buffer ,filename ,mode
         (let ((current-position
                (ycmd--col-line-to-position
                 ,(plist-get keys :column)
                 ,(plist-get keys :line)
                 (current-buffer))))
           (goto-char current-position)
           ,@body)))))

(defmacro ycmd-test-with-completer-command (subcommand &rest body)
  "Run a request with SUBCOMMAND and eval BODY with response."
  (declare (indent 1))
  `(ycmd-deferred:sync!
     (ycmd--run-completer-command ,subcommand
       (lambda (response)
         ,@body))))

(ycmd-ert-deftest get-defined-subcommands-cpp "test.cpp" 'c++-mode
  :line 1 :column 1
  (let ((commands '("ClearCompilationFlagCache" "FixIt" "GetDoc"
                    "GetDocImprecise" "GetParent" "GetType"
                    "GetTypeImprecise" "GoTo" "GoToDeclaration"
                    "GoToDefinition" "GoToImprecise" "GoToInclude"))
        (result (ycmd--get-defined-subcommands)))
    (should (equal result commands))))

(ycmd-ert-deftest get-defined-subcommands-python "test.py" 'python-mode
  :line 1 :column 1
  (let ((commands '("GetDoc" "GoTo" "GoToDeclaration" "GoToDefinition"
                    "GoToReferences" "RestartServer"))
        (result (ycmd--get-defined-subcommands)))
    (should (equal result commands))))

(ycmd-ert-deftest get-defined-subcommands-go "test.go" 'go-mode
  :line 1 :column 1
  (let ((commands '("GoTo" "GoToDeclaration" "GoToDefinition"
                    "RestartServer"))
        (result (ycmd--get-defined-subcommands)))
    (should (equal result commands))))

(ycmd-ert-deftest get-defined-subcommands-typescript "test.ts" 'typescript-mode
  :line 1 :column 1
  (let ((commands '("GetDoc" "GetType" "GoToDefinition" "GoToReferences"
                    "GoToType" "RefactorRename" "RestartServer"))
        (result (ycmd--get-defined-subcommands)))
    (should (equal result commands))))

(ycmd-ert-deftest get-defined-subcommands-javascript "simple_test.js" 'js-mode
  :line 1 :column 1
  (let ((commands '("GetDoc" "GetType" "GoTo" "GoToDefinition"
                    "GoToReferences" "RefactorRename" "RestartServer"))
        (result (ycmd--get-defined-subcommands)))
    (should (equal result commands))))

(defun ycmd-get-completions-sync ()
  (ycmd-deferred:sync!
   (ycmd-deferred:timeout 10
     (ycmd-get-completions))))

(ycmd-ert-deftest get-completions-cpp "test.cpp" 'c++-mode
  :line 8 :column 7
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (should (cl-some (lambda (c)
                         (string-equal
                          "llurp" (cdr (assq 'insertion_text c))))
                       .completions))
      (should (= .completion_start_column 7)))))

(ycmd-ert-deftest get-completions-cpp-unicode "test-unicode.cpp" 'c++-mode
  :line 8 :column 7
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (should (cl-some (lambda (c)
                         (string-equal
                          "bär" (cdr (assq 'insertion_text c))))
                       .completions))
      (should (= .completion_start_column 7)))))

(ert-deftest ycmd-test-no-semantic-completion-cpp ()
  (let ((ycmd-auto-trigger-semantic-completion))
    (ycmd-ert-with-resource-buffer "test.cpp" 'c++-mode
      (let ((current-position
             (ycmd--col-line-to-position
              7 8 (current-buffer))))
        (goto-char current-position)
        (let ((response (ycmd-get-completions-sync)))
          (let-alist response
            (should-not .completions)))))))

(ycmd-ert-deftest completion-at-point-clang
    "test-completion-at-point.cpp" 'c++-mode
  :line 8 :column 8
  (skip-unless (not (version-list-< (version-to-list emacs-version) '(24 4))))
  (let ((expected (f-read "test-completion-at-point-expected.cpp"))
        (completion-at-point-functions (list #'ycmd-complete-at-point)))
    (completion-at-point)
    (should (string= (buffer-string) expected))
    (set-buffer-modified-p nil)
    (set-visited-file-name nil 'no-query)))

(ycmd-ert-deftest goto-declaration "test-goto.cpp" 'c++-mode
  :line 9 :column 7
  (ycmd-test-with-completer-command "GoToDeclaration"
    (let-alist response
      (if .exception
          (should nil)
        (should (= .column_num 10))
        (should (= .line_num 2))))))

(ycmd-ert-deftest goto-definition "test-goto.cpp" 'c++-mode
  :line 9 :column 7
  (ycmd-test-with-completer-command "GoToDefinition"
    (let-alist response
      (if .exception
          (should nil)
        (should (= .column_num 11))
        (should (= .line_num 5))))))

(ycmd-ert-deftest goto-references-not-available "test-goto.cpp" 'c++-mode
  :line 9 :column 7
  ;; we need to temporarly overwrite `message' in order to
  ;; check the return string
  (cl-letf (((symbol-function 'message)
             (lambda (format-string &rest args)
              (apply 'format format-string args))))
    (let ((result (ycmd-deferred:sync!
                    (ycmd--run-completer-command "GoToReferences" nil))))
      (should (string= "GoToReferences is not supported by current Completer"
                       result)))))

(ycmd-ert-deftest get-completions-python "test.py" 'python-mode
  :line 7 :column 3
  ;; (skip-unless nil)
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (should (cl-some (lambda (c)
                         (string-equal
                          "a" (cdr (assq 'insertion_text c))))
                       .completions))
      (should (cl-some (lambda (c)
                         (string-equal
                          "b" (cdr (assq 'insertion_text c))))
                       .completions))
      (should (= .completion_start_column 3)))))

(ycmd-ert-deftest get-completions-go "test.go" 'go-mode
  :line 9 :column 10
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (let ((c (nth 0 .completions)))
        (should (string= "Logger" (cdr (assq 'insertion_text c))))
        (should (= .completion_start_column 6))))))

(ycmd-ert-deftest get-completions-javascript "simple_test.js" 'js-mode
  :line 13 :column 43
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (should (cl-some (lambda (c)
                         (string-equal
                          "a_simple_function" (cdr (assq 'insertion_text c))))
                       .completions)))))

(ycmd-ert-deftest get-completions-typescript "test.ts" 'typescript-mode
  :line 16 :column 6
  (let ((response (ycmd-get-completions-sync)))
    (let-alist response
      (should (cl-some (lambda (c)
                         (string-equal
                          "methodA" (cdr (assq 'insertion_text c))))
                       .completions)))))

(ert-deftest ycmd-test-fixit-same-location-false ()
  (let ((data '(((chunks . (((replacement_text . "foo"))))
                 (location (filepath . "test.cpp")
                           (column_num . 1) (line_num . 2)))
                ((chunks . (((replacement_text . "bar"))))
                 (location (filepath . "test.cpp")
                           (column_num . 3) (line_num . 4))))))
    (should-not (ycmd--fixits-have-same-location-p data))))

(ert-deftest ycmd-test-fixit-same-location-true ()
  (let ((data '(((chunks . (((replacement_text . "foo"))))
                 (location (filepath . "test.cpp")
                           (column_num . 1) (line_num . 2)))
                ((chunks . (((replacement_text . "bar"))))
                 (location (filepath . "test.cpp")
                           (column_num . 1) (line_num . 2))))))
    (should (ycmd--fixits-have-same-location-p data))))

(defun ycmd-test-fixit-handler (response file-name expected-num)
  (when (cdr (assq 'fixits response))
    (let ((ycmd-confirm-fixit nil))
      ;; Override ycmd--show-fixits in order to return number of
      ;; fixits for same location.
      (cl-letf (((symbol-function 'ycmd--show-fixits)
                 (lambda (fixits &optional title)
                   (cons :multiple-fixits (length fixits)))))
        (let ((return-val (ycmd--handle-fixit-response response)))
          (if (and (consp return-val)
                   (eq (car return-val) :multiple-fixits))
              (eq (cdr return-val) expected-num)
            (let ((actual (buffer-string))
                  (expected (f-read file-name)))
              (set-buffer-modified-p nil)
              (set-visited-file-name nil 'no-query)
              (string= actual expected))))))))

(defmacro ycmd-ert-deftest-fixit (name mode &rest keys)
  (declare (indent 2) (debug t))
  (let* ((line (plist-get keys :line))
         (column (plist-get keys :column)))
    `(ycmd-ert-deftest ,name ,(plist-get keys :filename) ,mode
       :line ,line :column ,column :disabled ,(plist-get keys :disabled)
       (ycmd-test-with-completer-command "FixIt"
         (should
          (ycmd-test-fixit-handler
           response
           ,(plist-get keys :filename-expected)
           ,(plist-get keys :expected-number-fixits)))))))

(ycmd-ert-deftest-fixit fixit-cpp-insert1 'c++-mode
  :filename "test-fixit-cpp11-insert1.cpp"
  :filename-expected "test-fixit-cpp11-insert1-expected.cpp"
  :line 7 :column 6)

(ycmd-ert-deftest-fixit fixit-cpp-insert2 'c++-mode
  :filename "test-fixit-cpp11-insert2.cpp"
  :filename-expected "test-fixit-cpp11-insert2-expected.cpp"
  :line 7 :column 5)

(ycmd-ert-deftest-fixit fixit-cpp-delete 'c++-mode
  :filename "test-fixit-cpp11-delete.cpp"
  :filename-expected "test-fixit-cpp11-delete-expected.cpp"
  :line 1 :column 6)

(ycmd-ert-deftest-fixit fixit-cpp-replace 'c++-mode
  :filename "test-fixit-cpp11-replace.cpp"
  :filename-expected "test-fixit-cpp11-replace-expected.cpp"
  :line 3 :column 6)

(ycmd-ert-deftest-fixit fixit-cpp-delete-add 'c++-mode
  :filename "test-fixit-cpp11-delete-add.cpp"
  :filename-expected "test-fixit-cpp11-delete-add-expected.cpp"
  :line 5 :column 3)

(ycmd-ert-deftest-fixit fixit-cpp-multiple 'c++-mode
  :filename "test-fixit-cpp11-multiple.cpp"
  :filename-expected "test-fixit-cpp11-multiple-expected.cpp"
  :line 2 :column 15)

(ycmd-ert-deftest-fixit fixit-cpp-notes 'c++-mode
  :filename "test-fixit-cpp11-notes.cpp"
  :line 4 :column 12
  :expected-number-fixits 2)

(defun ycmd-test-refactor-rename-handler (response file-names-alist)
  (when (cdr (assq 'fixits response))
    (let ((ycmd-confirm-fixit nil))
      (ycmd--handle-fixit-response response)
      (cl-every (lambda (f)
                  (let ((buffer
                         (find-file-noselect
                          (f-join ycmd-test-resources-location (car f))))
                        (expected
                         (f-read
                          (f-join ycmd-test-resources-location (cdr f)))))
                    (with-current-buffer buffer
                      (let ((actual (buffer-string)))
                        (set-buffer-modified-p nil)
                        (set-visited-file-name nil 'no-query)
                        (string= actual expected)))))
                file-names-alist))))

(ert-deftest ycmd-test-refactor-rename-simple ()
  (let* ((file-path (f-join ycmd-test-resources-location "simple_test.js"))
         (data `((fixits . (((chunks . (((range
                                          (start (filepath . ,file-path) (column_num . 5) (line_num . 1))
                                          (end (filepath . ,file-path) (column_num . 22) (line_num . 1)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path) (column_num . 25) (line_num . 13))
                                          (end (filepath . ,file-path) (column_num . 42) (line_num . 13)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path) (column_num . 24) (line_num . 14))
                                          (end (filepath . ,file-path) (column_num . 41) (line_num . 14)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path) (column_num . 24) (line_num . 15))
                                          (end (filepath . ,file-path) (column_num . 41) (line_num . 15)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path) (column_num . 7) (line_num . 21))
                                          (end (filepath . ,file-path) (column_num . 24) (line_num . 21)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path) (column_num . 28) (line_num . 21))
                                          (end (filepath . ,file-path) (column_num . 45) (line_num . 21)))
                                         (replacement_text . "test"))))
                             (text . "")
                             (location (filepath . ,file-path) (column_num . 34) (line_num . 15))))))))
    (should (ycmd-test-refactor-rename-handler
             data '(("simple_test.js" . "simple_test-expected.js"))))))

(ert-deftest ycmd-test-refactor-rename-multiple-files ()
  (let* ((file-path-1 (f-join ycmd-test-resources-location "file1.js"))
         (file-path-2 (f-join ycmd-test-resources-location "file2.js"))
         (file-path-3 (f-join ycmd-test-resources-location "file3.js"))
         (data `((fixits . (((chunks . (((range
                                          (start (filepath . ,file-path-1) (column_num . 5) (line_num . 1))
                                          (end (filepath . ,file-path-1) (column_num . 11) (line_num . 1)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path-1) (column_num . 14) (line_num . 3))
                                          (end (filepath . ,file-path-1) (column_num . 20) (line_num . 3)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path-2) (column_num . 14) (line_num . 2))
                                          (end (filepath . ,file-path-2) (column_num . 20) (line_num . 2)))
                                         (replacement_text . "test"))
                                        ((range
                                          (start (filepath . ,file-path-3) (column_num . 12) (line_num . 3))
                                          (end (filepath . ,file-path-3) (column_num . 18) (line_num . 3)))
                                         (replacement_text . "test"))))
                             (text . "")
                             (location (filepath . ,file-path-1) (column_num . 5) (line_num . 1))))))))
    (should (ycmd-test-refactor-rename-handler
             data '(("file1.js" . "file1-expected.js")
                    ("file2.js" . "file2-expected.js")
                    ("file3.js" . "file3-expected.js"))))))

(ert-deftest ycmd-test-col-line-to-position ()
  (ycmd-ert-with-resource-buffer "test-goto.cpp" 'c++-mode
    (should (= (ycmd--col-line-to-position 10 2) 23))))

(defun ycmd-test-has-property-with-value (property value item)
  (let ((pred (pcase value
                ((pred stringp) 'string=)
                (`nil 'eq)
                (_ '=))))
    (funcall pred value (get-text-property 0 property item))))

(ert-deftest company-ycmd-test-construct-candidate-clang-function ()
  (ycmd-ert-with-temp-buffer 'c++-mode
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
      (should (string= (cdr (assq 'insertion_text data))
                       (substring-no-properties candidate-1)))
      (should (ycmd-test-has-property-with-value 'kind "fn" candidate-1))
      (should (ycmd-test-has-property-with-value 'meta "int foo( int i )" candidate-1))
      (should (ycmd-test-has-property-with-value 'return_type "int" candidate-1))
      (should (ycmd-test-has-property-with-value 'params "( int i )" candidate-1))
      (should (ycmd-test-has-property-with-value 'doc "A docstring" candidate-1))
      (should (string= (cdr (assq 'insertion_text data))
                       (substring-no-properties candidate-2)))
      (should (ycmd-test-has-property-with-value 'kind "fn" candidate-2))
      (should (ycmd-test-has-property-with-value 'meta "void foo()" candidate-2))
      (should (ycmd-test-has-property-with-value 'return_type "void" candidate-2))
      (should (ycmd-test-has-property-with-value 'params "()" candidate-2))
      (should (ycmd-test-has-property-with-value 'doc "A docstring" candidate-2)))))

(ert-deftest company-ycmd-test-construct-candidate-clang-union ()
  (ycmd-ert-with-temp-buffer 'c++-mode
    (let* ((data '((menu_text . "foo")
                   (insertion_text . "foo")
                   (detailed_info . "union (anonymous) foo\n")
                   (extra_menu_info . "union (anonymous)")
                   (kind . "MEMBER")))
           (candidate (car (company-ycmd--construct-candidate-clang data))))
      (should (string= "foo" (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "member" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "union (anonymous) foo" candidate))
      (should-not (get-text-property 0 'params candidate))
      (should (ycmd-test-has-property-with-value
               'return_type "union (anonymous)" candidate)))))

(ert-deftest company-ycmd-test-construct-candidtate-clang-class ()
  (ycmd-ert-with-temp-buffer 'c++-mode
    (let* ((data '((insertion_text . "A")
                   (detailed_info . " A\n A()\n A( int a )\n A( int a, int b )\n")
                   (menu_text . "A")
                   (kind . "CLASS")))
           (params-expected '(nil "()" "( int a )" "( int a, int b )"))
           (meta-expected '(" A" " A()" " A( int a )" " A( int a, int b )"))
           (candidates (company-ycmd--construct-candidate-clang data)))
      (should (= 4 (length candidates)))
      (should (cl-every (lambda (expected candidate)
                          (ycmd-test-has-property-with-value 'params expected candidate))
                        params-expected (reverse candidates)))
      (should (cl-every (lambda (expected candidate)
                          (ycmd-test-has-property-with-value 'meta expected candidate))
                        meta-expected (reverse candidates)))
      (should (cl-every (lambda (candidate)
                          (ycmd-test-has-property-with-value 'return_type "" candidate))
                        candidates))
      (should (cl-every (lambda (candidate)
                          (ycmd-test-has-property-with-value 'kind "class" candidate))
                        candidates)))))

(ert-deftest company-ycmd-test-construct-candidate-go ()
  (ycmd-ert-with-temp-buffer 'go-mode
    (let* ((data '((menu_text . "Print")
                   (insertion_text . "Print")
                   (detailed_info . "Print func(a ...interface{}) (n int, err error) func")
                   (extra_menu_info . "func(a ...interface{}) (n int, err error)")
                   (kind . "func")))
           (candidate (company-ycmd--construct-candidate-go data)))
      (should (string= (cdr (assq 'insertion_text data))
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "func" candidate))
      (should (ycmd-test-has-property-with-value 'params "(a ...interface{})" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "func Print(a ...interface{}) (n int, err error)" candidate))
      (should (ycmd-test-has-property-with-value
               'return_type "(n int, err error)" candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-python ()
  (ycmd-ert-with-temp-buffer 'python-mode
    (let* ((data '((insertion_text . "foo")
                   (detailed_info . "foo(self, a, b)\n\nA function")
                   (extra_data
                    (location
                     (line_num . 6)
                     (column_num . 7)
                     (filepath . "/foo/bar.py")))
                   (extra_menu_info . "def bar.Foo.foo")))
           (candidate (company-ycmd--construct-candidate-python data)))
      (let-alist data
        (should (string= .insertion_text (substring-no-properties candidate)))
        (should (ycmd-test-has-property-with-value 'meta "foo(self, a, b)" candidate))
        (should (ycmd-test-has-property-with-value 'params "(self, a, b)" candidate))
        (should (ycmd-test-has-property-with-value 'doc .detailed_info candidate))
        (should (ycmd-test-has-property-with-value 'kind .extra_menu_info candidate))
        (should (ycmd-test-has-property-with-value
                 'filepath .extra_data.location.filepath candidate))
        (should (ycmd-test-has-property-with-value
                 'line_num .extra_data.location.line_num candidate))))))

(ert-deftest company-ycmd-test-construct-candidate-rust-function ()
  (ycmd-ert-with-temp-buffer 'rust-mode
    (let* ((data '((insertion_text . "foo")
                   (kind . "Function")
                   (extra_data
                    (location
                     (line_num . 40)
                     (column_num . 8)
                     (filepath . "/foo/bar.rs")))
                   (extra_menu_info . "fn foo(&self, x: f64) -> f64")))
           (candidate (company-ycmd--construct-candidate-rust data)))
      (let-alist data
        (should (string= .insertion_text (substring-no-properties candidate)))
        (should (ycmd-test-has-property-with-value 'meta .extra_menu_info candidate))
        (should (ycmd-test-has-property-with-value 'kind .kind candidate))
        (should (ycmd-test-has-property-with-value
                 'annotation (concat "(&self, x: f64) -> f64 [" .kind "]") candidate))
        (should (ycmd-test-has-property-with-value 'params "(&self, x: f64)" candidate))
        (should (ycmd-test-has-property-with-value
                 'filepath .extra_data.location.filepath candidate))
        (should (ycmd-test-has-property-with-value
                 'line_num .extra_data.location.line_num candidate))
        (should (ycmd-test-has-property-with-value
                 'column_num .extra_data.location.column_num candidate))))))

(ert-deftest company-ycmd-test-construct-candidate-rust-struct ()
  (ycmd-ert-with-temp-buffer 'rust-mode
    (let* ((data '((insertion_text . "foo")
                   (kind . "StructField")
                   (extra_data
                    (location
                     (line_num . 40)
                     (column_num . 8)
                     (filepath . "/foo/bar.rs")))
                   (extra_menu_info . "&str")))
           (candidate (company-ycmd--construct-candidate-rust data)))
      (let-alist data
        (should (string= .insertion_text (substring-no-properties candidate)))
        (should (ycmd-test-has-property-with-value 'meta .extra_menu_info candidate))
        (should (ycmd-test-has-property-with-value 'kind .kind candidate))
        (should (ycmd-test-has-property-with-value
                 'annotation (concat " &str [" .kind "]") candidate))
        (should (ycmd-test-has-property-with-value
                 'filepath .extra_data.location.filepath candidate))
        (should (ycmd-test-has-property-with-value
                 'line_num .extra_data.location.line_num candidate))
        (should (ycmd-test-has-property-with-value
                 'column_num .extra_data.location.column_num candidate))))))

(ert-deftest company-ycmd-test-construct-candidate-rust-module ()
  (ycmd-ert-with-temp-buffer 'rust-mode
    (let* ((data '((insertion_text . "foo")
                   (kind . "Module")
                   (extra_data
                    (location
                     (line_num . 1)
                     (filepath . "/foobar/bar/foo.rs")))
                   (extra_menu_info . "/foobar/bar/foo.rs")))
           (candidate (company-ycmd--construct-candidate-rust data)))
      (let-alist data
        (should (string= .insertion_text (substring-no-properties candidate)))
        (should (ycmd-test-has-property-with-value 'meta .extra_menu_info candidate))
        (should (ycmd-test-has-property-with-value 'kind .kind candidate))
        (should (ycmd-test-has-property-with-value
                 'annotation (concat " bar/foo.rs [" .kind "]") candidate))
        (should (ycmd-test-has-property-with-value
                 'filepath .extra_data.location.filepath candidate))
        (should (ycmd-test-has-property-with-value
                 'column_num .extra_data.location.column_num candidate))))))

(ert-deftest company-ycmd-test-construct-candidate-javascript ()
  (ycmd-ert-with-temp-buffer 'js-mode
    (let* ((data '((insertion_text . "a_function")
                   (extra_menu_info . "fn(param: ?) -> string")
                   (detailed_info . "fn(param: ?) -> string\nReturns a string.")))
           (candidate (company-ycmd--construct-candidate-javascript data)))
      (should (string= "a_function" (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "fn" candidate))
      (should (ycmd-test-has-property-with-value 'meta "fn(param: ?) -> string" candidate))
      (should (ycmd-test-has-property-with-value 'return_type "string" candidate))
      (should (ycmd-test-has-property-with-value 'params "(param: ?)" candidate))
      (should (ycmd-test-has-property-with-value
               'doc "fn(param: ?) -> string\nReturns a string." candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-typescript-simple ()
  (ycmd-ert-with-temp-buffer 'typescript-mode
    (let* ((data '((insertion_text . "Foo")
                   (kind . "class")
                   (extra_data . "class")
                   (menu_text . "Foo")))
           (candidate (company-ycmd--construct-candidate-typescript data)))
      (should (string= "Foo" (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "class" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "(class) Foo" candidate))
      (should (ycmd-test-has-property-with-value 'params nil candidate))
      (should (ycmd-test-has-property-with-value 'return_type nil candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-typescript-method ()
  (ycmd-ert-with-temp-buffer 'typescript-mode
    (let* ((data '((insertion_text . "testMethod")
                   (kind . "method")
                   (menu_text . "testMethod (method) Bar.testMethod(): void")))
           (candidate (company-ycmd--construct-candidate-typescript data)))
      (should (string= "testMethod" (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "method" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "(method) Bar.testMethod(): void" candidate))
      (should (ycmd-test-has-property-with-value 'params "()" candidate))
      (should (ycmd-test-has-property-with-value 'return_type "void" candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-typescript-method-2 ()
  (ycmd-ert-with-temp-buffer 'typescript-mode
    (let* ((data '((insertion_text . "foo")
                   (kind . "method")
                   (menu_text . "foo   (method) Function.foo(thisArg: any, argArray?: any): any")))
           (candidate (company-ycmd--construct-candidate-typescript data)))
      (should (string= "foo" (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value 'kind "method" candidate))
      (should (ycmd-test-has-property-with-value
               'meta "(method) Function.foo(thisArg: any, argArray?: any): any" candidate))
      (should (ycmd-test-has-property-with-value 'params "(thisArg: any, argArray?: any)" candidate))
      (should (ycmd-test-has-property-with-value 'return_type "any" candidate)))))

(ert-deftest company-ycmd-test-construct-candidate-generic ()
  (ycmd-ert-with-temp-buffer 'c++-mode
    (let* ((data '((insertion_text . "foo")
                   (extra_menu_info . "[ID]")))
           (candidate (company-ycmd--construct-candidate-generic data)))
      (should (string= (cdr (assq 'insertion_text data))
                       (substring-no-properties candidate)))
      (should (ycmd-test-has-property-with-value
               'return_type (cdr (assq 'extra_menu_info data)) candidate)))))

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
  (let ((data "(self, a, b)"))
    (should (equal (company-ycmd--remove-self-from-function-args data)
                   "(a, b)"))))

(ert-deftest company-ycmd-test-remove-template-args-from-function-args ()
  (let ((data "<'a, T>(a, b)"))
    (should (equal (company-ycmd--remove-template-args-from-function-args data)
                   "(a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-simple ()
  (let ((data "foo(self, a, b)"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "(self, a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-multiline ()
  (let ((data "foo(self, a,\nb)"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "(self, a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-with-docstring ()
  (let ((data "foo(self, a, b) -> A docstring"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "(self, a, b)"))))

(ert-deftest company-ycmd-test-extract-params-python-with-docstring-no-params-1 ()
  (let ((data "foo() -> A docstring"))
    (should (equal (company-ycmd--extract-params-python data "foo")
                   "()"))))

(ert-deftest company-ycmd-test-extract-params-python-with-docstring-no-params-2 ()
  (let ((data "x.__foo__() <==> foo(x)"))
    (should (equal (company-ycmd--extract-params-python data "__foo__")
                   "()"))))

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

(defun ycmd-test-eldoc-func ()
  (ycmd-eldoc--cache-store nil nil)
  (ycmd-deferred:sync!
    (deferred:next
      (lambda () (or (ycmd-eldoc--info-at-point) "")))))

(ycmd-ert-deftest eldoc-info-at-point-on-keyword "test-eldoc.cpp" 'c++-mode
  :line 8 :column 8
  (should (equal (substring-no-properties (ycmd-test-eldoc-func))
                 "void bar( int x )")))
(ycmd-ert-deftest eldoc-info-at-point-between-par "test-eldoc.cpp" 'c++-mode
  :line 8 :column 11
  (should (equal (substring-no-properties (ycmd-test-eldoc-func))
                 "void bar( int x )")))

(ycmd-ert-deftest eldoc-force-semantic-info-fail "test-eldoc.cpp" 'c++-mode
  :line 8 :column 5
  (let ((ycmd-eldoc-always-semantic-server-query-modes nil)
        (ycmd-force-semantic-completion nil)
        (ycmd-eldoc--get-type-supported-p nil))
    (should-not (ycmd-test-eldoc-func))))

(ycmd-ert-deftest eldoc-force-semantic-info "test-eldoc.cpp" 'c++-mode
  :line 8 :column 5
  (skip-unless (not (version-list-< (version-to-list emacs-version) '(24 4))))
  (let ((ycmd-eldoc-always-semantic-server-query-modes t)
        (ycmd-force-semantic-completion nil))
    (should (equal (substring-no-properties (ycmd-test-eldoc-func))
                   "Foo l"))))

(ycmd-ert-deftest semantic-completer-available "test-eldoc.cpp" 'c++-mode
  :line 8 :column 5
  (should (eq (ycmd-semantic-completer-available?) t)))

(ycmd-ert-deftest semantic-completer-not-available "test-eldoc.el" 'emacs-lisp-mode
  :line 1 :column 3
  (should (eq (ycmd-semantic-completer-available?) 'none)))

(ert-deftest ycmd-test-not-running ()
  (should-not (ycmd-running?)))

(ert-deftest ycmd-test-json-encode ()
  (let ((data '((foo))))
    (should (string= (ycmd--json-encode data)
                     "{\"foo\":{}}"))))

(ert-deftest ycmd-test-location-data-predicate ()
  (let ((data '((filepath . ,file-path) (column_num . 34) (line_num . 15))))
    (should (ycmd--location-data? data))
    (should-not (ycmd--location-data? (list data)))))

(ert-deftest ycmd-test-filter-and-sort-candidates1 ()
  (skip-unless (not (version-list-< (version-to-list emacs-version) '(24 4))))
  (ycmd-ert-with-temp-buffer 'fundamental-mode
    (with-ycmd-test-mode
     (let* ((data '(("candidates" . (((prop1 . aoo) (prop2 . bar))
                                     ((prop1 . bfo) (prop2 . zoo))
                                     ((prop1 . cfo) (prop2 . moo))))
                    ("sort_property" . "prop1")
                    ("query" . "fo")))
            (result (ycmd-filter-and-sort-candidates data)))
       (should (string= (cdr (assq 'prop1 (nth 0 result))) "bfo"))
       (should (string= (cdr (assq 'prop1 (nth 1 result))) "cfo"))))))

(ert-deftest ycmd-test-filter-and-sort-candidates2 ()
  (ycmd-ert-with-temp-buffer 'fundamental-mode
    (with-ycmd-test-mode
     (let* ((data '(("candidates" . (bfo aoo cfo))
                    ("sort_property" . "")
                    ("query" . "fo")))
            (result (ycmd-filter-and-sort-candidates data)))
       (should (string= (nth 0 result) "bfo"))
       (should (string= (nth 1 result) "cfo"))))))

(defun flycheck-ycmd-test-mode ()
  (flycheck-ycmd-setup)
  (ycmd-test-mode))

(ert-deftest flycheck-ycmd-test-error ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-error.cpp" 'flycheck-ycmd-test-mode
     '(3 15 error "expected ';' at end of declaration list (FixIt available)"
         :checker ycmd))))

(ert-deftest flycheck-ycmd-test-warning ()
  (let ((flycheck-checkers '(ycmd)))
    (flycheck-ert-should-syntax-check
     "test-warning.cpp" 'flycheck-ycmd-test-mode
     '(5 13 warning "unused variable 'a'" :checker ycmd))))

(flycheck-ert-initialize ycmd-test-resources-location)

(provide 'ycmd-test)

;;; ycmd-test.el ends here
