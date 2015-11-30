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


(provide 'ycmd-test)

;;; ycmd-test.el ends here
