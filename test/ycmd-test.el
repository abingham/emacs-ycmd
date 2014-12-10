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

(defun ycmd-test-load-file (filename)
  (let* ((this-dir (f-dirname (f-this-file)))
         (filename (f-join this-dir filename)))
    (f-read filename)))

(setq ycmd-test-cpp-content
      (ycmd-test-load-file "test.cpp"))

(setq ycmd-test-goto-declaration-cpp-content
      (ycmd-test-load-file "test_goto_declaration.cpp"))

(defun ycmd-test-create-file (content)
  "Create a new temporary file and write CONTENT to it.

Returns buffer visiting new file."
  (let* ((filename (make-temp-file "ycmd-test"))
         (buff (create-file-buffer filename)))
    (with-current-buffer buff
      (set-visited-file-name filename)
      (erase-buffer)
      (insert content)
      (save-buffer))
    buff))

(defun ycmd-test-prepare-file (content mode)
  "Create a new temporary file containing CONTENT, put that file
into MODE, and wait for initial ycmd parsing of the file to
complete.

This has the side-effect of (re)starting ycmd.

Return the buffer.
"
  (lexical-let ((buff (ycmd-test-create-file content))
                (ycmd-extra-conf-handler 'load))
    (with-current-buffer buff
      (ycmd-open)
      (ycmd-mode t)
      (funcall mode)
      (while ycmd--notification-in-progress (sit-for 0.1)))
    buff))

(ert-deftest ycmd-test-completions ()
  "Test that completion candidates contain expected results."
  (lexical-let ((buff (ycmd-test-prepare-file ycmd-test-cpp-content 'c++-mode)))
    
    (deferred:sync!
      
      (deferred:$
        (ycmd-get-completions
         buff
         (ycmd--col-line-to-position 7 8 buff))
        
        (deferred:nextc it
          (lambda (completions)
            (let ((start-col (assoc-default 'completion_start_column completions))
                  (completions (assoc-default 'completions completions)))
              (should (some (lambda (c) (string-equal "llurp" (assoc-default 'insertion_text c))) completions))
              (should (= start-col 7)))))))
    
    (kill-buffer buff)))

(ert-deftest ycmd-test-goto-declaration ()
  "Test that goto-declaration works."
  (lexical-let ((buff (ycmd-test-prepare-file ycmd-test-goto-declaration-cpp-content 'c++-mode)))
    
    (deferred:sync!
      
      (deferred:$
        (ycmd--send-goto-request
         "GoToDeclaration"
         buff
         (ycmd--col-line-to-position 7 9 buff))
        
        (deferred:nextc it
          (lambda (location)
            (if (assoc-default 'exception location)
                (should nil)
              (progn
                (should (= (assoc-default 'column_num location) 11))
                (should (= (assoc-default 'line_num location) 5))))))))
    
    (kill-buffer buff)))

(provide 'ycmd-test)

;;; ycmd-test.el ends here
