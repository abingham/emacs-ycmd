;;; run.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords:

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

;;

;;; Code:

(defun ycmd-runs-this-script-p ()
  t)

(defvar ycmd-runner-file
  (if load-in-progress load-file-name (buffer-file-name)))

(defvar request-backend)

(defun ycmd-run-tests-main ()
  "Main entry point of the test runner."
  (let* ((load-prefer-newer t)
         (source-directory (locate-dominating-file ycmd-runner-file "Cask"))
         (pkg-rel-dir (format ".cask/%s/elpa" emacs-version))
         (python-path (executable-find "python")))
    (unless python-path
      (error "Python not found"))
    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s, built at %s"
             emacs-version (format-time-string "%F" emacs-build-time))

    (let ((debug-on-error t))
      (load (expand-file-name "ycmd" source-directory))
      (load (expand-file-name "company-ycmd" source-directory))
      (load (expand-file-name "flycheck-ycmd" source-directory))
      (load (expand-file-name "ycmd-eldoc" source-directory))
      (load (expand-file-name "ycmd-test" (file-name-directory ycmd-runner-file))))

    (let* ((debug-on-error t)
           (ycmd-path-raw (pop argv))
           (ycmd-path (if (f-absolute? ycmd-path-raw)
                          ycmd-path-raw
                        (expand-file-name ycmd-path-raw source-directory)))
           (ycmd-server-command (list python-path ycmd-path))
           (ert-selector (pop argv))
           (ycmd-request-backend (intern (pop argv)))
           (request-backend (if (memq ycmd-request-backend '(curl url-retrieve))
                                ycmd-request-backend
                              request-backend)))
      (message "request backend: %s" request-backend)
      (unless (f-exists? ycmd-path)
        (error "Ycmd path does not exist"))
      (ert-run-tests-batch-and-exit (and "ycmd-" ert-selector)))))

(when (and noninteractive (ycmd-runs-this-script-p))
  (ycmd-run-tests-main))

(provide 'run)
;;; run.el ends here
