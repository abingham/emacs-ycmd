;;; ycmd.el --- emacs bindings to the ycmd completion server
;;
;; Copyright (c) 2014 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((request "0.2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd is a modular server that provides completion for C/C++/ObjC
;; and Python, among other languages. This module provides an emacs
;; client for that server.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
;;
;; ycmd depends on the following packages:
;;
;;   request
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'ycmd)
;;
;; Basic usage:
;;
;; To start a ycmd server, use 'ycmd-open. Before you do anything else, you need to instruct it to load a configuration file with 'ycmd-load-conf-file. Then you can use 'ycmd-get-completions to get completions at some point in a file. For example:
;;
;;   (ycmd-open "/full/path/to/some/.ycm_extra_conf.json")
;;   (ycmd-get-completions (point))
;;
;; You can use 'ycmd-display-completions to toy around with completion
;; interactively and see the shape of the structures in use.
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

(require 'hmac-def)
(require 'hmac-md5) ; provides encode-hex-string
(require 'json)
(require 'request)

(defcustom ycmd-host "127.0.0.1"
  "The host on which the ycmd server is running."
  :type '(string)
  :group 'ycmd)

; TODO: Figure out the best default value for this.
(defcustom ycmd-server-command '("python" "/Users/sixtynorth/projects/ycmd/ycmd")
  "The name of the ycmd server program. This may be a single
string or a list."
  :type '(string)
  :group 'ycmd)

(defcustom ycmd-server-args (list "--log" "info")
  "Extra arguments to pass to the ycmd server."
  :type '()
  :group 'ycmd)

(defvar ycmd-server-actual-port 0
  "The actual port being used by the ycmd server. This is set
  based on the output from the server itself.")

(defconst ycmd-hmac-secret nil
  "This is populated with the hmac secret of the current
  connection. Users should never need to modify this, hence the
  defconst. It is not, however, treated as a constant by this
  code. This value gets set in ycmd-open.")

(defun ycmd-generate-hmac-secret ()
  (let ((result '()))
    (dotimes (x 16 result)
      (setq result (cons (byte-to-string (random 256)) result)))
    (apply 'concat result)))

(defconst ycmd-server-process "ycmd-server"
  "The emacs name of the server process. This is used by
  functions like start-process, get-process, and delete-process.")

(defun ycmd-json-encode (obj)
  "A version of json-encode that uses {} instead of null for nil
values. This produces output for empty alists that ycmd expects."
  ;; TODO: replace flat with cl-flet or cl-letf
  (flet ((json-encode-keyword (k) (cond ((eq k t)          "true")
                                        ((eq k json-false) "false")
                                        ((eq k json-null)  "{}"))))
    (json-encode obj)))

(define-hmac-function ycmd-hmac-function
  (lambda (x) (secure-hash 'sha256 x nil nil 1))
  64 64)

(defun ycmd-options-contents (hmac-secret)
  (let ((hmac-secret (base64-encode-string hmac-secret)))
    `((filetype_blacklist (vimwiki . 1) (mail . 1) (qf . 1) (tagbar . 1) (unite . 1) (infolog . 1) (notes . 1) (text . 1) (pandoc . 1) (markdown . 1))
      (auto_start_csharp_server . 1)
      (filetype_whitelist (* . 1))
      (csharp_server_port . 2000)
      (seed_identifiers_with_syntax . 0)
      (auto_stop_csharp_server . 1)
      (max_diagnostics_to_display . 30)
      (min_num_identifier_candidate_chars . 0)
      (use_ultisnips_completer . 1)
      (complete_in_strings . 1)
      (complete_in_comments . 0)
      (confirm_extra_conf . 1)
      (server_keep_logfiles . 1)
      (global_ycm_extra_conf . "")
      (extra_conf_globlist . [])
      (hmac_secret . ,hmac-secret)
      (collect_identifiers_from_tags_files . 0)
      (filetype_specific_completion_to_disable (gitcommit . 1))
      (collect_identifiers_from_comments_and_strings . 0)
      (min_num_of_chars_for_completion . 2)
      (filepath_completion_use_working_dir . 0)
      (semantic_triggers . ())
      (auto_trigger . 1))))

(defun ycmd-running? ()
  (interactive)
  (if (get-process ycmd-server-process) 't nil))

(defun ycmd-create-options-file (hmac-secret)
  (let ((options-file (make-temp-file "ycmd-options"))
        (options (ycmd-options-contents hmac-secret)))
    (with-temp-file options-file
      (insert (ycmd-json-encode options)))
    options-file))

(defun ycmd-open (filename)
  (interactive
  (list
   (read-file-name "Filename: ")))
  (ycmd-close)
  (let ((hmac-secret (ycmd-generate-hmac-secret)))
    (ycmd-start-server hmac-secret)
    (setq ycmd-hmac-secret hmac-secret)
    (ycmd-load-conf-file filename)))

(defun ycmd-close ()
  (interactive)
  (if (ycmd-running?)
      (delete-process ycmd-server-process)))

(defun ycmd-start-server (hmac-secret)
  (let ((proc-buff (get-buffer-create "*ycmd-server*")))
    (set-buffer proc-buff)
    (erase-buffer)
    
    (let* ((options-file (ycmd-create-options-file hmac-secret))
           (server-command (if (listp ycmd-server-command)
                               ycmd-server-command
                             (list ycmd-server-command)))
           (args (apply 'list (concat "--options_file=" options-file) ycmd-server-args))
           (server-program+args (append server-command args))
           (proc (apply #'start-process ycmd-server-process proc-buff server-program+args))
           (cont 1))
      (while cont
        (set-process-query-on-exit-flag proc nil)
        (accept-process-output proc 0 100 t)
        (let ((proc-output (with-current-buffer proc-buff
			     (buffer-string))))
	  (cond
	   ((string-match "^serving on http://.*:\\\([0-9]+\\\)$" proc-output)
	    (progn
              (set-variable 'ycmd-server-actual-port
                            (string-to-number (match-string 1 proc-output)))
              (setq cont nil)))
	   (t
	    (incf cont)
	    (when (< 3000 cont) ; timeout after 3 seconds
	      (error "Server timeout.")))))))))

(defun ycmd-load-conf-file (filename)
  (interactive
   (list
    (read-file-name "Filename: ")))
  (let ((filename (expand-file-name filename)))
    (ycmd-request
     "/load_extra_conf_file"
     `(("filepath" . ,filename)))))

;; TODO: ycmd-close

(defun ycmd-display-completions (pos)
  (interactive "d")
  (let ((completions (ycmd-get-completions pos)))
    (pop-to-buffer "*ycmd-completions*")
    (erase-buffer)
    (insert (pp-to-string completions))))

(defun ycmd-get-completions (pos)
  (let* ((column-num (+ 1 (save-excursion (goto-char pos) (current-column))))
         (line-num (line-number-at-pos (point)))
         (full-path (buffer-file-name))
         (file-contents (buffer-string))
         (file-types '("cpp"))
         (content `(("column_num" . ,column-num)
                    ("file_data" .
                     ((,full-path . (("contents" . ,file-contents)
                                     ("filetypes" . ,file-types)))))
                    ("filepath" . ,full-path)
                    ("line_num" . ,line-num))))
         (ycmd-request "/completions" content :parser 'json-read)))

(defun* ycmd-request (location content &key (parser 'buffer-string))
  (let* ((content (json-encode content))
         (hmac (ycmd-hmac-function content ycmd-hmac-secret))
         (hex-hmac (encode-hex-string hmac))
         (encoded-hex-hmac (base64-encode-string hex-hmac 't)))
    (request-response-data
     (request
      (format "http://%s:%s%s" ycmd-host ycmd-server-actual-port location)
      :headers `(("Content-Type" . "application/json")
                 ("X-Ycm-Hmac" . ,encoded-hex-hmac))
      :sync 1
      :parser parser
      :data content
      :type "POST"))))

(provide 'ycmd)

;;; ycmd.el ends here
