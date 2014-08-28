(require 'hmac-def)
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

(defvar ycmd-server-actual-port 0
  "The actual port being used by the ycmd server. This is set
  based on the output from the server itself."
  :type int
  :group 'ycmd)

(defconst ycmd-hmac-secret "1234123412341234") ; TODO: Create real secret code.

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

(defun ycmd-create-options-file (hmac-secret)
  (let ((options-file (make-temp-file "ycmd-options"))
        (options (ycmd-options-contents hmac-secret)))
    (with-temp-file options-file
      ; TODO: Need to encode the semantic-triggers (and all empty lists) as "{}" rather than "null".
      (insert (json-encode options)))
    options-file))

(defun ycmd-open ()
  (interactive)
  (let ((proc-buff (get-buffer-create "*ycmd-server*")))
    (set-buffer proc-buff)
    (erase-buffer)
    
    (let* ((options-file (ycmd-create-options-file ycmd-hmac-secret))
           (server-command (if (listp ycmd-server-command)
                               ycmd-server-command
                             (list ycmd-server-command)))
           (args (list (concat "--options_file=" options-file)))
           (server-program+args (append server-command args))
           (proc (apply #'start-process "ycmd-server" proc-buff server-program+args))
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
                            (string-to-number (match-string 1 proc-output)))c
              (setq cont nil)))
	   (t
	    (incf cont)
	    (when (< 3000 cont) ; timeout after 3 seconds
	      (error "Server timeout.")))))))))

(defun ycmd-get-completions (pos)
  (interactive "d")
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
    ; TODO: Right now we just log the result, but we want to do something more constructive with it...e.g. auto-complete.
    (message
     (format "%s"
             (ycmd-request "/completions" content :parser 'json-read)))))

(defun ycmd-load-conf-file ()
  (ycmd-request
   "/load_extra_conf_file"
   ; TODO: Obviously figure out the correct way to specify the right file.
   '(("filepath" . "/Users/sixtynorth/projects/boost_python_exception/.ycm_extra_conf.py"))))

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


