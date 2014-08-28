(defun ycmd-company-candidates ()
  "Get the list of completions at point."
  (if (ycmd-running?)
      (let ((completions (assoc-default 'completions (ycmd-get-completions (point)))))
        (mapcar (lambda (c) (cdr (assoc 'insertion_text c))) completions))
    nil)
  )

(defun company-ycmd-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  ; See company.el for more info.
  (case command
    (interactive (company-begin-backend 'company-ycmd-backend))
    (prefix (and (eq major-mode 'c++-mode)
                 buffer-file-name
                 (ycmd-running?)
                 (not (company-in-string-or-comment))
                 ;(buffer-substring (line-beginning-position) (point))
                 ""))
    ;; TODO: (candidates (cons :async
    ;;                         (lambda (cb) (company-clang--candidates arg cb))))
    (candidates (ycmd-company-candidates))
    (meta       (company-clang--meta arg))
    ))

(add-to-list 'company-backends 'company-ycmd-backend)


