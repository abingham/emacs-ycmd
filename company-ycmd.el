;; (defun company-ycmd-candidates ()
;;   "Get the list of completions at point."
;;   (if (ycmd-running?)
;;       (let ((completions (assoc-default
;; 			  'completions
;; 			  (ycmd-get-completions (point)))))
;;         (mapcar (lambda (c) (cdr (assoc 'insertion_text c)))
;; 		completions))
;;     nil))

(defconst company-ycmd-completion-properties
  '(kind
    extra_menu_info
    detailed_info
    menu_text))

(defun company-ycmd-construct-candidate (src)
  (let ((candidate (assoc-default 'insertion_text src)))
    (dolist (prop company-ycmd-completion-properties candidate)
      (put-text-property 0 1 prop (assoc-default prop src) candidate))))

(defun company-ycmd-candidates ()
  "Get list of completion candidate structs at point."
  (if (ycmd-running?)
      (mapcar
       'company-ycmd-construct-candidate
       (assoc-default 'completions (ycmd-get-completions (point))))
    nil))

(defun company-ycmd-get-metadata (candidate)
  (get-text-property 0 'detailed_info candidate))

(defun company-ycmd-get-annotation (candidate)
  (format " [%s]" (get-text-property 0 'kind candidate)))

(defun company-ycmd-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  ; See company.el for more info.
  (case command
    (interactive (company-begin-backend 'company-ycmd-backend))
    (prefix (and (eq major-mode 'c++-mode)
                 buffer-file-name
                 (ycmd-running?)
                 (not (company-in-string-or-comment))
                 ""
                 ;;(buffer-substring-no-properties (line-beginning-position) (point))
                 ))
    ;; TODO: (candidates (cons :async
    ;;                         (lambda (cb) (company-clang--candidates arg cb))))
    (candidates (company-ycmd-candidates))
    (meta (company-ycmd-get-metadata arg))
    (annotation (company-ycmd-get-annotation arg))
    (no-cache 't) ; Until we get is all sorted out...
    ))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd-backend to company-backends"
  (add-to-list 'company-backends 'company-ycmd-backend))

(provide 'company-ycmd)
