(require 'auto-complete)
(require 'ycmd)

;; (defvar ac-ycmd-cache nil
;;   "Hold the results of the last ycmd-get-completions.")

(defun ac-ycmd-candidates ()
  "Get the list of completions at point."
  (if (ycmd-running?)
      (let ((completions (assoc-default 'completions (ycmd-get-completions (point)))))
        (mapcar (lambda (c) (cdr (assoc 'insertion_text c))) completions))
    nil))
  ;; (if (ycmd-running?)
  ;;     (progn
  ;;       (setq ac-ycmd-cache (assoc-default 'completions (ycmd-get-completions (point))))
  ;;       (mapcar (lambda (c) (cdr (assoc 'insertion_text c))) ac-ycmd-cache))
  ;;   (setq ac-ycmd-cache nil))

;; The autocomplete source for ycmd
(defvar ac-source-ycmd
  '(;(depends ycmd)
    (candidates . ac-ycmd-candidates)
    ;(cache)
    ;(document)
    ;(symbol . "s")
    (requires . 0)
    ))

;;;###autoload
(defun ac-ycmd-setup ()
  "Add ac-source-ycmd to autocomplete list."
  (setq ac-sources (append '(ac-source-ycmd) ac-sources)))

(provide 'auto-complete-ycmd)
