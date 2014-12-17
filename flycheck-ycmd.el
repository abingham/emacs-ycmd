;;; flycheck-ycmd -- flycheck integration for ycmd
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'ycmd)

;; See http://www.lunaryorn.com/2014/12/03/generic-syntax-checkers-in-flycheck.html for more info

;; TODO: Add hook to ycmd-file-parse-result-hook which caches parse results.
;;       We'll look in that cache later when flycheck does a syntax check.

;; This maps ycmd result 'kinds' to flycheck 'levels'.
;; TODO: What are all of the available options?
(setq flycheck-ycmd--level-map
      '(("ERROR" . error)
        ("WARNING" . warning)))

(defun flycheck-ycmd--result-to-error (r)
  "Convert ycmd parse result structure R into a flycheck error object."
  (ycmd--with-destructured-parse-result
   r
   (if (string-equal filepath (buffer-file-name))
       (flycheck-error-new
        :line start-line-num
        :column start-column-num
        :buffer (current-buffer)
        :filename filepath
        :message text
        :level (assoc-default kind flycheck-ycmd--level-map 'string-equal 'error)))))

(defun flycheck-ycmd--start (checker callback)
  "Start ycmd flycheck CHECKER using CALLBACK to communicate with flycheck."
  (let ((errors (delq
                 nil
                 (mapcar 'flycheck-ycmd--result-to-error
                         flycheck-ycmd--cache))))
    (funcall callback 'finished errors))

  ;; OR call (callback 'errored some-message)
  )

(setq flycheck-ycmd--cache '())

(defun flycheck-ycmd--cache-parse-results (results)
  "Cache ycmd output RESULTS for error display.

We cache the results and use them as the basis for the error
display."
  ;; Proper approach:
  ;; 1. Find all affected filepaths, and clear them from the cache
  ;; 2. For each result, insert into cache-long-line-scans
  ;; 3. Remove things that haven't been used in a while.

  ;; For now, just store the results wholesale
  (setq flycheck-ycmd--cache results)

  (flycheck-buffer))

(defun flycheck-ycmd-setup ()
  "Convenience function to setup the ycmd flycheck checker.

This adds a hook to watch for ycmd parse results, and it adds the
ycmd checker to the list of flycheck checkers."
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
  (add-to-list 'flycheck-checkers 'ycmd))

(flycheck-define-generic-checker 'ycmd
  "A flycheck checker using parse results from ycmd."
  :start #'flycheck-ycmd--start
  :modes '(c++-mode)
  :predicate (lambda () ycmd-mode))

(provide 'flycheck-ycmd)
;;; flycheck-ycmd.el ends here
