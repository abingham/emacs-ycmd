(require 'ert)
(require 'ycmd)

(setq ycmd-test-cpp-content
      "class Llama {
public:
    void llurp() {}
};

int main(int, char**) {
    Llama l;
    l.

    return 0;
}")

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

