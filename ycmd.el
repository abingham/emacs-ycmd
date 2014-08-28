(require 'hmac-def)
(require 'json)
(require 'request)

;; POST /completions HTTP/1.1
;; Accept: application/json
;; Accept-Encoding: gzip, deflate
;; Content-Length: 1022
;; Host: 127.0.0.1:62030
;; User-Agent: HTTPie/0.8.0
;; X-Ycm-Hmac: NmFmOGMwMmRkNmJhNmNhNzdlZTA2YzQxNzc0NjdkNDAxMmZkNGU1OTNmNTU5ZWIzNTNjMDJlMTZlYTcxNTI2Nw==
;; content-type: application/json

;; {
;;     "column_num": 7, 
;;     "file_data": {
;;         "/Users/sixtynorth/projects/ycmd/examples/samples/some_cpp.cpp": {
;;             "contents": "// Copyright (C) 2014  Google Inc.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//     http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\nstruct Foo {\n  int x;\n  int y  // There's a missing semicolon here\n  char c;\n};\n\nint main()\n{\n  Foo foo;\n  // The location after the dot is line 28, col 7\n  foo.\n}\n\n\n", 
;;             "filetypes": [
;;                 "cpp"
;;             ]
;;         }
;;     }, 
;;     "filepath": "/Users/sixtynorth/projects/ycmd/examples/samples/some_cpp.cpp", 
;;     "line_num": 25
;; }
;; (setq data '(("column_num" . 16)
;; 	     ("file_data" .
;; 	      (("/Users/sixtynorth/sandbox/clang_rename/foo.cpp" .
;; 		(("contents" . "#include \"foo.hpp\"

;; void Foo::foo() {
;;     int x = fno
;;     int y = x + 1;
;; }
;; ")
;; 		 ("filetypes" . ("cpp"))))))
;; 	     ("filepath" . "/Users/sixtynorth/sandbox/clang_rename/foo.cpp")
;; 	     ("line_num" . 4))
;;       )

(defcustom ycmd-host "127.0.0.1"
  :type '(string)
  :group 'ycmd)

(defcustom ycmd-server-program '("python" "/Users/sixtynorth/projects/ycmd/ycmd")
  :type '(string)
  :group 'ycmd)

(define-hmac-function ycmd-hmac
  (lambda (x) (secure-hash 'sha256 x nil nil 1))
  64 64)

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
                    ("line_num" . ,line-num)))
         (completions '((candidates . (list "Foo" "Bar" "Baz")))))
    (ycmd-request "/completions" content :parser 'json-read)
    (auto-complete '(completions))))

;; (defun simple-test ()
;;   (interactive)
;;   (message
;;    (ycmd-request
;;     "/load_extra_conf_file"
;;     '(("filepath" . "/Users/sixtynorth/projects/boost_python_exception/.ycm_extra_conf.py"))))
;;   (message
;;    (ycmd-request
;;     "/debug_info"
;;     '()))
;;   (message
;;    (format "%s" 
;;            (ycmd-request
;;             "/completions"
;;             `(("column_num" . 7)
;;               ("file_data" .
;;                (("/Users/sixtynorth/projects/boost_python_exception/src/boost_python_exception/exceptions.cpp" .
;;                  (("contents" . "class Foo { int x; int y; char c; };

;; int main() {
;;     Foo f;
;;     f.
;; }
;; ")
;;                   ("filetypes" . (list "cpp"))))))
;;               ("filepath" . "/Users/sixtynorth/projects/boost_python_exception/src/boost_python_exception/exceptions.cpp")
;;               ("line_num" . 5))
;;             :parser 'json-read))))

(defun* ycmd-request (location content &key (parser 'buffer-string))
  (let* ((options (json-read-file "/Users/sixtynorth/projects/ycmd/options.json.BAK"))
         (hmac-secret (base64-decode-string (cdr (assoc 'hmac_secret options))))
         (content (json-encode content))
         (hmac (my-hmac content hmac-secret))
         (hex-hmac (encode-hex-string hmac))
         (encoded-hex-hmac (base64-encode-string hex-hmac 't)))
    (request-response-data
     (request
      (concat "http://127.0.0.1:58526" location)
      :headers `(("Content-Type" . "application/json")
                 ("X-Ycm-Hmac" . ,encoded-hex-hmac))
      :sync 1
      :parser parser
      :data content
      :type "POST"))))


