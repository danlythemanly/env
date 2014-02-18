(eval-when-compile
  (require 'cl))

(require 'url)
(require 'url-http)
(require 'json)

(defvar github-markdown:url "https://api.github.com/markdown")

(defun github-markdown:read-markdown (file)
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun github-markdown:construct-post-input (file &optional mode context)
  (let ((mode (or mode "gfm"))
        (context (or context "github/gollum"))
        (data (github-markdown:read-markdown file)))
    (json-encode `((mode . ,mode) (context . ,context) (text . ,data)))))

(defun github-markdown:post (file)
  (let ((url-request-method "POST")
        (url-request-data (github-markdown:construct-post-input file)))
    (with-current-buffer (url-retrieve-synchronously github-markdown:url)
      (goto-char (point-min))
      (if (re-search-forward "^$" nil t)
          (message "%s" (buffer-substring (1+ (point)) (point-max)))
        (error "Error")))))

(defun github-markdown ()
  (interactive)
  (progn (write-region (github-markdown:post (buffer-file-name)) nil "/tmp/gfm.html" nil)
         (find-file "/tmp/gfm.html")
         (rename-buffer "*rendered-markdown*")
         (shell-command "open -a 'Google Chrome' /tmp/gfm.html")))


(provide 'github-markdown)
;;; github-markdown.el ends here
