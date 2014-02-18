;; CONFIGS and CHEAT SHEETS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;      C-v             pgdown
;;      M-v             pgup
;;      M->             end of document
;;      M-<             start of document

;;      C-M-a           start of func
;;      C-M-e           end of func
;;      C-M-h           select func

;;      C-M-u           start of block ({)
;;      C-M-n           end of block (})

;;      C-M-\           indent region      

;;      C-u SPC         pop mark

;;      M-=             count lines in region

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun figlet ()
  "Replace region with figlet representation"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "figlet" t t))

(defun linux-google ()
  "Google region in Chrome from Linux"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "linux-chrome-open.bash"))

(defun linux-copy ()
  "Copy region to linux clipboard"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "xsel-copy"))

(defun mac-google ()
  "Google region in Chrome from a mac"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "mac-chrome-open.bash"))

(defun mac-copy ()
  "Copy region to mac clipboard"
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

(defun mac-paste ()
  "Paste region from mac clipboard"
  (interactive)
  (shell-command "pbpaste" t))


(cond
 ((string-equal system-type "darwin") 
  (progn (defalias 'google 'mac-google)
         (load "server")
         (unless (server-running-p) (server-start))
         (global-set-key "\C-cc" 'mac-copy)
         (global-set-key "\C-cv" 'mac-paste)
         (set-face-attribute 'default nil :family "Consolas")
         (set-face-attribute 'default nil :height 130)))
 ((string-equal system-type "gnu/linux")
  (progn (defalias 'google 'linux-google)
         (global-set-key "\C-cc" 'linux-copy)))
 )

(global-set-key "\C-cg" 'goto-line)

(global-set-key (kbd "M-J")  'windmove-left)
(global-set-key (kbd "M-L") 'windmove-right)
(global-set-key (kbd "M-I")    'windmove-up)
(global-set-key (kbd "M-K")  'windmove-down)

(defun fontinc ()
  "Increase xfce terminal font size."
  (interactive)
  (shell-command "xfce-term-font.bash inc"))

(defun fontdec ()
  "Decrease xfce terminal font size."
  (interactive)
  (shell-command "xfce-term-font.bash dec"))


(global-set-key (kbd "M-+")  'fontinc)
(global-set-key (kbd "M-_")  'fontdec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;      M-N             new terminal
;;      M-M             toggle line/char mode 
;;
;;      C-x C-f /sudo::/path/to/file   
;;                      open as root

(defun bash (buffer-name)
  "Start bash terminal and rename buffer."
  (interactive (list (read-string "buffer name: " "^term^")))
  (term "/bin/bash")
  (rename-buffer buffer-name t))

(defun ssh (host)
  "ssh to a host in new term."
  (interactive (list (read-string "host: ")))
  (setenv "EMACS_SSH_HOST" host)
  (term "sshwrap.bash")
  (rename-buffer (concat "^" host "^") t))

(global-set-key (kbd "M-N")  'bash)

(eval-after-load "term"
  '(progn
     (term-set-escape-char ?\C-x)
     (defun term-toggle-mode ()
       (interactive)
       (if (term-in-line-mode) 
	   (term-char-mode)
	 (term-line-mode)))
     (define-key term-raw-map (kbd "M-M") 'term-toggle-mode)
     (global-set-key (kbd "M-M") 'term-toggle-mode)
     (define-key term-raw-map (kbd "M-N") 'bash)
     (define-key term-raw-map (kbd "M-J") 'windmove-left)
     (define-key term-raw-map (kbd "M-L") 'windmove-right)
     (define-key term-raw-map (kbd "M-I") 'windmove-up)
     (define-key term-raw-map (kbd "M-K") 'windmove-down) 
     (define-key term-raw-map (kbd "M-+") 'fontinc) 
     (define-key term-raw-map (kbd "M-_") 'fontdec) 
     ))

;; close the terminal buffer automatically on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))


(defun docview-key-override ()
  "Override movement keys for docview mode."
  (local-set-key (kbd "M-J") 'windmove-left)
  (local-set-key (kbd "M-L") 'windmove-right)
  (local-set-key (kbd "M-I") 'windmove-up)
  (local-set-key (kbd "M-K") 'windmove-down))

(add-hook 'doc-view-mode-hook 'docview-key-override)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;      C-c s s         Find symbol.
;;      C-c s d         Find global definition.
;;      C-c s g         Find global definition (alternate binding).
;;      C-c s G         Find global definition without prompting.
;;      C-c s c         Find functions calling a function.
;;      C-c s C         Find called functions.
;;      C-c s t         Find text string.
;;      C-c s e         Find egrep pattern.
;;      C-c s f         Find a file.
;;      C-c s i         Find files #including a file.

;;      C-c s b         Display *cscope* buffer.
;;      C-c s B         Auto display *cscope* buffer toggle.
;;      C-c s n         Next symbol.
;;      C-c s N         Next file.
;;      C-c s p         Previous symbol.
;;      C-c s P         Previous file.
;;      C-c s u         Pop mark.
;;
;;      C-c s a         Set initial directory.
;;      C-c s A         Unset initial directory.
;;
;;      C-c s L         Create list of files to index.
;;      C-c s I         Create list and index.
;;      C-c s E         Edit list of files to index.
;;      C-c s W         Locate this buffer's cscope directory
;;                      ("W" --> "where").
;;      C-c s S         Locate this buffer's cscope directory.
;;                      (alternate binding: "S" --> "show").
;;      C-c s T         Locate this buffer's cscope directory.
;;                      (alternate binding: "T" --> "tell").
;;      C-c s D         Dired this buffer's directory.

(if (file-exists-p "~/.emacs.d/xcscope.el/xcscope.el")
    (progn
      (load-file "~/.emacs.d/xcscope.el/xcscope.el")
      (require 'xcscope)
      (add-hook 'prog-mode-hook 'cscope-minor-mode)
      (add-hook 'c-mode-common-hook 'cscope-minor-mode)
      ))


(if (file-exists-p "~/.emacs.d/github-markdown-api.el")
    (load-file "~/.emacs.d/github-markdown-api.el"))

(defun markdown ()
  (interactive)
  (progn
    (write-region
     (with-output-to-string
       (shell-command-on-region (point-min) (point-max) "markdown" standard-output))
     nil "/tmp/md.html" nil)
    (find-file "/tmp/md.html")
    (rename-buffer "*rendered-markdown*")
    (shell-command "open -a 'Google Chrome' /tmp/md.html")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default c-basic-offset 4
	      c-file-style "bsd"
              tab-width 4
              indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((((class color) (min-colors 16)) (:background "thistle1" :foreground "darkorchid4"))))
 '(ediff-current-diff-Ancestor ((((class color) (min-colors 16)) (:background "thistle1" :foreground "darkorchid4"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "thistle1" :foreground "DarkOrchid4"))))
 '(ediff-current-diff-C ((((class color) (min-colors 16)) (:background "thistle1" :foreground "Darkorchid4"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "lightgrey" :foreground "black"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "lightgrey" :foreground "black"))))
 '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "rosybrown1" :foreground "darkorchid4"))))
 '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "rosybrown1" :foreground "darkorchid4"))))
 '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "rosybrown1" :foreground "darkorchid4"))))
 '(ediff-fine-diff-C ((((class color) (min-colors 16)) (:background "rosybrown1" :foreground "darkorchid4"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "lightgrey" :foreground "black"))))
 '(ediff-odd-diff-Ancestor ((((class color) (min-colors 16)) (:background "lightgrey" :foreground "black"))))
 '(ediff-odd-diff-C ((((class color) (min-colors 16)) (:background "lightgrey" :foreground "black")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (dolist (hook '(text-mode-hook))(add-hook hook (lambda () (flyspell-mode 1))))(dolist (hook '(text-mode-hook))(add-hook hook (lambda () (flyspell-buffer))))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-buffer)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
        '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook 
                             ruby-mode-hook java-mode-hook))

(put 'nxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `(("." . "~/.emacs-saves")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 
