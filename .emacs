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

(defun theme-dark ()
  "Set dark theme"
  (interactive)
  (disable-theme 'whiteboard)
  (load-theme 'wombat t))

(defun theme-light ()
  "Set light theme"
  (interactive)
  (disable-theme 'wombat-ediff)
  (load-theme 'whiteboard t))


(defun toggle-theme ()
  "Toggle between light or dark theme"
  (interactive)
  (load-theme 'wombat-ediff t))

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
         (setq x-alt-keysym 'meta)
         (setq exec-path (append exec-path '("/usr/local/bin")))
         (set-face-attribute 'default nil :family "Consolas")
         (set-face-attribute 'default nil :height 130)))
 ((string-equal system-type "gnu/linux")
  (progn (defalias 'google 'linux-google)
         (global-set-key "\C-cc" 'linux-copy)))
 )

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ch" 'hl-line-mode)

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

(defun doc-view-keys()
  "Make movement keys work for doc-view mode."
  (progn
    (auto-revert-mode)
    (define-key doc-view-mode-map (kbd "M-J")
      '(lambda ()
         "windmove-left for docview"
         (interactive)
         (windmove-left 1)))
    (define-key doc-view-mode-map (kbd "M-L")
      '(lambda ()
         "windmove-right for docview"
         (interactive)
         (windmove-right 1)))
    (define-key doc-view-mode-map (kbd "M-I")
      '(lambda ()
         "windmove-up for docview"
         (interactive)
         (windmove-up 1)))
    (define-key doc-view-mode-map (kbd "M-K")
      '(lambda ()
         "windmove-down for docview"
         (interactive)
         (windmove-down 1)))))

(add-hook 'doc-view-mode-hook 'doc-view-keys)
(setq doc-view-continuous t)

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

(defun xcscope-key-override ()
  "Override M-K for xcscope mode."
  (local-set-key (kbd "M-K")  'windmove-down))

(if (file-exists-p "~/.emacs.d/xcscope.el/xcscope.el")
    (progn
      (load-file "~/.emacs.d/xcscope.el/xcscope.el")
      (require 'xcscope)
      (add-hook 'cscope-list-entry-hook 'xcscope-key-override)

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
       (shell-command-on-region (point-min) (point-max) "/usr/local/bin/markdown" standard-output))
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

(setq auto-mode-alist (cons '("\\.md$" . text-mode) auto-mode-alist))

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

(setq initial-scratch-message
      (concat 
      ";;  _          _ _                                        \n"
      ";; | |__   ___| | | ___     ___ _ __ ___   __ _  ___ ___  \n"
      ";; | '_ \\ / _ \\ | |/ _ \\   / _ \\ '_ ` _ \\ / _` |/ __/ __| \n"
      ";; | | | |  __/ | | (_) | |  __/ | | | | | (_| | (__\\__ \\ \n"
      ";; |_| |_|\\___|_|_|\\___/   \\___|_| |_| |_|\\__,_|\\___|___/ \n"
      ";;                                                        \n"))


(setq inhibit-splash-screen t)
(theme-dark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq asm-comment-char ?\#)

(put 'upcase-region 'disabled nil)
