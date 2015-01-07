(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "danlythemanly@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-fetch-old-headers t)
(setq user-mail-address "danlythemanly@gmail.com")
(setq user-full-name "Dan Williams")
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (define-key gnus-summary-mode-map (kbd "M-K")
              '(lambda ()
                 "windmove-down for gnus summary"
                 (interactive)
                 (windmove-down 1)))))
