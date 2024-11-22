;;; https://github.com/kensanata/ggg

(use-package gnus
  :preface
  (defun my-gnus-summary-keys ()
    (local-set-key "y" 'gmail-archive)
    (local-set-key "d" 'gmail-trash)
    (local-set-key "$" 'gmail-report-spam))

  (defun gmail-archive ()
    "Archive the current or marked mails.
This moves them into the All Mail folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

  (defun gmail-trash ()
    "Trash the current or marked mails.
This moves them into the Trash folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Trash"))

  (defun gmail-report-spam ()
    "Report the current or marked mails as spam.
This moves them into the Spam folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

  :custom
  ;; You need to replace this key ID with your own key ID!
  (mml-secure-openpgp-signers '("A52D68794EBED758"))
  ;; This tells Gnus to get email from Gmail via IMAP.
  (gnus-select-method
   '(nnimap "gmail"
            ;; It could also be imap.googlemail.com if that's your server.
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)))
  (gnus-secondary-select-methods '((nntp "news.gmane.io")))
  ;; This tells Gnus to use the Gmail SMTP server. This
  ;; automatically leaves a copy in the Gmail Sent folder.
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  ;; Tell message mode to use SMTP.
  (message-send-mail-function 'smtpmail-send-it)
  ;; Gmail system labels have the prefix [Gmail], which matches
  ;; the default value of gnus-ignored-newsgroups. That's why we
  ;; redefine it.
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  ;; The agent seems to confuse nnimap, therefore we'll disable it.
  (gnus-agent nil)
  ;; We don't want local, unencrypted copies of emails we write.
  (gnus-message-archive-group nil)
  ;; We want to be able to read the emails we wrote.
  (mml-secure-openpgp-encrypt-to-self t)

  ;; https://www.emacswiki.org/emacs/GnusFormatting
  (gnus-summary-line-format "%U%R%z %([%&user-date;]  %-20,20f  %B%s%)\n")
  (gnus-group-line-format "%M%S%p%P%B%(%G%) (%y)\n")
  (gnus-user-date-format-alist '((t . "%d %b %Y %H:%M")))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-thread-hide-subtree t)
  (gnus-thread-indent-level 2)
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")

  (gnus-always-read-dribble-file t)
  :hook
  (('message-setup . 'mml-secure-message-encrypt) ;; Encrypt all messages
   ;; Add two key bindings for your Gmail experience.
   ('gnus-summary-mode . 'my-gnus-summary-keys)))
