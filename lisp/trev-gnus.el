;;; trev-gnus.el --- Gnus ergonomics for my config  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: mail, news
;; Package-Version: 1.0.0

;;; Commentary:

;; Small command layer that makes Gnus easier to drive from memory while
;; keeping the full Gnus command set available through Transient.

;;; Code:

(require 'cl-lib)
(require 'use-package)
(require 'gnus)
(require 'gnus-art)
(require 'gnus-group)
(require 'gnus-msg)
(require 'gnus-search)
(require 'gnus-sum)
(require 'gnus-win)
(require 'message)
(require 'mml)
(require 'mml-sec)
(require 'smtpmail)
(require 'transient)

(defgroup trev-gnus nil
  "Personal Gnus ergonomics."
  :group 'gnus)

(defcustom trev-gnus-gmail-archive-group "[Gmail]/All Mail"
  "Gmail group used for archiving messages."
  :type 'string
  :group 'trev-gnus)

(defcustom trev-gnus-gmail-trash-group "[Gmail]/Trash"
  "Gmail group used for trashing messages."
  :type 'string
  :group 'trev-gnus)

(defcustom trev-gnus-gmail-spam-group "[Gmail]/Spam"
  "Gmail group used for reporting spam."
  :type 'string
  :group 'trev-gnus)

(defun trev-gnus--summary-buffer ()
  "Return the relevant summary buffer for the current Gnus context."
  (cond
   ((derived-mode-p 'gnus-summary-mode)
    (current-buffer))
   ((and (boundp 'gnus-article-current-summary)
         (buffer-live-p gnus-article-current-summary))
    gnus-article-current-summary)
   ((and (boundp 'gnus-summary-buffer)
         (buffer-live-p (symbol-value 'gnus-summary-buffer)))
    (symbol-value 'gnus-summary-buffer))))

(defun trev-gnus--summary-call (function &rest args)
  "Call summary FUNCTION with ARGS from summary or article buffers."
  (if-let* ((buffer (trev-gnus--summary-buffer)))
      (with-current-buffer buffer
        (apply function args))
    (user-error "No Gnus summary buffer is available")))

(defun trev-gnus--article-buffer ()
  "Return the relevant article buffer for the current Gnus context."
  (cond
   ((derived-mode-p 'gnus-article-mode)
    (current-buffer))
   ((and (boundp 'gnus-article-buffer)
         (buffer-live-p (symbol-value 'gnus-article-buffer)))
    (symbol-value 'gnus-article-buffer))))

(defun trev-gnus--article-call (function &rest args)
  "Call article FUNCTION with ARGS from summary or article buffers."
  (if-let* ((buffer (trev-gnus--article-buffer)))
      (with-current-buffer buffer
        (apply function args))
    (user-error "No Gnus article buffer is available")))

(defmacro trev-gnus--define-summary-command (name command)
  "Define NAME as an interactive wrapper for summary COMMAND."
  `(defun ,name ()
     ,(format "Run `%s' in the current Gnus summary buffer." command)
     (interactive)
     (trev-gnus--summary-call
      (lambda ()
        (call-interactively #',command)))))

(defmacro trev-gnus--define-article-command (name command)
  "Define NAME as an interactive wrapper for article COMMAND."
  `(defun ,name ()
     ,(format "Run `%s' in the current Gnus article buffer." command)
     (interactive)
     (trev-gnus--article-call
      (lambda ()
        (call-interactively #',command)))))

(defun trev-gnus--nnimap-prefix ()
  "Return the current nnimap group prefix, including trailing colon."
  (or (when (and (boundp 'gnus-newsgroup-name)
                 (stringp gnus-newsgroup-name)
                 (string-match "\\`\\(nnimap\\+[^:]+:\\)" gnus-newsgroup-name))
        (match-string 1 gnus-newsgroup-name))
      (when (and (boundp 'gnus-select-method)
                 (eq (car-safe gnus-select-method) 'nnimap))
        (concat (gnus-method-to-server-name gnus-select-method) ":"))
      "nnimap+gmail:"))

(defun trev-gnus--gmail-group (group)
  "Return fully qualified Gmail GROUP for the current nnimap server."
  (concat (trev-gnus--nnimap-prefix) group))

(defun trev-gnus-archive ()
  "Move the current article to Gmail All Mail."
  (interactive)
  (trev-gnus--summary-call
   #'gnus-summary-move-article nil
   (trev-gnus--gmail-group trev-gnus-gmail-archive-group)))

(defun trev-gnus-trash ()
  "Move the current article to Gmail Trash."
  (interactive)
  (trev-gnus--summary-call
   #'gnus-summary-move-article nil
   (trev-gnus--gmail-group trev-gnus-gmail-trash-group)))

(defun trev-gnus-report-spam ()
  "Move the current article to Gmail Spam."
  (interactive)
  (trev-gnus--summary-call
   #'gnus-summary-move-article nil
   (trev-gnus--gmail-group trev-gnus-gmail-spam-group)))

(defun trev-gnus-mark-unread ()
  "Mark the current article unread."
  (interactive)
  (trev-gnus--summary-call #'gnus-summary-clear-mark-forward 1))

(defun trev-gnus-tick ()
  "Tick the current article."
  (interactive)
  (trev-gnus--summary-call #'gnus-summary-tick-article-forward 1))

(defun trev-gnus-reply ()
  "Reply to the current article."
  (interactive)
  (trev-gnus--summary-call #'gnus-summary-reply))

(defun trev-gnus-reply-all ()
  "Reply to all recipients of the current article."
  (interactive)
  (trev-gnus--summary-call #'gnus-summary-wide-reply))

(defun trev-gnus-forward ()
  "Forward the current article."
  (interactive)
  (trev-gnus--summary-call #'gnus-summary-mail-forward))

(defun trev-gnus-compose ()
  "Compose a new message from Gnus."
  (interactive)
  (if (derived-mode-p 'gnus-article-mode)
      (gnus-article-mail nil)
    (trev-gnus--summary-call #'gnus-summary-mail-other-window)))

(defun trev-gnus--current-search-group-spec ()
  "Return a search group spec for the current summary group."
  (trev-gnus--summary-call
   (lambda ()
     (let* ((group gnus-newsgroup-name)
            (server (if (fboundp 'gnus-group-server)
                        (gnus-group-server group)
                      (gnus-method-to-server
                       (gnus-find-method-for-group group)))))
       (list (cons server (list group)))))))

(defun trev-gnus-search-current-group (&optional no-parse)
  "Create an ephemeral IMAP search group for the current group.
With prefix argument NO-PARSE, pass the query directly to the
underlying Gnus search backend."
  (interactive "P")
  (require 'gnus-search)
  (require 'gnus-group)
  (gnus-group-read-ephemeral-search-group
   no-parse
   `((search-query-spec . ,(gnus-search-make-spec no-parse))
     (search-group-spec . ,(trev-gnus--current-search-group-spec)))))

(trev-gnus--define-summary-command
 trev-gnus-catchup-and-exit gnus-summary-catchup-and-exit)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-unread gnus-summary-limit-to-unread)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-author gnus-summary-limit-to-author)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-recipient gnus-summary-limit-to-recipient)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-subject gnus-summary-limit-to-subject)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-headers gnus-summary-limit-to-headers)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-bodies gnus-summary-limit-to-bodies)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-marks gnus-summary-limit-to-marks)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-age gnus-summary-limit-to-age)
(trev-gnus--define-summary-command
 trev-gnus-limit-to-singletons gnus-summary-limit-to-singletons)
(trev-gnus--define-summary-command
 trev-gnus-pop-limit gnus-summary-pop-limit)
(trev-gnus--define-summary-command
 trev-gnus-reselect-current-group gnus-summary-reselect-current-group)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-date gnus-summary-sort-by-date)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-most-recent-date gnus-summary-sort-by-most-recent-date)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-author gnus-summary-sort-by-author)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-recipient gnus-summary-sort-by-recipient)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-subject gnus-summary-sort-by-subject)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-marks gnus-summary-sort-by-marks)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-score gnus-summary-sort-by-score)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-original gnus-summary-sort-by-original)
(trev-gnus--define-summary-command
 trev-gnus-sort-by-newsgroups gnus-summary-sort-by-newsgroups)
(trev-gnus--define-summary-command
 trev-gnus-show-thread gnus-summary-show-thread)
(trev-gnus--define-summary-command
 trev-gnus-hide-thread gnus-summary-hide-thread)
(trev-gnus--define-summary-command
 trev-gnus-toggle-threads gnus-summary-toggle-threads)
(trev-gnus--define-summary-command
 trev-gnus-next-thread gnus-summary-next-thread)
(trev-gnus--define-summary-command
 trev-gnus-prev-thread gnus-summary-prev-thread)
(trev-gnus--define-summary-command
 trev-gnus-refer-thread gnus-summary-refer-thread)
(trev-gnus--define-summary-command
 trev-gnus-toggle-header gnus-summary-toggle-header)
(trev-gnus--define-summary-command
 trev-gnus-show-raw-article gnus-summary-show-raw-article)
(trev-gnus--define-summary-command
 trev-gnus-browse-url gnus-summary-browse-url)
(trev-gnus--define-summary-command
 trev-gnus-search-article-forward gnus-summary-search-article-forward)
(trev-gnus--define-summary-command
 trev-gnus-search-article-backward gnus-summary-search-article-backward)
(trev-gnus--define-summary-command
 trev-gnus-search-group gnus-summary-search-group)
(trev-gnus--define-summary-command
 trev-gnus-make-group-from-search gnus-summary-make-group-from-search)

(trev-gnus--define-article-command
 trev-gnus-browse-html-article gnus-article-browse-html-article)
(trev-gnus--define-article-command
 trev-gnus-show-images gnus-article-show-images)
(trev-gnus--define-article-command
 trev-gnus-view-part gnus-article-view-part)
(trev-gnus--define-article-command
 trev-gnus-toggle-truncate-lines gnus-article-toggle-truncate-lines)

(transient-define-prefix trev-gnus-filter-transient ()
  "Limit the current summary buffer."
  [["Filter"
    ("u" "unread" trev-gnus-limit-to-unread)
    ("a" "author" trev-gnus-limit-to-author)
    ("r" "recipient" trev-gnus-limit-to-recipient)
    ("s" "subject" trev-gnus-limit-to-subject)
    ("h" "headers" trev-gnus-limit-to-headers)
    ("b" "body" trev-gnus-limit-to-bodies)]
   ["More"
    ("m" "marks" trev-gnus-limit-to-marks)
    ("A" "age" trev-gnus-limit-to-age)
    ("t" "singletons" trev-gnus-limit-to-singletons)
    ("p" "pop limit" trev-gnus-pop-limit)
    ("g" "reselect" trev-gnus-reselect-current-group)]])

(transient-define-prefix trev-gnus-sort-transient ()
  "Sort the current summary buffer."
  [["Sort"
    ("d" "date" trev-gnus-sort-by-date)
    ("D" "newest date" trev-gnus-sort-by-most-recent-date)
    ("a" "author" trev-gnus-sort-by-author)
    ("r" "recipient" trev-gnus-sort-by-recipient)
    ("s" "subject" trev-gnus-sort-by-subject)]
   ["Other"
    ("m" "marks" trev-gnus-sort-by-marks)
    ("S" "score" trev-gnus-sort-by-score)
    ("o" "original" trev-gnus-sort-by-original)
    ("g" "group" trev-gnus-sort-by-newsgroups)]])

(transient-define-prefix trev-gnus-thread-transient ()
  "Thread commands for the current summary buffer."
  [["Thread"
    ("TAB" "show" trev-gnus-show-thread)
    ("S-TAB" "hide" trev-gnus-hide-thread)
    ("t" "toggle" trev-gnus-toggle-threads)
    ("n" "next" trev-gnus-next-thread)
    ("p" "previous" trev-gnus-prev-thread)
    ("r" "refer thread" trev-gnus-refer-thread)]])

(transient-define-prefix trev-gnus-view-transient ()
  "Article display commands."
  [["Article"
    ("h" "toggle headers" trev-gnus-toggle-header)
    ("H" "raw article" trev-gnus-show-raw-article)
    ("w" "browse URL" trev-gnus-browse-url)
    ("o" "browse HTML" trev-gnus-browse-html-article)]
   ["MIME"
    ("i" "show images" trev-gnus-show-images)
    ("p" "view part" trev-gnus-view-part)
    ("l" "toggle lines" trev-gnus-toggle-truncate-lines)]])

(transient-define-prefix trev-gnus-search-transient ()
  "Search commands for Gnus."
  [["Search"
    ("s" "IMAP search group" trev-gnus-search-current-group)
    ("a" "article forward" trev-gnus-search-article-forward)
    ("A" "article backward" trev-gnus-search-article-backward)
    ("g" "search group name" trev-gnus-search-group)
    ("p" "persist search" trev-gnus-make-group-from-search)]])

(transient-define-prefix trev-gnus-dispatch ()
  "Gnus command menu."
  [["Actions"
    ("a" "archive" trev-gnus-archive)
    ("d" "trash" trev-gnus-trash)
    ("!" "spam" trev-gnus-report-spam)
    ("u" "unread" trev-gnus-mark-unread)
    ("t" "tick" trev-gnus-tick)
    ("c" "catch up" trev-gnus-catchup-and-exit)]
   ["Compose"
    ("m" "compose" trev-gnus-compose)
    ("r" "reply" trev-gnus-reply)
    ("R" "reply all" trev-gnus-reply-all)
    ("f" "forward" trev-gnus-forward)]
   ["Menus"
    ("s" "search" trev-gnus-search-transient)
    ("/" "filter" trev-gnus-filter-transient)
    ("S" "sort" trev-gnus-sort-transient)
    ("T" "thread" trev-gnus-thread-transient)
    ("v" "view" trev-gnus-view-transient)]])

(transient-define-prefix trev-gnus-message-security ()
  "Security and attachment commands for Message mode."
  [["Security"
    ("s" "sign" mml-secure-message-sign)
    ("e" "encrypt" mml-secure-message-encrypt)
    ("b" "sign+encrypt" mml-secure-message-sign-encrypt)]
   ["Attach"
    ("a" "file" mml-attach-file)]])

(transient-define-prefix trev-gnus-group-dispatch ()
  "Gnus group command menu."
  [["Read"
    ("RET" "select" gnus-group-select-group)
    ("SPC" "read" gnus-group-read-group)
    ("n" "next unread" gnus-group-next-unread-group)
    ("p" "previous unread" gnus-group-prev-unread-group)
    ("g" "refresh all" gnus-group-get-new-news)
    ("G" "refresh group" gnus-group-get-new-news-this-group)]
   ["Search"
    ("s" "ephemeral search" gnus-group-read-ephemeral-search-group)
    ("S" "persistent search" gnus-group-make-search-group)
    ("j" "jump group" gnus-group-jump-to-group)
    ("a" "apropos" gnus-group-apropos)]
   ["List"
    ("l" "subscribed" gnus-group-list-groups)
    ("L" "all" gnus-group-list-all-groups)
    ("k" "killed" gnus-group-list-killed)
    ("t" "ticked" gnus-group-list-ticked)]
   ["Group"
    ("u" "toggle subscribe" gnus-group-toggle-subscription-at-point)
    ("c" "catch up" gnus-group-catchup-current)
    ("C" "catch up all" gnus-group-catchup-current-all)
    ("m" "compose mail" gnus-group-mail)
    ("q" "quit" gnus-group-exit)]])

(defun trev-gnus--setup-group-keys ()
  "Install personal group keys."
  (define-key gnus-group-mode-map (kbd "?") #'trev-gnus-group-dispatch))

(defun trev-gnus--setup-summary-keys ()
  "Install personal summary keys."
  (dolist (binding
           `(("?" . trev-gnus-dispatch)
             ("/" . trev-gnus-filter-transient)
             ("a" . trev-gnus-archive)
             ("d" . trev-gnus-trash)
             ("!" . trev-gnus-report-spam)
             ("u" . trev-gnus-mark-unread)
             ("t" . trev-gnus-tick)
             ("r" . trev-gnus-reply)
             ("R" . trev-gnus-reply-all)
             ("f" . trev-gnus-forward)
             ("m" . trev-gnus-compose)
             ("s" . trev-gnus-search-current-group)))
    (define-key gnus-summary-mode-map
                (kbd (car binding)) (cdr binding))))

(defun trev-gnus--setup-article-keys ()
  "Install personal article keys."
  (dolist (binding
           `(("?" . trev-gnus-dispatch)
             ("/" . trev-gnus-filter-transient)
             ("a" . trev-gnus-archive)
             ("d" . trev-gnus-trash)
             ("!" . trev-gnus-report-spam)
             ("u" . trev-gnus-mark-unread)
             ("t" . trev-gnus-tick)
             ("r" . trev-gnus-reply)
             ("R" . trev-gnus-reply-all)
             ("f" . trev-gnus-forward)
             ("m" . trev-gnus-compose)
             ("s" . trev-gnus-search-current-group)
             ("q" . kill-current-buffer)))
    (define-key gnus-article-mode-map
                (kbd (car binding)) (cdr binding))))

(defun trev-gnus--setup-message-keys ()
  "Install personal message keys."
  (define-key message-mode-map (kbd "C-c C-x") #'trev-gnus-message-security))

(defun trev-gnus--setup-windows ()
  "Install personal Gnus window layouts."
  (gnus-add-configuration
   '(article
     (vertical 1.0
               (summary 0.3 point)
               (article 1.0)))))

;;;###autoload
(defun trev-gnus-setup ()
  "Install personal Gnus commands, keys, and layout."
  (trev-gnus--setup-group-keys)
  (trev-gnus--setup-summary-keys)
  (trev-gnus--setup-article-keys)
  (trev-gnus--setup-windows)
  (trev-gnus--setup-message-keys))

(use-package gnus
  :demand t
  :custom
  (gnus-agent nil)
  (gnus-article-date-headers '(combined-lapsed))
  (gnus-article-skip-boring t)
  (gnus-check-new-newsgroups nil)
  (gnus-group-line-format "%M%S%p%P%B%(%G%) (%y)\n")
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-init-file nil)
  (gnus-logo-colors
   `(,(face-attribute 'font-lock-builtin-face :foreground)
     ,(face-attribute 'font-lock-keyword-face :foreground)))
  (gnus-message-archive-group nil)
  (gnus-secondary-select-methods
   '((nntp "news.gmane.io"
           (nntp-connection-timeout 5))))
  (gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)))
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  (gnus-summary-line-format "%U%R%z %([%&user-date;]  %-20,20f  %B%s%)\n")
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-hide-subtree t)
  (gnus-thread-ignore-subject t)
  (gnus-thread-indent-level 2)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-treat-fill-long-lines '(typep "text/plain"))
  (gnus-treat-hide-boring-headers 'head)
  (gnus-treat-strip-leading-blank-lines t)
  (gnus-treat-strip-multiple-blank-lines t)
  (gnus-treat-strip-trailing-blank-lines t)
  (gnus-use-dribble-file nil)
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %10R")
     ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday %6R")
     (t . "%Y-%m-%d %R")))
  (gnus-visible-headers
   '("^From:" "^To:" "^Cc:" "^Subject:" "^Date:" "^Newsgroups:"
     "^Followup-To:" "^Reply-To:" "^Mail-Followup-To:" "^List-Id:"
     "^Gnus-Warning:"))
  (message-send-mail-function 'smtpmail-send-it)
  (mm-html-blocked-images "\\`\\(https?\\|ftp\\)://")
  (mm-html-inhibit-images nil)
  (mm-inline-large-images 'resize)
  (mm-text-html-renderer 'shr)
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-secure-openpgp-signers '("A52D68794EBED758"))
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  :config
  (trev-gnus-setup))

(provide 'trev-gnus)
;;; trev-gnus.el ends here
