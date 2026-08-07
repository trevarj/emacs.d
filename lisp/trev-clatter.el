;;; trev-clatter.el --- clatter config for the ZNC bouncer -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package clatter
  :load-path "~/Workspace/clatter.el"
  :ensure nil
  :commands (clatter clatter-status clatter-disconnect)
  :functions (cape-emoji clatter-chathistory-request clatter-dcc-setup clatter-setup)
  :defines (clatter-fools clatter-networks erc-fools erc-server)
  :bind
  (("C-c #" . trev/clatter-connect)
   ("C-c C-SPC" . clatter-track-switch)
   :map clatter-mode-map
   ("C-c h" . (lambda (count)
                "Request the latest messages, optionally limited to COUNT."
                (interactive "P")
                (clatter-chathistory-request
                 (and count (prefix-numeric-value count)))))
   ("C-c g" . clatter-track-clear-all)
   ("C-c n" . clatter-nicklist-toggle)
   ("C-c :" . clatter-track-list)
   ("C-c -" . clatter-toggle-fools))
  :custom
  (clatter-quit-on-exit nil)
  (clatter-track-count-style 'glyph)
  (clatter-track-in-buffer-mode-line t)
  (clatter-track-exclude-targets '("*server*"))
  (clatter-track-indicators
   '((mention . nil)
     (dm . "✉")
     (activity . nil)))
  (clatter-url-preview-enable t)
  (clatter-buffer-name-style 'channel)
  (clatter-compact-system-messages 'compact)
  (clatter-display-on-join nil)
  (clatter-display-on-welcome nil)
  (clatter-self-echo-mode 'optimistic)
  (clatter-message-order 'oldest-first)
  (clatter-nick-column-width 14)
  (clatter-timestamp-side 'left)
  (clatter-timestamp-only-if-changed t)
  (clatter-prompt-format "%n: ")
  (clatter-prompt-alignment 'right)
  (clatter-header-line-preset 'context)
  (clatter-chathistory-limit 100)
  :hook
  (clatter-mode . (lambda ()
                    (add-hook 'completion-at-point-functions #'cape-emoji nil t)))
  :config
  (require 'my-secrets)
  (require 'gnutls)
  (setopt clatter-networks
          `(("soju"
             :server ,erc-server
             :port 6697
             :tls t
             :bouncer t
             :nick "trev"
             :username "trev"
             :realname ,user-full-name))
          clatter-fools erc-fools)

  (require 'clatter-dcc)
  (clatter-dcc-setup)
  (clatter-setup))

;;;###autoload
(defun trev/clatter-connect ()
  "Connect Clatter to the Libera bouncer."
  (interactive)
  (clatter "soju"))

(provide 'trev-clatter)
;;; trev-clatter.el ends here
