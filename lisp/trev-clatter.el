;;; trev-clatter.el --- clatter config for the ZNC bouncer -*- lexical-binding: t; -*-
;;
;; Connects clatter.el to the same ZNC bouncer ERC uses (erc-server/erc-port,
;; from secrets.el.gpg via my-secrets), authenticated as :user "trev/libera".
;; The clatter package is installed from the local checkout via :vc.

;;; Commentary:
;;; Code:

(use-package clatter
  ;; Load the active branch directly from the local checkout.  This avoids a
  ;; stale package-quickstart entry pointing at a removed package-vc directory.
  :load-path "/home/trev/Workspace/clatter.el"
  :ensure nil
  :init
  (setq load-prefer-newer t)
  :commands (clatter clatter-status clatter-disconnect)
  :bind
  (("C-c C-SPC" . clatter-track-switch)
   :map clatter-mode-map
   ("C-c h"   . trev/clatter-chathistory-latest)
   ("C-c n"   . clatter-nicklist-toggle)
   ("C-c :"   . clatter-track-list)
   ("C-c -"   . clatter-toggle-fools))
  :preface
  (require 'auth-source)
  (declare-function clatter-connect "clatter-connection" (network-id &rest args))
  (declare-function clatter-chathistory-request "clatter-chathistory" (&optional count))
  (declare-function clatter-setup "clatter")
  (declare-function clatter-dcc-setup "clatter-dcc")
  (defvar clatter-networks)
  (defvar clatter-mode-map)
  (defvar clatter-default-realname)
  (defvar clatter-default-port)
  (defvar clatter-use-auth-source)
  (defvar clatter-fools)
  (defvar erc-server)
  (defvar erc-fools)

  (defun trev/clatter-connect ()
    "Connect clatter to the libera bouncer (same endpoint as ERC)."
    (interactive)
    (require 'clatter)
    (clatter-connect "libera"))

  (defun trev/clatter-chathistory-latest (count)
    "Request the latest COUNT messages for the current clatter target.
Without a prefix argument, request 100 messages (mirrors
`trev/erc-chathistory-latest')."
    (interactive "P")
    (require 'clatter)
    (clatter-chathistory-request
     (if count (prefix-numeric-value count) 100)))

  :custom
  ;; --- connection / bouncer behavior ---
  (clatter-tls-method 'builtin)
  (clatter-nick-reclaim-enabled nil)
  (clatter-quit-on-exit nil)
  ;; --- extras ---
  (clatter-track-count-style 'glyph)
  (clatter-track-in-buffer-mode-line t)
  (clatter-track-exclude-targets '("*server*"))
  (clatter-track-indicators
   '((mention . nil)
     (dm . "✉")
     (activity . nil)))
  (clatter-url-preview-enable t)
  (clatter-image-enable nil)
  (clatter-notify-echo-area nil)
  (clatter-smart-enabled t)
  (clatter-smart-noise '(join part quit nick away))
  ;; Keep IRCv3 read-marker sync off; local read-state persistence remains on.
  (clatter-read-marker-enabled nil)
  (clatter-read-state-enabled t)
  ;; --- display ---
  (clatter-compact-system-messages 'compact)
  (clatter-display-on-join nil)
  (clatter-display-on-welcome nil)
  (clatter-receive-query-display 'bury)
  (clatter-self-echo-mode 'optimistic)
  (clatter-message-order 'oldest-first)
  (clatter-nick-column-width 14)
  (clatter-timestamp-side 'left)
  (clatter-timestamp-only-if-changed t)
  (clatter-prompt-format "%n: ")
  (clatter-prompt-alignment 'right)
  (clatter-header-line-preset 'context)
  (clatter-mode-line-show-topic nil)
  (clatter-chathistory-limit 100)

  :hook
  (clatter-mode . (lambda ()
                    (add-to-list 'completion-at-point-functions 'cape-emoji)))
  :config
  (require 'my-secrets)
  (require 'gnutls)

  ;; Network entry reusing ERC's bouncer endpoint (erc-server, from secrets).
  ;; Computed values can't go in :custom (it quotes), so setopt here.  The
  ;; `:bouncer' flag suppresses NickServ IDENTIFY and nick reclaim while the
  ;; password still authenticates this client to ZNC.  Port is fixed at 6697 (TLS,
  ;; same as ERC's erc-port) to avoid pulling in ERC just to read its defcustom.
  (setopt clatter-networks
          `(("libera"
             :server ,erc-server
             :port 6697
             :tls t
             :bouncer t
             :nick "trev"
             :username "trev/libera"	  ; ZNC user/network, like ERC
             :realname ,user-full-name))) ; matches ERC erc-user-full-name
  (setopt clatter-default-realname user-full-name)

  ;; Reuse ERC's fool list.  NOTE: erc-fools is a list of regexps, while
  ;; clatter-fools expects nicks or (NICK . NETWORK) pairs that clatter runs
  ;; through `wildcard-to-regexp'.  Passing regexps through may not match the
  ;; way you expect - curate via /fool later if needed.
  (when (boundp 'erc-fools)
    (setq clatter-fools erc-fools))

  ;; clatter.el does not require these; pull them in for the features you opted into.
  (require 'clatter-dcc)
  (clatter-dcc-setup)	 ; adds the DCC CTCP hook (clatter-dcc.el:541)

  ;; Install cleanup hooks + enable the bundled extras per the flags above.
  (clatter-setup))

(provide 'trev-clatter)
;;; trev-clatter.el ends here
