;;; erc-znc.el --- ZNC bouncer integration for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: comm
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration helpers for using ERC with a ZNC bouncer.  Enable the
;; global minor mode `erc-znc-mode' to install everything:
;;
;;     (use-package erc-znc
;;       :ensure nil
;;       :hook (erc-mode . erc-znc-mode))
;;
;; It provides:
;;
;; - `erc-znc-connect' to attach to the bouncer.
;; - Correct handling of bouncer history replayed on connect: each
;;   replayed line is shown at its original IRCv3 `server-time', and
;;   already-seen replayed lines do not re-light `erc-track' (so a stale
;;   mention of your nick does not look like a fresh ping).  A per-target
;;   "watermark" of the latest seen timestamp is persisted via savehist.
;; - Capability negotiation for the ZNC/IRCv3 features this relies on.
;; - `*status'/`*playback' command helpers (see the `C-c z' prefix).
;; - Desktop notifications for genuine mentions and PMs that are
;;   suppressed during history playback.

;;; Code:

(require 'erc)
(require 'erc-networks)
(require 'seq)

(declare-function erc-networks--id-string "erc-networks")

(defvar erc--msg-props)
(defvar erc-networks--id)
(defvar savehist-additional-variables)

(defgroup erc-znc nil
  "Integration helpers for using ERC with a ZNC bouncer."
  :group 'erc
  :prefix "erc-znc-")

(defcustom erc-znc-server nil
  "Hostname of the ZNC bouncer to attach to.
Usually populated from an encrypted secrets file before connecting."
  :type '(choice (const :tag "Unset" nil) string))

;;;; IRCv3 server-time parsing.

(defun erc-znc--tag-value (tags name)
  "Return NAME from ERC message TAGS."
  (when-let* ((tag (seq-find (lambda (tag)
                               (equal (format "%s" (car-safe tag)) name))
                             tags))
              (value (cdr tag)))
    (if (listp value) (car value) value)))

(defun erc-znc--parsed ()
  "Return the inserted message's parsed ERC response."
  (when-let* ((pos (erc-find-parsed-property)))
    (erc-get-parsed-vector pos)))

(defun erc-znc--message-time ()
  "Return the inserted message's IRCv3 server-time value, or nil."
  (when-let* ((parsed (erc-znc--parsed))
              (stamp (erc-znc--tag-value (erc-response.tags parsed) "time")))
    (ignore-errors (date-to-time stamp))))

;;;; Per-target watermark of the latest seen server-time.

(defvar erc-znc--watermarks nil
  "Alist of latest seen server-time stamps keyed by (NETWORK TARGET).")

(defun erc-znc--target-key ()
  "Return the watermark key for the current ERC buffer."
  (let ((network (or (and (boundp 'erc-networks--id)
                          erc-networks--id
                          (erc-networks--id-string erc-networks--id))
                     (erc-with-server-buffer
                       (or erc-server-announced-name erc-session-server))
                     erc-session-server))
        (target (or (erc-default-target) (buffer-name))))
    (list network target)))

(defun erc-znc--watermark ()
  "Return the latest seen server-time for the current ERC buffer."
  (alist-get (erc-znc--target-key) erc-znc--watermarks nil nil #'equal))

(defun erc-znc--set-watermark (time)
  "Record TIME as seen for the current ERC buffer."
  (let* ((key (erc-znc--target-key))
         (watermark (alist-get key erc-znc--watermarks nil nil #'equal)))
    (when (or (null watermark) (time-less-p watermark time))
      (setf (alist-get key erc-znc--watermarks nil nil #'equal) time))))

;;;; Display: original timestamps + no false pings on replay.

(defun erc-znc--apply-server-time ()
  "Make ERC's displayed timestamp use the IRCv3 time tag.
Intended for `erc-insert-modify-hook'."
  (when-let* ((time (erc-znc--message-time))
              ((boundp 'erc--msg-props))
              ((hash-table-p erc--msg-props)))
    (puthash 'erc--ts time erc--msg-props)))

(defun erc-znc--track-modified-channels (orig &rest args)
  "Suppress `erc-track' for messages at or before the seen watermark.
Advises `erc-track-modified-channels'."
  (let* ((time (erc-znc--message-time))
         (watermark (and time (erc-znc--watermark))))
    (if (and watermark (not (time-less-p watermark time)))
        nil
      (prog1 (apply orig args)
        (when time
          (erc-znc--set-watermark time))))))

;;;; Connecting and *status helpers.

;;;###autoload
(defun erc-znc-connect ()
  "Attach to the ZNC bouncer named by `erc-znc-server'."
  (interactive)
  (require 'my-secrets)
  (unless (bound-and-true-p erc-znc-server)
    (user-error "`erc-znc-server' is not set"))
  (setq erc-server erc-znc-server)
  (erc-tls :server erc-server :port erc-port
           :user erc-nick :full-name erc-user-full-name))

;;;###autoload
(define-obsolete-function-alias 'erc-connect 'erc-znc-connect "1.0.0")

(defun erc-znc--clear-query-buffer ()
  "Ask ZNC to clear the buffer for a query when its buffer is killed.
Intended for `erc-kill-buffer-hook'."
  (when (erc-query-buffer-p)
    (erc-znc--status (format "clearbuffer %s" (erc-target)))))

;;;; Capability negotiation.

(defcustom erc-znc-capabilities '("server-time" "znc.in/self-message")
  "IRCv3/ZNC capabilities to request when connecting.
Each is sent as its own `CAP REQ' line, so a capability the server does
not support is rejected (NAK) on its own without affecting the others.

Only add capabilities ERC actually understands.  ERC has no handling for
e.g. `multi-prefix', `userhost-in-names', `extended-join', `away-notify',
`account-notify' or `chghost'; requesting those can corrupt nick lists or
surface raw, unhandled messages, so they are intentionally omitted."
  :type '(repeat string))

(defun erc-znc--request-capabilities ()
  "Request `erc-znc-capabilities' from the server during login.
Intended as `:before' advice on `erc-login'."
  (dolist (cap erc-znc-capabilities)
    (erc-server-send (format "CAP REQ :%s" cap)))
  (erc-server-send "CAP END"))

;;;; *status / *playback command helpers.

(defun erc-znc--status (line)
  "Send LINE to ZNC's *status module from the current server connection.
Uses `erc-server-send' (a raw PRIVMSG) rather than the input-line path,
which requires message-insertion state only bound during real input."
  (erc-server-send (format "PRIVMSG *status :%s" line)))

(defun erc-znc-jump ()
  "Tell ZNC to reconnect the current network to IRC (*status Jump)."
  (interactive)
  (erc-znc--status "Jump"))

(defun erc-znc-list-channels ()
  "Ask ZNC to list this network's channels (*status ListChans)."
  (interactive)
  (erc-znc--status "ListChans"))

(defun erc-znc-detach (channel)
  "Detach CHANNEL on ZNC so it stops appearing for this client.
Defaults to the current channel."
  (interactive (list (or (erc-default-target) (read-string "Channel: "))))
  (erc-znc--status (format "Detach %s" channel)))

(defun erc-znc-attach (channel)
  "Re-attach CHANNEL on ZNC."
  (interactive (list (read-string "Channel: " (erc-default-target))))
  (erc-znc--status (format "Attach %s" channel)))

(defun erc-znc-clear-buffer (target)
  "Clear ZNC's stored buffer for TARGET (*status ClearBuffer).
Defaults to the current target."
  (interactive (list (or (erc-default-target) (read-string "Target: "))))
  (erc-znc--status (format "ClearBuffer %s" target)))

(defvar-keymap erc-znc-prefix-map
  :doc "Keymap for ZNC bouncer commands, bound under \\`C-c z' in ERC buffers."
  "j" #'erc-znc-jump
  "l" #'erc-znc-list-channels
  "d" #'erc-znc-detach
  "a" #'erc-znc-attach
  "c" #'erc-znc-clear-buffer)

;;;; Notifications for genuine (non-replayed) mentions and PMs.

(defcustom erc-znc-notify t
  "When non-nil, desktop-notify for live mentions and private messages.
Notifications are suppressed for history replayed by the bouncer."
  :type 'boolean)

(defcustom erc-znc-notify-types '(mention query)
  "Kinds of live message that trigger a notification.
`mention' is a message naming your nick in a channel; `query' is any
private message."
  :type '(set (const mention) (const query)))

(defcustom erc-znc-notify-cooldown 30
  "Seconds to suppress repeat notifications from the same target.
After notifying for a mention or PM from a given channel/nick, further
notifications from that same target are withheld until this many seconds
have elapsed.  This keeps a burst of messages (someone spamming a query)
to a single notification rather than one per line.  Set to 0 to disable."
  :type 'natnum)

(defvar erc-znc--last-notify (make-hash-table :test 'equal)
  "Hash table mapping a target key to the time it last notified.
Used to enforce `erc-znc-notify-cooldown'.")

(defun erc-znc--live-message-p ()
  "Return non-nil when the inserted message is live, not replayed backlog.
A message is live when it has no server-time tag, or its server-time is
newer than the buffer's seen watermark."
  (let* ((time (erc-znc--message-time))
         (watermark (and time (erc-znc--watermark))))
    (or (null time) (null watermark) (time-less-p watermark time))))

(defun erc-znc--sender-nick ()
  "Return the nick that sent the inserted message, or nil."
  (when-let* ((parsed (erc-znc--parsed))
              (sender (erc-response.sender parsed)))
    (car (split-string sender "!"))))

(defun erc-znc--from-self-p ()
  "Return non-nil when the inserted message was sent by you."
  (equal (erc-znc--sender-nick) (erc-current-nick)))

(defun erc-znc--mention-p ()
  "Return non-nil when the inserted message names your nick."
  (when-let* ((nick (erc-current-nick)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (concat "\\_<" (regexp-quote nick) "\\_>") nil t))))

(defun erc-znc--notify (title body)
  "Show a desktop notification with TITLE and BODY, falling back to `message'."
  (if (and (require 'notifications nil t) (fboundp 'notifications-notify))
      (notifications-notify :title (format "ERC: %s" title)
                            :body (truncate-string-to-width (string-trim body) 200))
    (message "ERC: %s: %s" title body)))

(defun erc-znc--notify-title ()
  "Return the notification title: \"NICK in #chan\", or \"NICK\" for a query."
  (let ((nick (or (erc-znc--sender-nick) "?"))
        (target (erc-default-target)))
    (if (and target (erc-channel-p target))
        (format "%s in %s" nick target)
      nick)))

(defun erc-znc--notify-body (parsed)
  "Return the plain message text of PARSED for a notification body.
A CTCP ACTION (/me) is shown as \"* NICK text\" rather than raw control
characters."
  (let ((text (or (erc-response.contents parsed) "")))
    (if (string-match "\\`\C-aACTION \\(.*\\)\C-a\\'" text)
        (format "* %s %s" (or (erc-znc--sender-nick) "") (match-string 1 text))
      text)))

(defun erc-znc--notify-allowed-p ()
  "Return non-nil unless the current target is within its cooldown window.
When it returns non-nil it also records the current time as this target's
last notification, so the next call within `erc-znc-notify-cooldown'
seconds is suppressed."
  (or (zerop erc-znc-notify-cooldown)
      (let* ((key (erc-znc--target-key))
             (last (gethash key erc-znc--last-notify)))
        (when (or (null last)
                  (> (float-time (time-subtract nil last))
                     erc-znc-notify-cooldown))
          (puthash key (current-time) erc-znc--last-notify)
          t))))

(defun erc-znc--maybe-notify ()
  "Notify for a live mention or PM in the just-inserted message.
The title names the sender (and channel); the body is the message text
itself, not the formatted buffer line.  A per-target cooldown
(`erc-znc-notify-cooldown') collapses a burst from one source into a
single notification.  Intended for `erc-insert-modify-hook'."
  (when (and erc-znc-notify
             (erc-znc--live-message-p)
             (not (erc-znc--from-self-p))
             (or (and (memq 'query erc-znc-notify-types) (erc-query-buffer-p))
                 (and (memq 'mention erc-znc-notify-types) (erc-znc--mention-p)))
             (erc-znc--notify-allowed-p))
    (when-let* ((parsed (erc-znc--parsed)))
      (erc-znc--notify (erc-znc--notify-title)
                       (erc-znc--notify-body parsed)))))

;;;; The mode that wires it all together.

;;;###autoload
(define-minor-mode erc-znc-mode
  "Global minor mode integrating ERC with a ZNC bouncer."
  :global t
  :group 'erc-znc
  (if erc-znc-mode
      (progn
        (add-hook 'erc-insert-modify-hook #'erc-znc--apply-server-time -90)
        (add-hook 'erc-insert-modify-hook #'erc-znc--maybe-notify 90)
        (advice-add 'erc-login :before #'erc-znc--request-capabilities)
        (add-hook 'erc-kill-buffer-hook #'erc-znc--clear-query-buffer)
        (keymap-set erc-mode-map "C-c z" erc-znc-prefix-map)
        (with-eval-after-load 'erc-track
          (advice-add 'erc-track-modified-channels
                      :around #'erc-znc--track-modified-channels))
        (with-eval-after-load 'savehist
          (add-to-list 'savehist-additional-variables 'erc-znc--watermarks)))
    (remove-hook 'erc-insert-modify-hook #'erc-znc--apply-server-time)
    (remove-hook 'erc-insert-modify-hook #'erc-znc--maybe-notify)
    (advice-remove 'erc-login #'erc-znc--request-capabilities)
    (remove-hook 'erc-kill-buffer-hook #'erc-znc--clear-query-buffer)
    (when (keymap-lookup erc-mode-map "C-c z")
      (keymap-unset erc-mode-map "C-c z" t))
    (advice-remove 'erc-track-modified-channels
                   #'erc-znc--track-modified-channels)))

(provide 'erc-znc)
;;; erc-znc.el ends here
