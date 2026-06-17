;;; erc-znc-tests.el --- Tests for erc-znc  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: comm

;;; Commentary:

;; ERT tests for `erc-znc'.  Pure logic (server-time tag parsing,
;; watermark comparison) is tested directly; ERC-dependent behavior is
;; exercised with the relevant buffer-locals stubbed.

;;; Code:

(require 'ert)
(require 'erc-znc)

;;;; IRCv3 server-time tag parsing.

(ert-deftest erc-znc-test-tag-value-string ()
  "A tag whose value is a bare string is returned verbatim."
  (should (equal (erc-znc--tag-value '((time . "2026-06-17T08:00:00.000Z")
                                       (account . "trev"))
                                     "time")
                 "2026-06-17T08:00:00.000Z")))

(ert-deftest erc-znc-test-tag-value-listed ()
  "A tag whose value is a list returns the list's first element."
  (should (equal (erc-znc--tag-value '((time "2026-06-17T08:00:00.000Z"))
                                     "time")
                 "2026-06-17T08:00:00.000Z")))

(ert-deftest erc-znc-test-tag-value-missing ()
  "A missing tag returns nil."
  (should (null (erc-znc--tag-value '((account . "trev")) "time"))))

;;;; Watermark store.

(ert-deftest erc-znc-test-watermark-set-and-get ()
  "Setting then reading a watermark round-trips, keyed by network+target."
  (let ((erc-znc--watermarks nil)
        (t1 (date-to-time "2026-06-17T08:00:00.000Z")))
    (cl-letf (((symbol-function 'erc-znc--target-key)
               (lambda () '("Libera.Chat" "#emacs"))))
      (erc-znc--set-watermark t1)
      (should (equal (erc-znc--watermark) t1)))))

(ert-deftest erc-znc-test-watermark-monotonic ()
  "A watermark only advances; an older time never replaces a newer one."
  (let ((erc-znc--watermarks nil)
        (older (date-to-time "2026-06-17T08:00:00.000Z"))
        (newer (date-to-time "2026-06-17T09:00:00.000Z")))
    (cl-letf (((symbol-function 'erc-znc--target-key)
               (lambda () '("Libera.Chat" "#emacs"))))
      (erc-znc--set-watermark newer)
      (erc-znc--set-watermark older)
      (should (equal (erc-znc--watermark) newer)))))

(ert-deftest erc-znc-test-watermark-per-target ()
  "Watermarks are independent per (network target) key."
  (let ((erc-znc--watermarks nil)
        (key "#a") )
    (cl-letf (((symbol-function 'erc-znc--target-key)
               (lambda () (list "Libera.Chat" key))))
      (setq key "#a") (erc-znc--set-watermark (date-to-time "2026-06-17T08:00:00Z"))
      (setq key "#b") (erc-znc--set-watermark (date-to-time "2026-06-17T09:00:00Z"))
      (setq key "#a")
      (should (equal (erc-znc--watermark)
                     (date-to-time "2026-06-17T08:00:00Z"))))))

;;;; Capability negotiation.

(ert-deftest erc-znc-test-request-capabilities ()
  "Each capability is REQ'd on its own line, followed by CAP END."
  (let ((erc-znc-capabilities '("server-time" "znc.in/self-message"))
        sent)
    (cl-letf (((symbol-function 'erc-server-send)
               (lambda (line &rest _) (push line sent))))
      (erc-znc--request-capabilities)
      (should (equal (nreverse sent)
                     '("CAP REQ :server-time"
                       "CAP REQ :znc.in/self-message"
                       "CAP END"))))))

;;;; *status command helpers.

(ert-deftest erc-znc-test-status-commands ()
  "Commands send a raw PRIVMSG to *status (not via the input-line path)."
  (let (sent)
    (cl-letf (((symbol-function 'erc-server-send)
               (lambda (line &rest _) (push line sent))))
      (erc-znc-jump)
      (erc-znc-detach "#emacs")
      (erc-znc-clear-buffer "#emacs")
      (should (equal (reverse sent)
                     '("PRIVMSG *status :Jump"
                       "PRIVMSG *status :Detach #emacs"
                       "PRIVMSG *status :ClearBuffer #emacs"))))))

;;;; Notification gating.

(ert-deftest erc-znc-test-live-message-p ()
  "A message is live unless its server-time is at/under the watermark."
  (let ((newer (date-to-time "2026-06-17T09:00:00Z"))
        (older (date-to-time "2026-06-17T08:00:00Z")))
    (cl-letf (((symbol-function 'erc-znc--message-time) (lambda () nil))
              ((symbol-function 'erc-znc--watermark) (lambda () older)))
      (should (erc-znc--live-message-p)))     ; no time -> live
    (cl-letf (((symbol-function 'erc-znc--message-time) (lambda () newer))
              ((symbol-function 'erc-znc--watermark) (lambda () older)))
      (should (erc-znc--live-message-p)))     ; newer than watermark -> live
    (cl-letf (((symbol-function 'erc-znc--message-time) (lambda () older))
              ((symbol-function 'erc-znc--watermark) (lambda () newer)))
      (should-not (erc-znc--live-message-p))))) ; replayed backlog -> not live

(ert-deftest erc-znc-test-mention-p ()
  "Mention detection is word-bounded against your nick."
  (cl-letf (((symbol-function 'erc-current-nick) (lambda () "trev")))
    (with-temp-buffer (insert "hey trev, look") (should (erc-znc--mention-p)))
    (with-temp-buffer (insert "trevor was here") (should-not (erc-znc--mention-p)))))

(ert-deftest erc-znc-test-from-self-p ()
  "Self-detection compares the response sender's nick to your own."
  (cl-letf (((symbol-function 'erc-current-nick) (lambda () "trev")))
    (cl-letf (((symbol-function 'erc-znc--parsed)
               (lambda () (make-erc-response :sender "trev!u@h"))))
      (should (erc-znc--from-self-p)))
    (cl-letf (((symbol-function 'erc-znc--parsed)
               (lambda () (make-erc-response :sender "bob!u@h"))))
      (should-not (erc-znc--from-self-p)))))

(ert-deftest erc-znc-test-notify-format-channel ()
  "A channel mention notifies as \"NICK in #chan\" with the bare message."
  (let (captured)
    (cl-letf (((symbol-function 'erc-znc--notify)
               (lambda (title body) (setq captured (cons title body))))
              ((symbol-function 'erc-znc--live-message-p) (lambda () t))
              ((symbol-function 'erc-query-buffer-p) (lambda () nil))
              ((symbol-function 'erc-znc--mention-p) (lambda () t))
              ((symbol-function 'erc-current-nick) (lambda () "trev"))
              ((symbol-function 'erc-default-target) (lambda () "#emacs"))
              ((symbol-function 'erc-znc--parsed)
               (lambda () (make-erc-response :sender "alice!u@h"
                                             :contents "hey trev"))))
      (let ((erc-znc-notify t) (erc-znc-notify-types '(mention query))
            (erc-znc-notify-cooldown 0))
        (erc-znc--maybe-notify))
      (should (equal captured '("alice in #emacs" . "hey trev"))))))

(ert-deftest erc-znc-test-notify-format-query ()
  "A private message notifies as just the sender's nick."
  (let (captured)
    (cl-letf (((symbol-function 'erc-znc--notify)
               (lambda (title body) (setq captured (cons title body))))
              ((symbol-function 'erc-znc--live-message-p) (lambda () t))
              ((symbol-function 'erc-query-buffer-p) (lambda () t))
              ((symbol-function 'erc-current-nick) (lambda () "trev"))
              ((symbol-function 'erc-default-target) (lambda () "alice"))
              ((symbol-function 'erc-znc--parsed)
               (lambda () (make-erc-response :sender "alice!u@h"
                                             :contents "you around?"))))
      (let ((erc-znc-notify t) (erc-znc-notify-types '(mention query))
            (erc-znc-notify-cooldown 0))
        (erc-znc--maybe-notify))
      (should (equal captured '("alice" . "you around?"))))))

(ert-deftest erc-znc-test-notify-cooldown ()
  "A burst from one target notifies once, then is suppressed until cooldown."
  (let ((erc-znc--last-notify (make-hash-table :test 'equal))
        (erc-znc-notify-cooldown 30)
        (count 0))
    (cl-letf (((symbol-function 'erc-znc--notify)
               (lambda (_title _body) (cl-incf count)))
              ((symbol-function 'erc-znc--live-message-p) (lambda () t))
              ((symbol-function 'erc-znc--from-self-p) (lambda () nil))
              ((symbol-function 'erc-query-buffer-p) (lambda () t))
              ((symbol-function 'erc-znc--target-key)
               (lambda () '("Libera.Chat" "alice")))
              ((symbol-function 'erc-znc--parsed)
               (lambda () (make-erc-response :sender "alice!u@h"
                                             :contents "spam"))))
      (let ((erc-znc-notify t) (erc-znc-notify-types '(mention query)))
        (erc-znc--maybe-notify)
        (erc-znc--maybe-notify)
        (erc-znc--maybe-notify))
      (should (= count 1))
      ;; A stale last-notify (older than the cooldown) lets it fire again.
      (puthash '("Libera.Chat" "alice")
               (time-subtract nil 31) erc-znc--last-notify)
      (let ((erc-znc-notify t) (erc-znc-notify-types '(mention query)))
        (erc-znc--maybe-notify))
      (should (= count 2)))))

(ert-deftest erc-znc-test-notify-body-action ()
  "A CTCP ACTION body is rendered as \"* NICK text\", not raw control chars."
  (cl-letf (((symbol-function 'erc-znc--sender-nick) (lambda () "alice")))
    (should (equal (erc-znc--notify-body
                    (make-erc-response :sender "alice!u@h"
                                       :contents "\C-aACTION waves\C-a"))
                   "* alice waves"))))

;;;; Mode wiring.

(ert-deftest erc-znc-test-mode-toggle ()
  "Enabling installs the hooks/advice/keymap; disabling removes them."
  (unwind-protect
      (progn
        (erc-znc-mode 1)
        (should (memq #'erc-znc--apply-server-time erc-insert-modify-hook))
        (should (memq #'erc-znc--maybe-notify erc-insert-modify-hook))
        (should (advice-member-p #'erc-znc--request-capabilities 'erc-login))
        (should (keymap-lookup erc-mode-map "C-c z"))
        (erc-znc-mode -1)
        (should-not (memq #'erc-znc--apply-server-time erc-insert-modify-hook))
        (should-not (memq #'erc-znc--maybe-notify erc-insert-modify-hook))
        (should-not (advice-member-p #'erc-znc--request-capabilities 'erc-login))
        (should-not (keymap-lookup erc-mode-map "C-c z")))
    (erc-znc-mode -1)))

(provide 'erc-znc-tests)
;;; erc-znc-tests.el ends here
