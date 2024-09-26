;;; -*- lexical-binding: t -*-

(use-package erc
  :preface
  (defun erc-connect ()
    (interactive)
    (erc :server "orangepi"
         :port 7777
         :user "trev"
         :password my/erc-password))
  (let ((hidden-fools t))
    (defun erc-toggle-fools ()
      (interactive)
      (setq hidden-fools (not hidden-fools))
      (erc-match-toggle-hidden-fools hidden-fools)
      (message "hidden fools: %s" (if hidden-fools "on" "off"))
      (set-buffer-modified-p t)))
  :config
  (load-library (expand-file-name "secrets.el.gpg" user-emacs-directory))
  (setq
   erc-server "orangepi"
   erc-port "7777"
   erc-nick "trev"
   erc-user-full-name "trev"
   erc-prompt 'erc-prompt-format
   erc-prompt-format (propertize "%n:" 'font-lock-face 'erc-input-face)
   erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART"
                             "353" "324" "332" "329" "333" "477")
   erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART" "353")
   erc-track-exclude-server-buffer t
   erc-pals my/erc-pals
   erc-fools my/erc-fools
   erc-fool-highlight-type 'all
   erc-current-nick-highlight-type 'nick-or-keyword
   erc-keywords '("linux" "rust")
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-timestamp-format "%H:%M"
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 16)
  (setopt erc-modules
          (seq-union '(nicks scrolltobottom spelling) erc-modules))
  (erc-spelling-mode)
  (erc-scrolltobottom-mode)
  (defun erc-match-directed-at-fool-p (msg) nil)
  :hook
  ((erc-mode . (lambda ()
                 (display-line-numbers-mode 0)
                 (auto-fill-mode -1)
                 (apheleia-mode -1)
                 (corfu-mode -1)
                 (setq-local scroll-margin 0)))
   (erc-text-matched . erc-hide-fools))
  :bind
  (("C-c e" . 'erc-connect)
   :map erc-mode-map
   ("C-c -" . 'erc-toggle-fools)))

(provide 'erc-init)
