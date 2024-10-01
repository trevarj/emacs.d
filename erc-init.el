;;; -*- lexical-binding: t -*-

(use-package erc
  :preface
  (defun erc-connect ()
    (interactive)
    (erc :server "orangepi"
         :port 7777
         :user "trev"
         :password my/erc-password))
  (defun erc-clear-query-buffer ()
    (when (erc-query-buffer-p)
      (erc-send-input-line "*status" (format "clearbuffer %s" (erc-target)))))
  (let ((hidden-fools t))
    (defun erc-toggle-fools ()
      (interactive)
      (setq hidden-fools (not hidden-fools))
      (erc-match-toggle-hidden-fools hidden-fools)
      (message "hidden fools: %s" (if hidden-fools "on" "off"))
      (set-buffer-modified-p t)))
  (defun get-doom-theme-color (name)
    (car (alist-get name doom-themes--colors)))
  :config
  (load-library (expand-file-name "secrets.el.gpg" user-emacs-directory))
  ;; Fix for restoring query buffers with self-messages
  (advice-add #'erc-login
              :before (lambda ()
                        (erc-server-send "CAP REQ :znc.in/self-message")
                        (erc-server-send "CAP END")))
  ;; Clear out query bufs when using using `AutoClearQueryBuffer = false`
  (add-to-list 'erc-kill-buffer-hook 'erc-clear-query-buffer)
  (setq erc-custom-nick-colors
        (mapcar 'get-doom-theme-color
                '(orange yellow teal blue dark-blue cyan violet)))
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
   erc-track-exclude '("#emacs" "#systemcrafters-live" "*status")
   erc-pals my/erc-pals
   erc-fools my/erc-fools
   erc-fool-highlight-type 'all
   erc-current-nick-highlight-type 'nick-or-keyword
   erc-keywords '("linux" "rust")
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-timestamp-format "%H:%M"
   erc-nicks-colors erc-custom-nick-colors
   erc-nicks--create-pool-function 'erc-nicks--create-culled-pool ; for above to work
   erc-receive-query-display 'bury
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 16)
  (custom-set-faces
   `(erc-current-nick-face ((t (:foreground
                                ,(get-doom-theme-color 'red)
                                :slant italic
                                :weight heavy))))
   `(erc-pal-face ((t (:foreground
                       ,(get-doom-theme-color 'green)
                       :weight heavy)))))
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
